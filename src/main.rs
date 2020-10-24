use clap::{self, Clap};
use evdev_rs::enums::{EventCode, EventType, EV_KEY::*};
use evdev_rs::*;
use gpio::{sysfs::SysFsGpioOutput, GpioOut};
use inotify::{EventMask, Inotify, WatchMask};
use lifx_core::Message;
use lifx_core::RawMessage;
use lifx_core::HSBK;
use std::iter::Iterator;
use std::net::*;
use std::path::PathBuf;
use std::process::Command;
use std::result::*;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::thread::sleep;
use std::time::Duration;
use std::{collections::HashMap, io};
use std::{
    collections::HashSet,
    fs::{read_dir, File},
};
use strum::*; // should be strum::EnumIter, but https://github.com/rust-analyzer/rust-analyzer/issues/6053

use KeyEventType::*;
use Mode::*;

/* TODO

maintainability
    once I actually understand the borrow checker
        tx, tx1...
        read_dev itself shouldnt be responsible for spawning the thread
        review all uses of 'clone', 'move', '&' etc.
        have 'handle_cmd' actually take a Command
            when debugging, print the command text
    better event names in lircd.conf
stability
    more asnycness
        e.g. mpris play/pause hangs for ~10s when spotify hasnt been active for a while
            this causes other commands to be queued rather than just firing
correctness
    xinput enable on kill
performance
features
    train
    weather
    spotify (web API) - https://crates.io/crates/rspotify looks well-maintained and documented
        search for song, artist etc.
        switch device (to, and maybe from, Pi)
*/

// command line arg data
#[derive(Clap, Debug)]
struct Opts {
    #[clap(short = "d", long = "debug")]
    debug: bool, // print various extra data
}

// useful constants
const EVDEV_DIR: &str = "/dev/input/";
const LIFX_IP: IpAddr = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 187)); //TODO scan for this at startup
const LIFX_PORT: u16 = 56700;
const KEY_SEND_IP: IpAddr = IpAddr::V4(Ipv4Addr::new(192, 168, 1, 236)); //TODO set from CLI
const KEY_SEND_PORT: u16 = 56702; // TODO ditto

fn main() {
    // get data from command line args
    let opts: Opts = Opts::parse();
    let debug = opts.debug;

    // set up channel for keyboard events
    let (tx, rx) = channel();

    // read from existing devices
    for dir_entry in read_dir(&EVDEV_DIR).unwrap() {
        let path = dir_entry.unwrap().path();
        if !path.is_dir() {
            println!("Found device: {}", path.to_str().unwrap());
            read_dev(tx.clone(), path, debug);
        }
    }

    // watch for new devices
    let tx1 = tx.clone();
    thread::spawn(move || {
        let mut inotify = Inotify::init().unwrap();
        inotify.add_watch(&EVDEV_DIR, WatchMask::CREATE).unwrap();
        let mut buffer = [0u8; 4096];
        loop {
            let events = inotify.read_events_blocking(&mut buffer).unwrap();
            for event in events {
                if !event.mask.contains(EventMask::ISDIR) {
                    if let Some(name) = event.name {
                        let path = name.to_str().unwrap();
                        println!("Found new device: {}", path);
                        //TODO cf. my Haskell lib for more principled solution
                        sleep(Duration::from_millis(300)); // sleep to avoid permission error
                        let full_path = PathBuf::from(EVDEV_DIR).join(path);
                        read_dev(tx1.clone(), full_path, debug);
                    }
                }
            }
        }
    });

    respond_to_events(rx, debug);
}

// create a new thread to read events from the device at p, and send them on s
// if the device doesn't have key events (eg. a mouse), it is ignored
fn read_dev(tx: Sender<InputEvent>, path: PathBuf, debug: bool) {
    let p = path.clone();
    thread::spawn(move || match File::open(path) {
        Ok(file) => match Device::new_from_fd(file) {
            Ok(dev) => {
                if dev.has_event_type(&EventType::EV_KEY) {
                    //TODO we shouldn't disable if we are currently in 'Idle' mode
                    //TODO this reruns for all devices, which is a bit pointless
                    //TODO resolve ambiguity (see error messages when Logitech wireless mouse is connected)
                    xinput(XInput::Disable, debug);
                    loop {
                        match dev.next_event(ReadFlag::NORMAL | ReadFlag::BLOCKING) {
                            Ok((_, event)) => tx.send(event).unwrap(),
                            Err(e) => {
                                println!(
                                    "Failed to get next event, abandoning device: {} ({:?})",
                                    p.to_str().unwrap(),
                                    e
                                );
                                break;
                            }
                        }
                    }
                }
            }
            Err(e) => {
                println!("Not an evdev device: {} ({:?})", p.to_str().unwrap(), e);
            }
        },
        Err(e) => println!("Couldn't open file: {} ({:?})", p.to_str().unwrap(), e),
    });
}

// print errors (and, if debug, output) to stdout
// takes a description of the command, and extra info (eg. device name)
fn handle_cmd(res: io::Result<std::process::Output>, cmd_name: &str, extra: &str, debug: bool) {
    match res {
        Ok(out) => {
            let err = out.stderr;
            if !err.is_empty() {
                print!(
                    "Failed to {}: {}\n{}",
                    cmd_name,
                    extra,
                    String::from_utf8(err).unwrap()
                )
            }
            if debug {
                //TODO doesn't seem to show anything when calling xinput
                print!("{}", String::from_utf8(out.stdout).unwrap());
            }
        }
        Err(e) => println!("Failed to {}: {} ({:?})", cmd_name, extra, e),
    }
}

// send off commands to a device, based on 'lircd.conf'
fn ir_cmd(dev: &str, cmd: &str, event_type: KeyEventType, debug: bool) {
    let send_type = match event_type {
        Pressed => "SEND_START",
        Released => "SEND_STOP",
        Repeated => {
            return (); // do nothing
        }
    };
    let res = Command::new("irsend").args(&[send_type, dev, cmd]).output();
    handle_cmd(res, "send IR command", cmd, debug);
}
fn ir_cmd_once(dev: &str, cmd: &str, debug: bool) {
    let res = Command::new("irsend")
        .args(&["SEND_ONCE", dev, cmd])
        .output();
    handle_cmd(res, "send IR command", cmd, debug);
}

// LIFX helpers, until there's a complete high-level LAN API in Rust
fn lifx_send(sock: &UdpSocket, target: SocketAddr, msg: Message) -> Result<(), io::Error> {
    let raw = RawMessage::build(&Default::default(), msg).unwrap();
    sock.send_to(&raw.pack().unwrap(), &target).map(|_| ())
}
fn set_hsbk(sock: &UdpSocket, target: SocketAddr, hsbk: HSBK) {
    let msg = Message::LightSetColor {
        color: hsbk,
        duration: 0,
        reserved: 0,
    };
    lifx_send(sock, target, msg).unwrap_or_else(|e| println!("Failed to set HSBK. ({:?})", e))
}
fn get_hsbk(sock: &UdpSocket, target: SocketAddr) -> Result<HSBK, lifx_core::Error> {
    let mut buf = [0; 88];
    lifx_send(sock, target, Message::LightGet)?;
    let (_n_bytes, _addr) = sock.recv_from(&mut buf)?;
    let raw = RawMessage::unpack(&buf)?;
    let msg = Message::from_raw(&raw)?;
    if let Message::LightState { color: hsbk, .. } = msg {
        Ok(hsbk)
    } else {
        Err(lifx_core::Error::ProtocolError(String::from(
            "failed to decode light response",
        )))
    }
}

// read from 'rx', responding to events
fn respond_to_events(rx: Receiver<InputEvent>, debug: bool) {
    // set up evdev-share
    let mut key_send_buf = [0; 2];
    let key_send_addr = SocketAddr::new(KEY_SEND_IP, KEY_SEND_PORT);
    let key_send_sock = &UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], 0))).unwrap();

    // set up LIFX
    let lifx_sock = UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], LIFX_PORT))).unwrap();
    lifx_sock
        .set_read_timeout(Some(Duration::from_secs(3)))
        .unwrap();
    let lifx_target: SocketAddr = SocketAddr::new(LIFX_IP, LIFX_PORT);

    // initialise state
    let mut hsbk = get_hsbk(&lifx_sock, lifx_target).unwrap_or_else(|e| {
        println!(
            "Failed to get HSBK from light - initialising all fields to 0. ({:?})",
            e
        );
        HSBK {
            brightness: 0,
            hue: 0,
            kelvin: 0,
            saturation: 0,
        }
    });
    let mut mode = Normal;
    let mut prev_mode = Normal; // the mode we were in before the current one
    let mut held = HashSet::new(); // keys which have been pressed since they were last released
    let mut last_key = KEY_RESERVED; // will be updated before it's ever read

    // set up GPIO stuff
    let mut led_map = HashMap::new();
    for mode in Mode::iter() {
        if let Some(led) = mode.led() {
            led_map.insert(mode, gpio::sysfs::SysFsGpioOutput::open(led).unwrap());
        }
    }
    let mut set_led = |mode: Mode, x: bool| {
        if let Some(led) = led_map.get_mut(&mode) {
            led.set_value(x)
                .unwrap_or_else(|e| println!("Failed to set GPIO: {}", e))
        }
    };
    set_led(mode, true);

    loop {
        let e = rx.recv().unwrap();
        if let EventCode::EV_KEY(k) = e.event_code {
            let ev_type = KeyEventType::from(e.value);

            // update state
            match ev_type {
                Pressed => {
                    held.insert(k.clone());
                    last_key = k.clone();
                }
                Released => {
                    held.remove(&k);
                }
                Repeated => {}
            }
            let ctrl = held.contains(&KEY_LEFTCTRL) || held.contains(&KEY_RIGHTCTRL);
            let _shift = held.contains(&KEY_LEFTSHIFT) || held.contains(&KEY_RIGHTSHIFT);
            let _alt = held.contains(&KEY_LEFTALT); // RIGHT_ALT is reserved for switching modes

            if debug {
                println!("{:?},  {:?}", k, ev_type);
            }

            // right-alt released - switch mode
            // we only do this when nothing is held, to avoid confusing X, virtual keyboards etc.
            if k == KEY_RIGHTALT && ev_type == Released && held.is_empty() {
                let new_mode = match last_key {
                    KEY_ESC => Some(Idle),
                    KEY_SPACE => Some(Normal),
                    KEY_T => Some(TV),
                    KEY_COMMA => Some(Sending),
                    KEY_RIGHTALT => Some(prev_mode), // this is a bit special - nothing else was pressed
                    _ => {
                        println!("Key does not correspond to a mode: {:?}", last_key);
                        None
                    }
                };
                if let Some(new_mode) = new_mode {
                    prev_mode = mode;
                    mode = new_mode;
                    if prev_mode == Idle {
                        xinput(XInput::Disable, debug)
                    };
                    if new_mode == Idle {
                        xinput(XInput::Enable, debug)
                    };
                    println!("Entering mode: {:?}", new_mode);
                    set_led(prev_mode, false);
                    set_led(new_mode, true);
                }
            } else if held.contains(&KEY_RIGHTALT) {
                // just wait for everything to be released
            } else {
                match mode {
                    Idle => (),
                    Sending => {
                        if debug {
                            println!("Sending: {:?}, {:?}", k, ev_type);
                        }
                        key_send_buf[0] = k as u8;
                        key_send_buf[1] = ev_type as u8;
                        key_send_sock
                            .send_to(&mut key_send_buf, key_send_addr)
                            .unwrap_or_else(|e| {
                                println!("Failed to send: {}", e);
                                0
                            });
                    }
                    Normal => {
                        let stereo = |cmd: &str| ir_cmd("stereo", cmd, ev_type, debug);
                        let stereo_once = |cmd: &str| ir_cmd_once("stereo", cmd, debug);
                        match (&k, ev_type) {
                            (KEY_P, Pressed) => {
                                stereo_once("KEY_POWER");
                                sleep(Duration::from_secs(1));
                                stereo_once("KEY_TAPE");
                            }
                            (KEY_VOLUMEUP, _) => stereo("KEY_VOLUMEUP"),
                            (KEY_VOLUMEDOWN, _) => stereo("KEY_VOLUMEDOWN"),
                            (KEY_MUTE, _) => stereo("muting"),
                            (KEY_PLAYPAUSE, Pressed) => mpris("PlayPause", debug),
                            (KEY_PREVIOUSSONG, Pressed) => mpris("Previous", debug),
                            (KEY_NEXTSONG, Pressed) => mpris("Next", debug),
                            (KEY_LEFT, _) => {
                                //TODO don't trigger on 'Released' (wait for 'or patterns'?)
                                hsbk = HSBK {
                                    hue: if ctrl {
                                        hsbk.hue.wrapping_sub(4096)
                                    } else {
                                        hsbk.hue.wrapping_sub(256)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_RIGHT, _) => {
                                hsbk = HSBK {
                                    hue: if ctrl {
                                        hsbk.hue.wrapping_add(4096)
                                    } else {
                                        hsbk.hue.wrapping_add(256)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_EQUAL, _) => {
                                hsbk = HSBK {
                                    saturation: if ctrl {
                                        65535
                                    } else {
                                        hsbk.saturation.checked_add(1024).unwrap_or(65535)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_MINUS, _) => {
                                hsbk = HSBK {
                                    saturation: if ctrl {
                                        0
                                    } else {
                                        hsbk.saturation.checked_sub(1024).unwrap_or(0)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_UP, _) => {
                                hsbk = HSBK {
                                    brightness: if ctrl {
                                        65535
                                    } else {
                                        hsbk.brightness.checked_add(1024).unwrap_or(65535)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_DOWN, _) => {
                                hsbk = HSBK {
                                    brightness: if ctrl {
                                        0
                                    } else {
                                        hsbk.brightness.checked_sub(1024).unwrap_or(0)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_LEFTBRACE, _) => {
                                hsbk = HSBK {
                                    kelvin: if ctrl {
                                        9000
                                    } else {
                                        std::cmp::min(hsbk.kelvin + 65, 9000)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            (KEY_RIGHTBRACE, _) => {
                                hsbk = HSBK {
                                    kelvin: if ctrl {
                                        2500
                                    } else {
                                        std::cmp::max(hsbk.kelvin - 65, 2500)
                                    },
                                    ..hsbk
                                };
                                set_hsbk(&lifx_sock, lifx_target, hsbk);
                            }
                            _ => (),
                        }
                    }
                    TV => {
                        let tv = |cmd: &str| ir_cmd("tv", cmd, ev_type, debug);
                        let tv_once = |cmd: &str| ir_cmd_once("tv", cmd, debug);
                        let switcher = |cmd: &str| ir_cmd("switcher", cmd, ev_type, debug);
                        match &k {
                            KEY_SPACE => {
                                if ev_type == Pressed {
                                    tv_once("KEY_AUX");
                                    sleep(Duration::from_millis(300));
                                    tv_once("KEY_AUX");
                                    sleep(Duration::from_millis(300));
                                    tv_once("KEY_OK");
                                }
                            }
                            KEY_P => tv("KEY_POWER"),
                            KEY_1 => {
                                if ctrl {
                                    switcher("KEY_1")
                                } else {
                                    tv("KEY_1")
                                };
                            }
                            KEY_2 => {
                                if ctrl {
                                    switcher("KEY_2")
                                } else {
                                    tv("KEY_2")
                                };
                            }
                            KEY_3 => {
                                if ctrl {
                                    switcher("KEY_3")
                                } else {
                                    tv("KEY_3")
                                };
                            }
                            KEY_4 => tv("KEY_4"),
                            KEY_5 => tv("KEY_5"),
                            KEY_6 => tv("KEY_6"),
                            KEY_7 => tv("KEY_7"),
                            KEY_8 => tv("KEY_8"),
                            KEY_9 => tv("KEY_9"),
                            KEY_0 => tv("KEY_0"),
                            KEY_VOLUMEUP => tv("KEY_VOLUMEUP"),
                            KEY_VOLUMEDOWN => tv("KEY_VOLUMEDOWN"),
                            KEY_MUTE => tv("KEY_MUTE"),
                            KEY_COMMA => tv("KEY_CHANNELDOWN"),
                            KEY_DOT => tv("KEY_CHANNELUP"),
                            KEY_A => tv("KEY_AUX"),
                            KEY_S => tv("KEY_SETUP"),
                            KEY_G => tv("KEY_G"),
                            KEY_Q => tv("KEY_MENU"),
                            KEY_UP => tv("KEY_UP"),
                            KEY_DOWN => tv("KEY_DOWN"),
                            KEY_LEFT => tv("KEY_LEFT"),
                            KEY_RIGHT => tv("KEY_RIGHT"),
                            KEY_ENTER => tv("KEY_OK"),
                            KEY_BACKSPACE => tv("KEY_BACK"),
                            KEY_I => tv("KEY_INFO"),
                            KEY_ESC => tv("KEY_EXIT"),
                            _ => (),
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
enum XInput {
    Enable,
    Disable,
}
// enable, disable all keyboards (devices with EV_KEY events)
//TODO this is noticeably slow, there's probably a better way ("xinput list --name-only"? (how to filter?))
fn xinput(action: XInput, debug: bool) {
    for dir_entry in read_dir(&EVDEV_DIR).unwrap() {
        let path = dir_entry.unwrap().path();
        if !path.is_dir() {
            let fd = File::open(path).unwrap();
            if let Ok(dev) = Device::new_from_fd(fd) {
                if dev.has_event_type(&EventType::EV_KEY) {
                    let dev_name = dev.name().unwrap();
                    let action_str = match action {
                        XInput::Enable => "enable",
                        XInput::Disable => "disable",
                    };
                    let res = Command::new("xinput")
                        .args(&[action_str, dev_name])
                        .output();
                    let cmd_name = String::from(action_str) + (" xinput device");
                    handle_cmd(res, &cmd_name, dev_name, debug);
                }
            }
        }
    }
}

fn mpris(cmd: &str, debug: bool) {
    let res = Command::new("dbus-send")
        .args(&[
            "--print-reply",
            "--dest=org.mpris.MediaPlayer2.spotifyd",
            "/org/mpris/MediaPlayer2",
            &(String::from("org.mpris.MediaPlayer2.Player.") + cmd),
        ])
        .output();
    handle_cmd(res, "perform mpris command", cmd, debug);
}

// program mode
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash, EnumIter)]
enum Mode {
    Idle,    // let the OS handle keypresses
    Sending, // send keypresses over UDP
    Normal,
    TV,
}
impl Mode {
    fn led(self) -> Option<u16> {
        match self {
            Idle => Some(6),    // yellow
            Sending => Some(5), // green
            Normal => Some(12), // blue
            TV => Some(13),     // red
        }
    }
}

//TODO suggest this be used in evdev library
#[derive(Clone, Copy, Debug, PartialEq)]
enum KeyEventType {
    Released,
    Pressed,
    Repeated,
}
impl KeyEventType {
    fn from(i: i32) -> KeyEventType {
        match i {
            0 => Released,
            1 => Pressed,
            2 => Repeated,
            n => panic!("Invalid key event type: {}", n),
        }
    }
}
