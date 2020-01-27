use clap::{self, Clap};
use evdev_rs::enums::{int_to_ev_key, EventCode, EV_KEY::*};
use evdev_rs::*;
use futures::stream::Stream;
use inotify::{EventMask, Inotify, WatchMask};
use librespot::connect::spirc::{Spirc, SpircTask};
use librespot::core::authentication::Credentials;
use librespot::core::config::{ConnectConfig, DeviceType, SessionConfig};
use librespot::core::session::Session;
use librespot::playback::audio_backend;
use librespot::playback::config::{Bitrate, PlayerConfig};
use librespot::playback::mixer::{self, MixerConfig};
use librespot::playback::player::{Player, PlayerEvent};
use lifx_core::Message;
use lifx_core::RawMessage;
use lifx_core::HSBK;
use std::collections::HashSet;
use std::fs::{read_dir, File};
use std::io;
use std::iter::Iterator;
use std::net::SocketAddr;
use std::net::UdpSocket;
use std::path::PathBuf;
use std::process::Command;
use std::result::*;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::thread::sleep;
use std::time::Duration;

// futures stuff - only used by the librespot code
use futures::sync::mpsc::UnboundedReceiver;
use tokio_core::reactor::Core;

use KeyEventType::*;
use Mode::*;

/* TODO

maintainability
    once I actually understand the borrow checker
        tx, tx1...
        read_dev itself shouldnt be responsible for spawning the thread
        spotify_main should be able to go into it's own thread like everything else
        review all uses of 'clone', 'move', '&' etc.
        mode_transition closure
            avoids repeated code in printing old and new mode
            needs to change mutable variable, ideally without it being explicitly passed in
        have 'handle_cmd' actually take a Command
            when debugging, print the command text
    tooling to manage imports?
    better event names in lircd.conf
    cross-compile without docker
        ask on irc: https://gitter.im/librespot-org/spotify-connect-resources
stability
    spirc.shutdown() on program exit
performance
    ignore devices which can't perform key events
features
    train
    weather
    spotify
        search for song, artist etc. (require web API?)
        switch device (to, and maybe from, Pi)
*/

// command line arg data
#[derive(Clap, Debug)]
struct Opts {
    #[clap(short = "n", default_value = "Pi")]
    spotify_device_name: String,
    #[clap(short = "p")]
    spotify_password: String,
    #[clap(short = "e")]
    evdev_port: u16, // for receiving events over LAN
    #[clap(short = "d", long = "debug")]
    debug: bool, // print various extra data
}

// useful constants
const EVDEV_DIR: &str = "/dev/input/";

fn main() {
    // get data from command line args
    let opts: Opts = Opts::parse();

    // set up channel for keyboard events
    let (tx, rx) = channel();

    // read from existing devices
    for dir_entry in read_dir(&EVDEV_DIR).unwrap() {
        let path = dir_entry.unwrap().path();
        if !path.is_dir() {
            println!("Found device: {}", path.to_str().unwrap());
            read_dev(tx.clone(), path);
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
                        read_dev(tx1.clone(), full_path);
                    }
                }
            }
        }
    });

    // watch for network events
    let evdev_port = opts.evdev_port;
    thread::spawn(move || {
        //TODO security
        let sock = &UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], evdev_port))).unwrap();
        let mut buf = [0; 2];
        loop {
            match sock.recv_from(&mut buf) {
                Ok((_n_bytes, _addr)) => {
                    if let Some(k) = int_to_ev_key(buf[0] as u32) {
                        let c = EventCode::EV_KEY(k);
                        let t = TimeVal::new(0, 0);
                        let ev = InputEvent::new(&t, &c, buf[1] as i32);
                        tx.clone().send((ev, None)).unwrap();
                    } else {
                        println!(
                            "Int received over network is not a valid key code: {:?}",
                            buf[0]
                        )
                    }
                }
                Err(e) => println!("Received invalid network message: {:?} ({:?})", buf, e),
            }
        }
    });

    let mut core = Core::new().unwrap();
    let (spirc, spirc_task, evs) =
        spotify_setup(opts.spotify_device_name, opts.spotify_password, &mut core);

    thread::spawn(move || {
        evs.wait().for_each(|e| {
            println!("Spotify event: {:?}", e);
        });
    });

    let debug = opts.debug;
    thread::spawn(move || {
        respond_to_events(rx, spirc, debug);
    });

    // run spotify
    core.run(spirc_task).unwrap();
}

// create a new thread to read events from the device at p, and send them on s
fn read_dev(tx: Sender<(InputEvent, Option<String>)>, path: PathBuf) {
    let p = path.clone();
    thread::spawn(move || match File::open(path) {
        Ok(file) => match Device::new_from_fd(file) {
            Ok(dev) => loop {
                match dev.next_event(ReadFlag::NORMAL | ReadFlag::BLOCKING) {
                    Ok((_, event)) => tx
                        .send((event, dev.phys().map(|s| String::from(s))))
                        .unwrap(),
                    Err(e) => {
                        println!(
                            "Failed to get next event, abandoning device: {} ({:?})",
                            p.to_str().unwrap(),
                            e
                        );
                        break;
                    }
                }
            },
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
fn respond_to_events(rx: Receiver<(InputEvent, Option<String>)>, spirc: Spirc, debug: bool) {
    let lifx_sock = UdpSocket::bind("0.0.0.0:56700").unwrap();
    lifx_sock
        .set_read_timeout(Some(Duration::from_secs(3)))
        .unwrap();
    let lifx_target: SocketAddr = "192.168.1.188:56700".parse().unwrap();

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
    let mut ctrl = false;
    let mut _shift = false;
    let mut _alt = false;
    let mut mode = Normal;
    let mut ignored: HashSet<String> = HashSet::new(); // MAC addresses of devices currently being ignored

    loop {
        let (e, phys) = rx.recv().unwrap();
        if let EventCode::EV_KEY(k) = e.event_code {
            let ev_type = KeyEventType::from(e.value);
            if debug {
                println!("{:?},  {:?}", k, ev_type);
            }
            match (&k, ev_type) {
                // stuff that happens in all modes
                (KEY_LEFTCTRL, Pressed) => ctrl = true,
                (KEY_RIGHTCTRL, Pressed) => ctrl = true,
                (KEY_LEFTSHIFT, Pressed) => _shift = true,
                (KEY_RIGHTSHIFT, Pressed) => _shift = true,
                (KEY_LEFTALT, Pressed) => _alt = true,
                (KEY_LEFTCTRL, Released) => ctrl = false,
                (KEY_RIGHTCTRL, Released) => ctrl = false,
                (KEY_LEFTSHIFT, Released) => _shift = false,
                (KEY_RIGHTSHIFT, Released) => _shift = false,
                (KEY_LEFTALT, Released) => _alt = false,
                (KEY_RIGHTALT, Pressed) => {
                    if let Some(phys) = phys.clone() {
                        let phys0 = phys.clone();
                        let action = if ignored.contains(&phys) {
                            // regain control
                            ignored.remove(&phys);
                            "disable"
                        } else {
                            // pass control back to X
                            ignored.insert(phys);
                            "enable"
                        };
                        let phys = phys0.clone();
                        // enable, disable all devices with same MAC
                        for dir_entry in read_dir(&EVDEV_DIR).unwrap() {
                            let path = dir_entry.unwrap().path();
                            if !path.is_dir() {
                                let fd = File::open(path).unwrap();
                                if let Ok(dev) = Device::new_from_fd(fd) {
                                    if let Some(phys1) = dev.phys() {
                                        if phys1 == phys {
                                            let dev_name = dev.name().unwrap();
                                            let res = Command::new("xinput")
                                                .args(&[action, dev_name])
                                                .output();
                                            let cmd_name =
                                                String::from(action) + (" xinput device");
                                            handle_cmd(res, &cmd_name, dev_name, debug);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                _ => (),
            }
            let phys = phys.clone();
            if phys.map_or(true, |phys| !ignored.contains(&phys)) {
                match mode {
                    Normal => {
                        let stereo = |cmd: &str| ir_cmd("stereo", cmd, ev_type, debug);
                        match (&k, ev_type) {
                            (KEY_T, Pressed) => {
                                println!("Entering TV mode");
                                mode = TV;
                            }
                            (KEY_P, Pressed) => {
                                ir_cmd_once("stereo", "KEY_POWER", debug);
                                sleep(Duration::from_secs(1));
                                ir_cmd_once("stereo", "KEY_TAPE", debug);
                            }
                            (KEY_VOLUMEUP, _) => stereo("KEY_VOLUMEUP"),
                            (KEY_VOLUMEDOWN, _) => stereo("KEY_VOLUMEDOWN"),
                            (KEY_MUTE, _) => stereo("muting"),
                            (KEY_PLAYPAUSE, Pressed) => spirc.play_pause(),
                            (KEY_PREVIOUSSONG, Pressed) => spirc.prev(),
                            (KEY_NEXTSONG, Pressed) => spirc.next(),
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
                        let switcher = |cmd: &str| ir_cmd("switcher", cmd, ev_type, debug);
                        match &k {
                            KEY_T => {
                                if ev_type == Pressed {
                                    println!("Entering normal mode");
                                    mode = Normal
                                }
                            }
                            KEY_SPACE => {
                                tv("KEY_AUX");
                                sleep(Duration::from_millis(300));
                                tv("KEY_AUX");
                                sleep(Duration::from_millis(300));
                                tv("KEY_OK");
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

// create a Connect device through librespot
fn spotify_setup(
    name: String,
    password: String,
    core: &mut Core,
) -> (Spirc, SpircTask, UnboundedReceiver<PlayerEvent>) {
    let mixer_name: Option<&String> = None;
    let mixer_fn = mixer::find(mixer_name).expect("Invalid mixer");
    // let mixer = mixer::find(None: Option<&String>).expect("Invalid mixer"); //TODO when 'type ascription' reaches stable
    let mixer = mixer_fn(Some(MixerConfig::default()));

    let connect_config = ConnectConfig {
        name: name.clone(),
        device_type: DeviceType::default(),
        volume: u16::max_value(),
        linear_volume: false,
    };

    let session = core
        .run(Session::connect(
            SessionConfig {
                device_id: name.clone(),
                ..SessionConfig::default()
            },
            Credentials::with_password(String::from("georgefsthomas@gmail.com"), password),
            None,
            core.handle().clone(),
        ))
        .unwrap();
    println!("Spotify connection established");

    let backend_fn = audio_backend::find(None).expect("Invalid backend");
    let (player, evs) = Player::new(
        PlayerConfig {
            bitrate: Bitrate::Bitrate320,
            ..PlayerConfig::default()
        },
        session.clone(),
        mixer.get_audio_filter(),
        move || backend_fn(None),
    );

    let (spirc, spirc_task) = Spirc::new(connect_config, session.clone(), player, mixer);
    (spirc, spirc_task, evs)
}

// program mode
#[derive(Debug)]
enum Mode {
    Normal,
    TV,
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
