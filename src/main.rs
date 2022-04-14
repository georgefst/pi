use clap::Clap;
use evdev_rs::enums::{EventCode, EventType, EV_KEY::*};
use evdev_rs::*;
use get_if_addrs::{get_if_addrs, IfAddr, Ifv4Addr};
use gpio::{sysfs::SysFsGpioOutput, GpioOut};
use inotify::{EventMask, Inotify, WatchMask};
use lifx_core::{LifxString, Message, PowerLevel, RawMessage, HSBK};
use std::iter::Iterator;
use std::net::*;
use std::path::PathBuf;
use std::process::Command;
use std::result::*;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
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
        tx, tx1, mode, mode1...
        read_dev itself shouldnt be responsible for spawning the thread
        review all uses of 'clone', 'move', '&' etc.
        have 'handle_cmd' actually take a Command
            when debugging, print the command text
    factor out red LED warnings: `let mut unknown_key = || set_led(LED::Red, true);`
        why does `set_led` closure need to be mutable in the first place?
        it will then become easier to add more features
            e.g. logging the issue to console
            or even use it for errors other than unexpected keys
    better event names in lircd.conf
stability
    more asnycness
        e.g. mpris play/pause hangs for ~10s when spotify hasnt been active for a while
            this causes other commands to be queued rather than just firing
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
    #[clap(short = 'd', long = "debug")]
    debug: bool, // print various extra data
    #[clap(long = "no-gpio")]
    no_gpio: bool, // for when LEDs aren't plugged in
    #[clap(long = "ip")]
    key_send_ips: Vec<IpAddr>,
    #[clap(long = "port")]
    key_send_port: u16,
}

// useful constants
const EVDEV_DIR: &str = "/dev/input/";
const LIFX_PORT: u16 = 56700;
const LIFX_TIMEOUT: Duration = Duration::from_secs(4);
const LIFX_FLASH_TIME: Duration = Duration::from_millis(350);
const RETRY_PAUSE_MS: u64 = 100;
const RETRY_MAX: i32 = 30;

fn main() {
    // get data from command line args
    let opts: Opts = Opts::parse();
    let debug = opts.debug;

    // set up channel for keyboard events
    let (tx, rx) = channel();

    let mode = Arc::new(Mutex::new(Idle));
    let mode1 = Arc::clone(&mode);

    // read from existing devices
    for dir_entry in read_dir(&EVDEV_DIR).unwrap() {
        let path = dir_entry.unwrap().path();
        if !path.is_dir() {
            println!("Found device: {}", path.to_str().unwrap());
            read_dev(Arc::clone(&mode), tx.clone(), path, debug);
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
                        let full_path = PathBuf::from(EVDEV_DIR).join(path);
                        read_dev(Arc::clone(&mode), tx1.clone(), full_path, debug);
                    }
                }
            }
        }
    });

    respond_to_events(mode1, rx, opts);
}

// create a new thread to read events from the device at p, and send them on s
// if the device doesn't have key events (eg. a mouse), it is ignored
fn read_dev(mode: Arc<Mutex<Mode>>, tx: Sender<InputEvent>, path: PathBuf, debug: bool) {
    thread::spawn(move || {
        // keep retrying on permission error - else we get failures on startup and with new devices
        //TODO cf. my Haskell evdev lib for more principled solution
        let mut attempts = 0;
        let file = loop {
            match File::open(path.clone()) {
                Ok(f) => break f,
                Err(e) => match e.kind() {
                    io::ErrorKind::PermissionDenied => {
                        if attempts >= RETRY_MAX {
                            println!(
                                "Giving up on new device - too many attempts: {}, {}",
                                path.to_str().unwrap(),
                                RETRY_MAX
                            );
                            return ();
                        } else {
                            attempts += 1;
                            sleep(Duration::from_millis(RETRY_PAUSE_MS));
                        }
                    }
                    _ => {
                        return {
                            println!("Couldn't open device: {} ({:?})", path.to_str().unwrap(), e);
                        }
                    }
                },
            }
        };
        match Device::new_from_fd(file) {
            Ok(dev) => {
                if dev.has_event_type(&EventType::EV_KEY) {
                    let mode = *mode.lock().unwrap();
                    if debug {
                        println!("New device, current mode {:?}", mode)
                    }
                    if mode != Idle {
                        //TODO run just for this device - difficult with my bluetooth keyboard since it appears as multiple?
                        xinput(XInput::Disable, debug);
                    }
                    loop {
                        match dev.next_event(ReadFlag::NORMAL | ReadFlag::BLOCKING) {
                            Ok((_, event)) => tx.send(event).unwrap(),
                            Err(e) => {
                                println!(
                                    "Failed to get next event, abandoning device: {} ({:?})",
                                    path.to_str().unwrap(),
                                    e
                                );
                                break;
                            }
                        }
                    }
                }
            }
            Err(e) => {
                println!(
                    "Failed to create evdev device: {}: {}",
                    path.to_str().unwrap(),
                    e
                );
            }
        }
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
fn set_lifx_power(sock: &UdpSocket, target: SocketAddr, level: PowerLevel) {
    let msg = Message::SetPower { level };
    lifx_send(sock, target, msg).unwrap_or_else(|e| println!("Failed to set LIFX power. ({:?})", e))
}
fn set_hsbk_delayed(sock: &UdpSocket, target: SocketAddr, hsbk: HSBK, dur: Duration) {
    let msg = Message::LightSetColor {
        color: hsbk,
        duration: dur.as_millis() as u32,
        reserved: 0,
    };
    lifx_send(sock, target, msg).unwrap_or_else(|e| println!("Failed to set HSBK. ({:?})", e))
}
fn set_hsbk(sock: &UdpSocket, target: SocketAddr, hsbk: HSBK) {
    set_hsbk_delayed(sock, target, hsbk, Duration::from_secs(0))
}
fn get_lifx_state(
    sock: &UdpSocket,
    target: SocketAddr,
) -> Result<(LifxString, PowerLevel, HSBK), lifx_core::Error> {
    let mut buf = [0; 88];
    lifx_send(sock, target, Message::LightGet)?;
    sock.set_read_timeout(Some(LIFX_TIMEOUT)).unwrap();
    loop {
        let (_n_bytes, _addr) = sock.recv_from(&mut buf)?;
        let raw = RawMessage::unpack(&buf)?;
        let msg = Message::from_raw(&raw)?;
        if let Message::LightState {
            label,
            power,
            color,
            ..
        } = msg
        {
            return Ok((label, power, color));
        } else {
            println!("Unexpected LIFX message: {:?}", msg);
        }
    }
}
// adapted from 'https://github.com/eminence/lifx/blob/master/utils/get_all_info/src/main.rs'
fn get_lifx_addresses() -> HashSet<SocketAddr> {
    fn get_ip() -> IpAddr {
        for iface in get_if_addrs().unwrap() {
            match iface.addr {
                IfAddr::V4(Ifv4Addr {
                    broadcast: Some(bcast),
                    ..
                }) => {
                    if iface.ip().is_loopback() {
                        continue;
                    }
                    return IpAddr::V4(bcast);
                }
                _ => (),
            }
        }
        panic!("Couldn't get ip")
    }

    let sock = UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], LIFX_PORT))).unwrap();
    sock.set_broadcast(true).unwrap();
    let addr = SocketAddr::new(get_ip(), LIFX_PORT);
    lifx_send(&sock, addr, Message::GetService).unwrap();

    sock.set_read_timeout(Some(LIFX_TIMEOUT)).unwrap();
    let mut devs = HashSet::new();
    let mut buf = [0; 1024];
    loop {
        match sock.recv_from(&mut buf) {
            Ok((n_bytes, addr)) => {
                let raw = RawMessage::unpack(&buf[0..n_bytes]).unwrap();
                match Message::from_raw(&raw) {
                    Ok(m) => match m {
                        Message::StateService {
                            port: _,
                            service: _,
                        } => {
                            println!("Found LIFX device: {}", addr);
                            devs.insert(addr);
                        }
                        _ => println!("Unexpected LIFX message: {:?}", m),
                    },
                    Err(e) => println!("Couldn't parse LIFX message: {:?}", e),
                }
            }
            Err(e) => match e.kind() {
                io::ErrorKind::WouldBlock => return devs, // due to 'set_read_timeout'
                _ => panic!("LIFX socket error: {:?}", e),
            },
        }
    }
}

// read from 'rx', responding to events
fn respond_to_events(mode: Arc<Mutex<Mode>>, rx: Receiver<InputEvent>, opts: Opts) {
    // set up evdev-share
    let mut key_send_buf = [0; 2];
    let key_send_addrs: Vec<SocketAddr> = opts
        .key_send_ips
        .iter()
        .map(|ip| SocketAddr::new(*ip, opts.key_send_port))
        .collect();
    let key_send_sock = &UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], 0))).unwrap();

    // set up LIFX
    let lifx_devs = get_lifx_addresses();
    let mut lifx_devs = lifx_devs.iter().cycle().peekable();
    let mut lifx_target = lifx_devs
        .next()
        .unwrap_or_else(|| panic!("No LIFX devices found"))
        .clone();
    let lifx_sock = UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], LIFX_PORT))).unwrap();
    lifx_sock.set_read_timeout(Some(LIFX_TIMEOUT)).unwrap();
    //TODO this initial value is never actually used
    let mut hsbk = HSBK {
        brightness: 0,
        hue: 0,
        kelvin: 0,
        saturation: 0,
    };

    // initialise state
    let mut prev_mode = Normal; // the mode we were in before the current one
    let mut held = HashSet::new(); // keys which have been pressed since they were last released
    let mut last_key = KEY_RESERVED; // will be updated before it's ever read

    // set up GPIO stuff
    let mut led_map = HashMap::new();
    if !opts.no_gpio {
        for led in LED::iter() {
            let port = led.id();

            // copied from my 'gpio-button' crate - see there for more info
            let gpio = loop {
                match SysFsGpioOutput::open(port) {
                    Ok(x) => {
                        println!("Successfully opened GPIO {}", port);
                        break x;
                    }
                    Err(e) => match e.kind() {
                        io::ErrorKind::PermissionDenied => {
                            println!("Permission error on GPIO {}, trying again", port);
                            sleep(Duration::from_millis(RETRY_PAUSE_MS));
                        }
                        _ => panic!("Failed to open GPIO: {}", e),
                    },
                }
            };

            led_map.insert(led, gpio);
        }
    }

    let mut set_led = |led: LED, x: bool| {
        //TODO this is a bit ugly - in practice guard passes iff '!opts.no_gpio'
        if let Some(led) = led_map.get_mut(&led) {
            led.set_value(x)
                .unwrap_or_else(|e| println!("Failed to set GPIO: {}", e))
        }
    };

    // flash all lights to show we have finished initialising
    for x in LED::iter() {
        set_led(x, true);
        sleep(Duration::from_millis(400));
        set_led(x, false)
    }
    sleep(Duration::from_millis(400));
    for l in mode.lock().unwrap().led() {
        set_led(l, true);
    }

    loop {
        let e = rx.recv().unwrap();
        if let EventCode::EV_KEY(k) = e.event_code {
            let ev_type = KeyEventType::from(e.value);
            if ev_type == Pressed {
                set_led(LED::Red, false)
            };

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
            let shift = held.contains(&KEY_LEFTSHIFT) || held.contains(&KEY_RIGHTSHIFT);
            let _alt = held.contains(&KEY_LEFTALT); // RIGHT_ALT is reserved for switching modes

            if opts.debug {
                println!("{:?},  {:?}", k, ev_type);
            }

            // right-alt released - switch mode
            // we only do this when nothing is held, to avoid confusing X, virtual keyboards etc.
            if k == KEY_RIGHTALT && ev_type == Released && held.is_empty() {
                let new_mode = match last_key {
                    KEY_ESC => Some(Idle),
                    KEY_DOT => Some(Normal),
                    KEY_T => Some(TV),
                    KEY_COMMA => Some(Sending),
                    KEY_Q => Some(Quiet),
                    KEY_RIGHTALT => Some(prev_mode), // this is a bit special - nothing else was pressed
                    _ => {
                        println!("Key does not correspond to a mode: {:?}", last_key);
                        None
                    }
                };
                if let Some(new_mode) = new_mode {
                    let mut mode = mode.lock().unwrap();
                    prev_mode = *mode;
                    *mode = new_mode;
                    if prev_mode == Idle {
                        xinput(XInput::Disable, opts.debug)
                    };
                    if new_mode == Idle {
                        xinput(XInput::Enable, opts.debug)
                    };
                    println!("Entering mode: {:?}", new_mode);
                    for l in prev_mode.led() {
                        set_led(l, false)
                    }
                    for l in new_mode.led() {
                        set_led(l, true)
                    }
                }
            } else if held.contains(&KEY_RIGHTALT) {
                // just wait for everything to be released
            } else {
                match *mode.lock().unwrap() {
                    Idle => (),
                    Quiet => {
                        if ev_type == Pressed {
                            set_led(LED::Red, true);
                        }
                    }
                    Sending => {
                        if opts.debug {
                            println!("Sending: {:?}, {:?}", k, ev_type);
                        }
                        key_send_buf[0] = k as u8;
                        key_send_buf[1] = ev_type as u8;
                        key_send_addrs.iter().for_each(|a| {
                            key_send_sock
                                .send_to(&mut key_send_buf, a)
                                .unwrap_or_else(|e| {
                                    println!("Failed to send: {}", e);
                                    0
                                });
                        });
                    }
                    Normal => {
                        let mut update_lifx =
                            |event_type: KeyEventType, inc: &dyn Fn(HSBK) -> HSBK| match event_type
                            {
                                Pressed => match get_lifx_state(&lifx_sock, lifx_target) {
                                    Ok((_, _, hsbk0)) => {
                                        hsbk = inc(hsbk0);
                                        set_hsbk(&lifx_sock, lifx_target, hsbk);
                                    }
                                    Err(e) => {
                                        println!("Failed to get bulb state: {:?}", e);
                                    }
                                },
                                Repeated => {
                                    hsbk = inc(hsbk);
                                    set_hsbk(&lifx_sock, lifx_target, hsbk);
                                }
                                Released => (),
                            };
                        let stereo = |cmd: &str| ir_cmd("stereo", cmd, ev_type, opts.debug);
                        let stereo_once = |cmd: &str| ir_cmd_once("stereo", cmd, opts.debug);
                        match (k, ev_type) {
                            (KEY_ESC, Released) => {
                                if ctrl {
                                    xinput(XInput::Enable, opts.debug);
                                    for (_mode, mut led) in led_map {
                                        led.set_low().unwrap_or_else(|e| {
                                            println!("Failed to reset LED: {}", e)
                                        });
                                    }
                                    panic!("Program ended by keypress")
                                }
                            }
                            (KEY_P, Pressed) => {
                                stereo_once("KEY_POWER");
                                sleep(Duration::from_secs(1));
                                stereo_once("KEY_TAPE");
                            }
                            (KEY_S, Pressed) => {
                                match get_lifx_state(
                                    &lifx_sock,
                                    lifx_devs.peek().unwrap().clone().clone(),
                                ) {
                                    Err(e) => {
                                        println!("Failed to change active LIFX device: {}", e)
                                    }
                                    Ok(s) => {
                                        lifx_target = lifx_devs.next().unwrap().clone();
                                        println!(
                                            "Changing active LIFX device to {}:\n  {:?}",
                                            lifx_target, s
                                        );
                                        hsbk = s.2;

                                        // flash to half brightness
                                        set_hsbk_delayed(
                                            &lifx_sock,
                                            lifx_target,
                                            HSBK {
                                                brightness: hsbk.brightness / 2,
                                                ..hsbk
                                            },
                                            LIFX_FLASH_TIME,
                                        );
                                        // TODO does the bulb queue messages? if so this is unnecessary, if not it's a race condition
                                        sleep(LIFX_FLASH_TIME);
                                        set_hsbk_delayed(
                                            &lifx_sock,
                                            lifx_target,
                                            hsbk,
                                            LIFX_FLASH_TIME,
                                        );
                                    }
                                }
                            }
                            (KEY_VOLUMEUP, _) => stereo("KEY_VOLUMEUP"),
                            (KEY_VOLUMEDOWN, _) => stereo("KEY_VOLUMEDOWN"),
                            (KEY_MUTE, Pressed) => stereo_once("muting"),
                            (KEY_PLAYPAUSE, Pressed) => mpris("PlayPause", opts.debug),
                            (KEY_PREVIOUSSONG, Pressed) => mpris("Previous", opts.debug),
                            (KEY_NEXTSONG, Pressed) => mpris("Next", opts.debug),
                            (KEY_L, Pressed) => match get_lifx_state(&lifx_sock, lifx_target) {
                                Ok((_, power0, _)) => {
                                    let power = match power0 {
                                        PowerLevel::Enabled => PowerLevel::Standby,
                                        PowerLevel::Standby => PowerLevel::Enabled,
                                    };
                                    set_lifx_power(&lifx_sock, lifx_target, power);
                                }
                                Err(e) => println!("Failed to get LIFX power: {:?}", e),
                            },
                            //TODO this feels like it could be drier, somehow
                            (KEY_LEFT, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let inc = 256;
                                    HSBK {
                                        hue: if ctrl {
                                            hsbk.hue.wrapping_sub(inc * 16)
                                        } else if shift {
                                            hsbk.hue.wrapping_sub(inc * 4)
                                        } else {
                                            hsbk.hue.wrapping_sub(inc)
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_RIGHT, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let inc = 256;
                                    HSBK {
                                        hue: if ctrl {
                                            hsbk.hue.wrapping_add(inc * 16)
                                        } else if shift {
                                            hsbk.hue.wrapping_add(inc * 4)
                                        } else {
                                            hsbk.hue.wrapping_add(inc)
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_EQUAL, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let max = 65535;
                                    HSBK {
                                        saturation: if ctrl {
                                            max
                                        } else {
                                            hsbk.saturation
                                                .checked_add(if shift { 2048 } else { 512 })
                                                .unwrap_or(max)
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_MINUS, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let min = 0;
                                    HSBK {
                                        saturation: if ctrl {
                                            min
                                        } else {
                                            hsbk.saturation
                                                .checked_sub(if shift { 2048 } else { 512 })
                                                .unwrap_or(min)
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_UP, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let max = 65535;
                                    HSBK {
                                        brightness: if ctrl {
                                            max
                                        } else {
                                            hsbk.brightness
                                                .checked_add(if shift { 2048 } else { 512 })
                                                .unwrap_or(max)
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_DOWN, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let min = 0;
                                    HSBK {
                                        brightness: if ctrl {
                                            min
                                        } else {
                                            hsbk.brightness
                                                .checked_sub(if shift { 2048 } else { 512 })
                                                .unwrap_or(min)
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_LEFTBRACE, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let min = 1500;
                                    HSBK {
                                        kelvin: if ctrl {
                                            min
                                        } else {
                                            std::cmp::max(
                                                hsbk.kelvin - (if shift { 200 } else { 50 }),
                                                min,
                                            )
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (KEY_RIGHTBRACE, event_type) => update_lifx(
                                event_type,
                                &(|hsbk: HSBK| {
                                    let max = 9000;
                                    HSBK {
                                        kelvin: if ctrl {
                                            max
                                        } else {
                                            std::cmp::min(
                                                hsbk.kelvin + (if shift { 200 } else { 50 }),
                                                max,
                                            )
                                        },
                                        ..hsbk
                                    }
                                }),
                            ),
                            (_, Pressed) => set_led(LED::Red, true),
                            _ => (),
                        }
                    }
                    TV => {
                        let tv = |cmd: &str| ir_cmd("tv", cmd, ev_type, opts.debug);
                        let tv_once = |cmd: &str| ir_cmd_once("tv", cmd, opts.debug);
                        let switcher = |cmd: &str| ir_cmd("switcher", cmd, ev_type, opts.debug);
                        match k {
                            KEY_SPACE => {
                                if ev_type == Pressed {
                                    let d = Duration::from_millis(if ctrl { 1000 } else { 350 });
                                    tv_once("KEY_AUX");
                                    sleep(d);
                                    tv_once("KEY_AUX");
                                    sleep(d);
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
                            _ => {
                                if ev_type == Pressed {
                                    set_led(LED::Red, true)
                                }
                            }
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
// enable, disable all devices
fn xinput(action: XInput, debug: bool) {
    let devs = String::from_utf8(
        Command::new("xinput")
            .args(&["list", "--name-only"])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap();
    for dev in devs
        .lines()
        .filter(|dev| dev.strip_prefix("Virtual").is_none())
    {
        let action_str = match action {
            XInput::Enable => "enable",
            XInput::Disable => "disable",
        };
        let res = Command::new("xinput").args(&[action_str, dev]).output();
        let cmd_name = String::from(action_str) + (" xinput device");
        handle_cmd(res, &cmd_name, dev, debug);
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash, EnumIter)]
enum LED {
    Blue,
    Yellow,
    Red,
    Green,
    White,
}
impl LED {
    fn id(&self) -> u16 {
        match self {
            LED::Blue => 12,
            LED::Green => 13,
            LED::Red => 5,
            LED::White => 16,
            LED::Yellow => 6,
        }
    }
}

// program mode
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash, EnumIter)]
enum Mode {
    Idle,    // let the OS handle keypresses
    Sending, // send keypresses over UDP
    Normal,
    TV,
    Quiet, // just turn off LEDs
}
impl Mode {
    fn led(self) -> Vec<LED> {
        match self {
            Idle => vec![LED::White],
            Sending => vec![LED::Yellow],
            Normal => vec![LED::Blue],
            TV => vec![LED::Green],
            Quiet => vec![],
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
