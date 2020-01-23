use clap::{self, Clap};
use evdev_rs::enums::{int_to_ev_key, EventCode, EV_KEY::*};
use evdev_rs::*;
use inotify::{EventMask, Inotify, WatchMask};
use librespot::connect::spirc::{Spirc, SpircTask};
use librespot::core::authentication::Credentials;
use librespot::core::config::{ConnectConfig, DeviceType, SessionConfig};
use librespot::core::session::Session;
use librespot::playback::audio_backend::{self, Sink};
use librespot::playback::config::{Bitrate, PlayerConfig};
use librespot::playback::mixer::{self, Mixer, MixerConfig};
use librespot::playback::player::Player;
use lifx_core::Message;
use lifx_core::RawMessage;
use lifx_core::HSBK;
use std::fs::{read_dir, File};
use std::io;
use std::net::SocketAddr;
use std::net::UdpSocket;
use std::path::PathBuf;
use std::process::Command;
use std::result::*;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;
use std::thread;
use std::thread::sleep;
use std::time::Duration;

// futures stuff - only used by the copied code from librespot
use futures::{Async, Future, Poll, Stream};
use tokio_core::reactor::Core;
use tokio_io::IoStream;

/* TODO

maintainability
    once I actually understand the borrow checker
        tx, tx1...
        read_dev itself shouldnt be responsible for spawning the thread
        spotify_main should be able to go into it's own thread like everything else
    decide what to explicitly import ('use' vs qualification)
        more *s?
        tooling to manage imports?
    cross-compile without docker
        ask on irc: https://gitter.im/librespot-org/spotify-connect-resources
stability
    too many unwraps (particularly in utility functions)
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
}

// useful constants
const EVDEV_DIR: &str = "/dev/input/";

fn main() {
    // get data from command line args
    let opts: Opts = Opts::parse();

    // channel for Spirc
    let (txs, rxs) = channel(); //TODO doesn't need to be a channel, only set once

    // set up channel for keyboard events
    let (tx, rx) = channel();

    // read from existing devices
    for r in read_dir(&EVDEV_DIR).unwrap() {
        let p = r.unwrap().path();
        read_dev(tx.clone(), p);
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
                // println!("{:?}", event);
                if !event.mask.contains(EventMask::ISDIR) {
                    //TODO cf. my Haskell lib for more principled solution
                    sleep(Duration::from_millis(300)); // sleep to avoid permission error
                    let p = PathBuf::from(EVDEV_DIR).join(event.name.unwrap().to_str().unwrap());
                    read_dev(tx1.clone(), p);
                }
            }
        }
    });

    // watch for network events
    let evdev_port = opts.evdev_port.clone();
    thread::spawn(move || {
        //TODO security
        let s = &UdpSocket::bind(SocketAddr::from(([0, 0, 0, 0], evdev_port))).unwrap();
        loop {
            let mut buf = [0; 2];
            let (_n_bytes, _addr) = s.recv_from(&mut buf).unwrap();
            let k = int_to_ev_key(buf[0] as u32).unwrap();
            let c = EventCode::EV_KEY(k);
            let t = TimeVal::new(0, 0);
            let ev = InputEvent::new(&t, &c, buf[1] as i32);
            tx.clone().send(ev).unwrap();
        }
    });

    thread::spawn(move || {
        respond_to_events(rx, rxs);
    });

    main_spotify(txs, opts.spotify_device_name, opts.spotify_password);
}

// create a new thread to read events from the device at p, and send them on s
fn read_dev(s: Sender<InputEvent>, p: PathBuf) {
    thread::spawn(move || {
        let file = File::open(p).unwrap();
        let dev = Device::new_from_fd(file).unwrap();
        loop {
            let (_, event) = dev
                .next_event(ReadFlag::NORMAL | ReadFlag::BLOCKING)
                .unwrap();
            s.send(event).unwrap();
        }
    });
}

// send off a command to a device, based on 'lircd.conf'
fn ir_cmd(dev: &str, cmd: &str) {
    Command::new("irsend")
        .arg("SEND_ONCE")
        .arg(dev)
        .arg(cmd)
        .output()
        .unwrap();
}

// LIFX helpers, until there's a complete high-level LAN API in Rust
fn lifx_send(sock: &UdpSocket, target: SocketAddr, msg: Message) {
    let raw = RawMessage::build(&Default::default(), msg).unwrap();
    sock.send_to(&raw.pack().unwrap(), &target).unwrap();
}
fn set_hsbk(sock: &UdpSocket, target: SocketAddr, hsbk: HSBK) {
    let msg = Message::LightSetColor {
        color: hsbk,
        duration: 0,
        reserved: 0,
    };
    lifx_send(sock, target, msg)
}
// fn get_hsbk(sock: &UdpSocket, target: SocketAddr) -> Result<HSBK> {
fn get_hsbk(sock: &UdpSocket, target: SocketAddr) -> Result<HSBK, lifx_core::Error> {
    let mut buf = [0; 88];
    lifx_send(sock, target, Message::LightGet);
    let (_n_bytes, _addr) = sock.recv_from(&mut buf)?;
    let raw = RawMessage::unpack(&buf)?;
    let msg = Message::from_raw(&raw).unwrap();
    if let Message::LightState { color: hsbk, .. } = msg {
        Ok(hsbk)
    } else {
        Err(lifx_core::Error::ProtocolError(String::from(
            "failed to decode light response",
        )))
    }
}

// read from 'rx', responding to events
fn respond_to_events(rx: Receiver<InputEvent>, txs: Receiver<Arc<Spirc>>) {
    let lifx_sock = UdpSocket::bind("0.0.0.0:56700").unwrap();
    lifx_sock
        .set_read_timeout(Some(Duration::from_secs(3)))
        .unwrap();
    let lifx_target: SocketAddr = "192.168.1.188:56700".parse().unwrap();

    // initialise state
    let mut hsbk = get_hsbk(&lifx_sock, lifx_target).unwrap();
    let mut ctrl = false;
    let mut _shift = false;
    let mut _alt = false;
    let mut mode = Mode::Normal;

    // wait to receive Spirc
    let spirc = txs
        .recv_timeout(Duration::from_secs(3))
        .expect("timed out waiting for Spirc object");

    loop {
        let e = rx.recv().unwrap();
        if let EventCode::EV_KEY(k) = e.event_code {
            // println!("{:?},  {:?}", k, e.value); //TODO arg for printing this sort of thing
            match (&k, e.value) {
                (KEY_LEFTCTRL, 1) => ctrl = true,
                (KEY_RIGHTCTRL, 1) => ctrl = true,
                (KEY_LEFTSHIFT, 1) => _shift = true,
                (KEY_RIGHTSHIFT, 1) => _shift = true,
                (KEY_LEFTALT, 1) => _alt = true,
                (KEY_RIGHTALT, 1) => _alt = true,
                (KEY_LEFTCTRL, 0) => ctrl = false,
                (KEY_RIGHTCTRL, 0) => ctrl = false,
                (KEY_LEFTSHIFT, 0) => _shift = false,
                (KEY_RIGHTSHIFT, 0) => _shift = false,
                (KEY_LEFTALT, 0) => _alt = false,
                (KEY_RIGHTALT, 0) => _alt = false,
                _ => (),
            }
            match mode {
                Mode::Normal => {
                    match (&k, e.value) {
                        (KEY_T, 1) => mode = Mode::TV,
                        (KEY_P, 1) => {
                            ir_cmd("stereo", "KEY_POWER");
                            sleep(Duration::from_secs(1));
                            ir_cmd("stereo", "KEY_TAPE");
                        }
                        (KEY_VOLUMEUP, 1) => ir_cmd("stereo", "KEY_VOLUMEUP"),
                        (KEY_VOLUMEDOWN, 1) => ir_cmd("stereo", "KEY_VOLUMEDOWN"),
                        (KEY_MUTE, 1) => ir_cmd("stereo", "muting"),
                        (KEY_PLAYPAUSE, 1) => spirc.play_pause(),
                        (KEY_PREVIOUSSONG, 1) => spirc.prev(),
                        (KEY_NEXTSONG, 1) => spirc.next(),
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
                Mode::TV => match (&k, e.value) {
                    (KEY_T, 1) => mode = Mode::Normal,
                    (KEY_SPACE, 1) => {
                        ir_cmd("tv", "KEY_AUX");
                        sleep(Duration::from_millis(300));
                        ir_cmd("tv", "KEY_AUX");
                        sleep(Duration::from_millis(300));
                        ir_cmd("tv", "KEY_OK");
                    }
                    (KEY_P, 1) => ir_cmd("tv", "KEY_POWER"),
                    (KEY_1, 1) => {
                        let dev = if ctrl { "switcher" } else { "tv" };
                        ir_cmd(dev, "KEY_1")
                    }
                    (KEY_2, 1) => {
                        let dev = if ctrl { "switcher" } else { "tv" };
                        ir_cmd(dev, "KEY_2")
                    }
                    (KEY_3, 1) => {
                        let dev = if ctrl { "switcher" } else { "tv" };
                        ir_cmd(dev, "KEY_3")
                    }
                    (KEY_4, 1) => ir_cmd("tv", "KEY_4"),
                    (KEY_5, 1) => ir_cmd("tv", "KEY_5"),
                    (KEY_6, 1) => ir_cmd("tv", "KEY_6"),
                    (KEY_7, 1) => ir_cmd("tv", "KEY_7"),
                    (KEY_8, 1) => ir_cmd("tv", "KEY_8"),
                    (KEY_9, 1) => ir_cmd("tv", "KEY_9"),
                    (KEY_0, 1) => ir_cmd("tv", "KEY_0"),
                    (KEY_VOLUMEUP, 1) => ir_cmd("tv", "KEY_VOLUMEUP"),
                    (KEY_VOLUMEDOWN, 1) => ir_cmd("tv", "KEY_VOLUMEDOWN"),
                    (KEY_MUTE, 1) => ir_cmd("tv", "KEY_MUTE"),
                    (KEY_COMMA, 1) => ir_cmd("tv", "KEY_CHANNELDOWN"),
                    (KEY_DOT, 1) => ir_cmd("tv", "KEY_CHANNELUP"),
                    (KEY_S, 1) => ir_cmd("tv", "KEY_SETUP"),
                    (KEY_G, 1) => ir_cmd("tv", "KEY_G"),
                    (KEY_Q, 1) => ir_cmd("tv", "KEY_MENU"),
                    (KEY_UP, 1) => ir_cmd("tv", "KEY_UP"),
                    (KEY_DOWN, 1) => ir_cmd("tv", "KEY_DOWN"),
                    (KEY_LEFT, 1) => ir_cmd("tv", "KEY_LEFT"),
                    (KEY_RIGHT, 1) => ir_cmd("tv", "KEY_RIGHT"),
                    (KEY_ENTER, 1) => ir_cmd("tv", "KEY_OK"),
                    (KEY_BACKSPACE, 1) => ir_cmd("tv", "KEY_BACK"),
                    (KEY_I, 1) => ir_cmd("tv", "KEY_INFO"),
                    (KEY_ESC, 1) => ir_cmd("tv", "KEY_EXIT"),
                    _ => (),
                },
            }
        }
    }
}

// create a Connect device through librespot
fn main_spotify(txs: Sender<Arc<Spirc>>, name: String, password: String) {
    let mut core = Core::new().unwrap();
    let handle = core.handle();

    let backend = audio_backend::find(None).expect("Invalid backend");

    let mixer_name: Option<&String> = None;
    let mixer = mixer::find(mixer_name).expect("Invalid mixer");
    // let mixer = mixer::find(None: Option<&String>).expect("Invalid mixer"); //TODO when 'type ascription' reaches stable

    let credentials =
        Credentials::with_password(String::from("georgefsthomas@gmail.com"), password);

    let session_config = SessionConfig {
        device_id: name.clone(),
        ..SessionConfig::default()
    };

    let player_config = PlayerConfig {
        bitrate: Bitrate::Bitrate320,
        ..PlayerConfig::default()
    };

    let connect_config = ConnectConfig {
        name,
        device_type: DeviceType::default(),
        volume: u16::max_value(),
        linear_volume: false,
    };

    let connect = Session::connect(
        session_config.clone(),
        credentials.clone(),
        None,
        handle.clone(),
    );

    let init = Main {
        spirc_sender: txs,
        player_config,
        connect_config: connect_config.clone(),
        backend,
        device: None,
        mixer,
        mixer_config: MixerConfig::default(),
        connect: connect,
        spirc: None,
        spirc_task: None,
        shutdown: false,
        signal: Box::new(tokio_signal::ctrl_c().flatten_stream()),
    };

    core.run(init).unwrap()
}

// data structures
#[derive(Debug)]
enum Mode {
    Normal,
    TV,
}

//TODO stuff from here down is more or less copied straight from librespot: main.rs
//     it scares me
//     attempt to understand it and modify it to my purposes

struct Main {
    spirc_sender: Sender<Arc<Spirc>>, //TODO this is a bit of a hack to get the Spirc object back to main thread
    player_config: PlayerConfig,
    connect_config: ConnectConfig,
    backend: fn(Option<String>) -> Box<dyn Sink>,
    device: Option<String>,
    mixer: fn(Option<MixerConfig>) -> Box<dyn Mixer>,
    mixer_config: MixerConfig,
    signal: IoStream<()>,
    spirc: Option<Arc<Spirc>>,
    spirc_task: Option<SpircTask>,
    connect: Box<dyn Future<Item = Session, Error = io::Error>>,
    shutdown: bool,
}
impl Future for Main {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<(), ()> {
        loop {
            let mut progress = false;

            // this seems to set up a session - only happens once at start
            if let Async::Ready(session) = self.connect.poll().unwrap() {
                println!("Setting up spotify");

                self.connect = Box::new(futures::future::empty());
                let mixer_config = self.mixer_config.clone();
                let mixer = (self.mixer)(Some(mixer_config));
                let player_config = self.player_config.clone();
                let connect_config = self.connect_config.clone();

                let audio_filter = mixer.get_audio_filter();
                let backend = self.backend;
                let device = self.device.clone();
                let (player, _) =
                    Player::new(player_config, session.clone(), audio_filter, move || {
                        (backend)(device)
                    });

                let (spirc, spirc_task) = Spirc::new(connect_config, session, player, mixer);
                let a = Arc::new(spirc);
                self.spirc = Some(a.clone());
                self.spirc_sender.send(a.clone()).unwrap();
                self.spirc_task = Some(spirc_task);

                progress = true;
            }

            // handle ctrl-c (copied)
            if let Async::Ready(Some(())) = self.signal.poll().unwrap() {
                println!("Ctrl-C received");
                if !self.shutdown {
                    if let Some(ref spirc) = self.spirc {
                        spirc.shutdown();
                    } else {
                        return Ok(Async::Ready(()));
                    }
                    self.shutdown = true;
                } else {
                    return Ok(Async::Ready(()));
                }
                progress = true;
            }

            // handle spirc tasks?
            if let Some(ref mut spirc_task) = self.spirc_task {
                println!("Running spotify task"); //TODO more info
                if let Async::Ready(()) = spirc_task.poll().unwrap() {
                    // only passes at shutdown? but polling runs the next task
                    if self.shutdown {
                        return Ok(Async::Ready(()));
                    } else {
                        panic!("Spirc shut down unexpectedly");
                    }
                }
            }

            if !progress {
                return Ok(Async::NotReady);
            }
        }
    }
}
