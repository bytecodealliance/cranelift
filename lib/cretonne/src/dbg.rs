//! Debug tracing macros.
//!
//! This module defines the `dbg!` macro which works like `println!` except it writes to the
//! Cretonne tracing output file if enabled.
//!
//! Tracing can be enabled by setting the `CRETONNE_DBG` environment variable to something
/// other than `0`.
///
/// The output will appear in files named `cretonne.dbg.*`, where the suffix is named after the
/// thread doing the logging.

use std::ascii::AsciiExt;
use std::cell::RefCell;
use std::env;
use std::ffi::OsStr;
use std::fmt;
use std::fs::File;
use std::io::{self, Write};
use std::sync::atomic;
use std::thread;

static STATE: atomic::AtomicIsize = atomic::ATOMIC_ISIZE_INIT;

/// Is debug tracing enabled?
///
/// Debug tracing can be enabled by setting the `CRETONNE_DBG` environment variable to something
/// other than `0`.
///
/// This inline function turns into a constant `false` when debug assertions are disabled.
#[inline]
pub fn enabled() -> bool {
    if cfg!(debug_assertions) {
        match STATE.load(atomic::Ordering::Relaxed) {
            0 => initialize(),
            s => s > 0,
        }
    } else {
        false
    }
}

/// Initialize `STATE` from the environment variable.
fn initialize() -> bool {
    let enable = match env::var_os("CRETONNE_DBG") {
        Some(s) => s != OsStr::new("0"),
        None => false,
    };

    if enable {
        STATE.store(1, atomic::Ordering::Relaxed);
    } else {
        STATE.store(-1, atomic::Ordering::Relaxed);
    }

    enable
}

thread_local! {
    static WRITER : RefCell<io::BufWriter<File>> = RefCell::new(open_file());
}

/// Write a line with the given format arguments.
///
/// This is for use by the `dbg!` macro.
pub fn writeln_with_format_args(args: fmt::Arguments) -> io::Result<()> {
    WRITER.with(|rc| writeln!(*rc.borrow_mut(), "{}", args))
}

/// Open the tracing file for the current thread.
fn open_file() -> io::BufWriter<File> {
    let file = match thread::current().name() {
            None => File::create("cretonne.dbg"),
            Some(name) => {
                let mut path = "cretonne.dbg.".to_owned();
                for ch in name.chars() {
                    if ch.is_ascii() && ch.is_alphanumeric() {
                        path.push(ch);
                    }
                }
                File::create(path)
            }
        }
        .expect("Can't open tracing file");
    io::BufWriter::new(file)
}

/// Write a line to the debug trace file if tracing is enabled.
///
/// Arguments are the same as for `printf!`.
#[macro_export]
macro_rules! dbg {
    ($($arg:tt)+) => {
        if $crate::dbg::enabled() {
            // Drop the error result so we don't get compiler errors for ignoring it.
            // What are you going to do, log the error?
            $crate::dbg::writeln_with_format_args(format_args!($($arg)+)).ok();
        }
    }
}

/// Write a line to the debug trace file if tracing is enabled.
///
/// The first argument is the `DebugState` object. Remaining arguments are the
/// same as for `println!`.
#[macro_export]
macro_rules! trace {
    ($state:expr, $($arg:tt)+) => {
        if cfg!(debug_assertions) {
            if $state.flags.tracing_enabled {
                // Drop the error result so we don't get compiler errors for ignoring it.
                // What are you going to do, log the error?
                writeln!($state.get_tracing_writer(), $($arg)+).ok();
            }
        }
    }
}

macro_rules! trace_opt_action {
    ($state:expr, $who:expr, $what:expr, $($arg:tt)+) => {
        if cfg!(debug_assertions) {
            if $state.flags.tracing_enabled {
                let writer = $state.get_tracing_writer();
                write!(writer, "  {} {}: ", $who, $what).ok();
                writeln!(writer, $($arg)+).ok();
            }
        }
    }
}

#[macro_export]
macro_rules! perform_pass {
    ($state:expr, $who:expr) => {
        if cfg!(debug_assertions) {
            // For now, always perform all passes.
            writeln!($state.get_tracing_writer(), "performing {}", $who).ok();
            true
        } else {
            true
        }
    }
}

#[macro_export]
macro_rules! end_pass {
    ($state:expr, $who:expr) => {
        if cfg!(debug_assertions) {
            writeln!($state.get_tracing_writer(), "end {}", $who).ok();
        }
    }
}

/// Prepare to perfom a single optimization step.
///
/// The first argument is the `DebugState` object. The second is the name of the
/// optimization being performed. Remaining arguments are the same as for `println!`.
///
/// This optionally logs the optimization and tests whether it should be enabled
#[macro_export]
macro_rules! perform_optimization {
    ($state:expr, $who:expr, $($arg:tt)+) => {
        if cfg!(debug_assertions) {
            if !$state.consume_fuel() {
                trace_opt_action!($state, $who, "opt disabled", $($arg)+);
                false
            } else {
                trace_opt_action!($state, $who, "opt", $($arg)+);
                true
            }
        } else {
            true
        }
    }
}

/// Helper for printing lists.
pub struct DisplayList<'a, T>(pub &'a [T]) where T: 'a + fmt::Display;

impl<'a, T> fmt::Display for DisplayList<'a, T>
    where T: 'a + fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.split_first() {
            None => write!(f, "[]"),
            Some((first, rest)) => {
                write!(f, "[{}", first)?;
                for x in rest {
                    write!(f, ", {}", x)?;
                }
                write!(f, "]")
            }
        }
    }
}

/// Debugging flags.
///
/// A set of flag data for controlling debugging features.
#[derive(Clone)]
pub struct DebugFlags {
    /// This determins whether tracing is enabled in the !dbg macro.
    ///
    pub tracing_enabled: bool,

    /// Optimization Fuel.
    ///
    pub fuel: Option<u64>,
}

/// Debugging state.
///
pub struct DebugState {
    /// This determins whether tracing is enabled in the !dbg macro.
    ///
    pub flags: DebugFlags,

    /// The output writer for tracing.
    ///
    tracing_writer: Option<BufWriter<File>>,
}

impl DebugState {
    /// Construct a new DebugState.
    ///
    pub fn new() -> DebugState {
        DebugState {
            flags: DebugFlags {
                tracing_enabled: false,
                fuel: None,
            },
            tracing_writer: None,
        }
    }

    /// Return the writer for tracing output. This is used by the !trace macro.
    ///
    pub fn get_tracing_writer(&mut self) -> &mut BufWriter<File> {
        debug_assert!(self.flags.tracing_enabled);
        match self.tracing_writer {
            Some(ref mut w) => w,
            None => {
                self.tracing_writer = Some(open_file());
                self.get_tracing_writer()
            }
        }
    }

    /// If fuel tracking is enabled, test whether there is any fuel left, and
    /// consume one unit if possible.
    ///
    /// Optimizers should consume call this before performing each transformation
    /// and only proceed if it returns true. This makes it limit the number of
    /// optimizations performed with very fine granularity, and one can binary
    /// search through optimizations to find conditions of interest.
    ///
    /// This inline function turns into a constant `true` when debug assertions are
    /// disabled.
    #[inline]
    pub fn consume_fuel(&mut self) -> bool {
        if cfg!(debug_assertions) {
            if let Some(ref mut value) = self.flags.fuel {
                if *value == 0 {
                    // Fuel tracking is enabled and we have run out of fuel.
                    return false;
                }

                // Consume one unit.
                *value -= 1;
            }
        }
        true
    }

    /// Enable fuel tracking and establish a fuel level.
    ///
    /// Subsequent calls to `consume_fuel` will return `true` until fuel runs out.
    pub fn set_fuel(&mut self, new_fuel: u64) {
        if cfg!(debug_assertions) {
            self.flags.fuel = Some(new_fuel);
        }
    }

    /// Disable fuel tracking.
    ///
    /// Subsequent calls to `consume_fuel` will return `true`.
    pub fn disable_fuel_tracking(&mut self) {
        if cfg!(debug_assertions) {
            self.flags.fuel = None;
        }
    }
}
