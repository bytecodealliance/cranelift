//! Test command for verifying the unwind emitted for each function.
//!
//! The `unwind` test command runs each function through the full code generator pipeline.
#![cfg_attr(feature = "cargo-clippy", allow(clippy::cast_ptr_alignment))]

use crate::subtest::{run_filecheck, Context, SubTest, SubtestResult};
use cranelift_codegen;
use cranelift_codegen::ir;
use cranelift_reader::TestCommand;
use std::borrow::Cow;
use std::fmt::Write;

struct TestUnwind;

pub fn subtest(parsed: &TestCommand) -> SubtestResult<Box<dyn SubTest>> {
    assert_eq!(parsed.command, "unwind");
    if !parsed.options.is_empty() {
        Err(format!("No options allowed on {}", parsed))
    } else {
        Ok(Box::new(TestUnwind))
    }
}

impl SubTest for TestUnwind {
    fn name(&self) -> &'static str {
        "unwind"
    }

    fn is_mutating(&self) -> bool {
        false
    }

    fn needs_isa(&self) -> bool {
        true
    }

    fn run(&self, func: Cow<ir::Function>, context: &Context) -> SubtestResult<()> {
        let isa = context.isa.expect("unwind needs an ISA");
        let mut comp_ctx = cranelift_codegen::Context::for_function(func.into_owned());

        comp_ctx.compile(isa).expect("failed to compile function");

        let mut mem = Vec::new();
        comp_ctx.emit_unwind_info(isa, &mut mem);

        let mut text = String::new();
        if mem.len() == 0 {
            writeln!(text, "No unwind information.").unwrap();
        } else {
            print_unwind_info(&mut text, &mem);
        }

        run_filecheck(&text, context)
    }
}

fn print_unwind_info(text: &mut String, mem: &[u8]) {
    assert!(mem.len() >= core::mem::size_of::<UnwindInfo>());
    let info = unsafe { UnwindInfo::from_ptr(mem.as_ptr()) };

    let node_count = info.unwind_code_count();

    // Assert correct alignment and padding of the unwind information
    assert!(mem.len() % 4 == 0);
    assert_eq!(
        mem.len(),
        core::mem::size_of::<UnwindInfo>()
            + ((node_count as usize) * core::mem::size_of::<UnwindCode>())
            + if (node_count & 1) == 1 {
                core::mem::size_of::<UnwindCode>()
            } else {
                0
            }
    );

    writeln!(
        text,
        "Unwind info: version = {}, flags = {}, prologue size: {}, unwind code count = {}, frame register = {}, frame register offset = {}",
        info.version(),
        info.flags(),
        info.prologue_size(),
        node_count,
        info.frame_register(),
        info.frame_register_offset()
    ).unwrap();

    let mut i = 0;
    while i < node_count {
        let offset = (core::mem::size_of::<UnwindInfo>()
            + ((i as usize) * core::mem::size_of::<UnwindCode>())) as isize;
        let code = unsafe { UnwindCode::from_ptr(mem.as_ptr().offset(offset)) };

        write!(
            text,
            "Unwind code: offset = {}, operation = {:#?}, info = {}",
            code.offset(),
            code.operation(),
            code.info(),
        )
        .unwrap();

        match code.additional_nodes() {
            0 => {
                writeln!(text).unwrap();
                i += 1;
            }
            1 => {
                let offset = offset + core::mem::size_of::<UnwindCode>() as isize;
                writeln!(text, ", value = {}", unsafe {
                    core::ptr::read_unaligned(mem.as_ptr().offset(offset) as *const u16)
                })
                .unwrap();
                i += 2;
            }
            2 => {
                let offset = offset + core::mem::size_of::<UnwindCode>() as isize;
                writeln!(text, ", value = {}", unsafe {
                    core::ptr::read_unaligned(mem.as_ptr().offset(offset) as *const u32)
                })
                .unwrap();
                i += 3;
            }
            _ => panic!("unexpected number of additional nodes"),
        };
    }
}

#[repr(C)]
struct UnwindInfo {
    version_and_flags: u8,
    prologue_size: u8,
    unwind_code_count: u8,
    frame_register: u8,
}

impl UnwindInfo {
    unsafe fn from_ptr(ptr: *const u8) -> Self {
        core::ptr::read_unaligned(ptr as *const Self)
    }

    fn version(&self) -> u8 {
        self.version_and_flags & 0x3
    }

    fn flags(&self) -> u8 {
        (self.version_and_flags & 0xF8) >> 3
    }

    fn prologue_size(&self) -> u8 {
        self.prologue_size
    }

    fn unwind_code_count(&self) -> u8 {
        self.unwind_code_count
    }

    fn frame_register(&self) -> u8 {
        self.frame_register & 0xF
    }

    fn frame_register_offset(&self) -> u8 {
        (self.frame_register & 0xF0) >> 4
    }
}

#[repr(C)]
struct UnwindCode {
    offset: u8,
    code_and_info: u8,
}

#[derive(Debug)]
enum UnwindOperation {
    PushNonvolatileRegister,
    LargeStackAlloc,
    SmallStackAlloc,
    SetFramePointer,
    SaveNonvolatileRegister,
    SaveNonvolatileRegisterFar,
    SaveXmm128,
    SaveXmm128Far,
    PushMachineFrame,
}

impl From<u8> for UnwindOperation {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::PushNonvolatileRegister,
            1 => Self::LargeStackAlloc,
            2 => Self::SmallStackAlloc,
            3 => Self::SetFramePointer,
            4 => Self::SaveNonvolatileRegister,
            5 => Self::SaveNonvolatileRegisterFar,
            6 => Self::SaveXmm128,
            7 => Self::SaveXmm128Far,
            8 => Self::PushMachineFrame,
            _ => panic!("unsupported unwind operation"),
        }
    }
}

impl UnwindCode {
    unsafe fn from_ptr(ptr: *const u8) -> Self {
        core::ptr::read_unaligned(ptr as *const Self)
    }

    fn offset(&self) -> u8 {
        self.offset
    }

    fn operation(&self) -> UnwindOperation {
        (self.code_and_info & 0xF).into()
    }

    fn info(&self) -> u8 {
        (self.code_and_info & 0xF0) >> 4
    }

    fn additional_nodes(&self) -> u8 {
        match self.operation() {
            UnwindOperation::LargeStackAlloc => match self.info() {
                0 => 1,
                1 => 2,
                _ => panic!("unexpected stack alloc info value"),
            },
            UnwindOperation::SaveNonvolatileRegister => 1,
            UnwindOperation::SaveNonvolatileRegisterFar => 2,
            UnwindOperation::SaveXmm128 => 1,
            UnwindOperation::SaveXmm128Far => 2,
            _ => 0,
        }
    }
}
