//! Defines `SimpleJITBackend`.

use cretonne::binemit::{Addend, CodeOffset, Reloc, RelocSink, TrapSink};
use cretonne::isa::TargetIsa;
use cretonne::result::CtonError;
use cretonne::{self, ir, settings};
use cton_module::{Backend, DataContext, Linkage, ModuleNamespace};
use cton_native;
use std::ffi::CString;
use libc;
use memory::Memory;

/// A record of a relocation to perform.
enum RelocRecord {
    Func {
        offset: CodeOffset,
        reloc: Reloc,
        name: ir::ExternalName,
        addend: Addend,
    },
}

pub struct SimpleJITCompiledFunction {
    code: *mut u8,
    size: usize,
    relocs: Vec<RelocRecord>,
}

pub struct SimpleJITCompiledData {
    _storage: *mut u8,
    _size: usize,
    _relocs: Vec<RelocRecord>,
}

/// A `SimpleJITBackend` implements `Backend` and emits code and data into memory where it can be
/// directly called and accessed.
pub struct SimpleJITBackend {
    isa: Box<TargetIsa>,
    code_memory: Memory,
    _data_memory: Memory,
}

impl SimpleJITBackend {
    /// Create a new `SimpleJITBackend`.
    pub fn new() -> Self {
        let (flag_builder, isa_builder) = cton_native::builders().unwrap_or_else(|_| {
            panic!("host machine is not a supported target");
        });
        let isa = isa_builder.finish(settings::Flags::new(&flag_builder));
        Self::with_isa(isa)
    }

    /// Create a new `SimpleJITBackend` with an arbitrary target. This is mainly
    /// useful for testing.
    ///
    /// SimpleJIT requires a `TargetIsa` configured for non-PIC.
    ///
    /// To create a `SimpleJITBackend` for native use, use the `new` constructor
    /// instead.
    pub fn with_isa(isa: Box<TargetIsa>) -> Self {
        debug_assert!(!isa.flags().is_pic(), "SimpleJIT requires non-PIC code");
        Self {
            isa,
            code_memory: Memory::new(),
            _data_memory: Memory::new(),
        }
    }
}

impl Backend for SimpleJITBackend {
    type CompiledFunction = SimpleJITCompiledFunction;
    type CompiledData = SimpleJITCompiledData;

    type FinalizedFunction = *mut u8;
    type FinalizedData = Box<[u8]>;

    fn isa(&self) -> &TargetIsa {
        &*self.isa
    }

    fn declare_function(&mut self, _name: &str, _linkage: Linkage) {
        // Nothing to do.
    }

    fn declare_data(&mut self, _name: &str, _linkage: Linkage, _writable: bool) {
        // Nothing to do.
    }

    fn define_function(
        &mut self,
        _name: &str,
        ctx: &cretonne::Context,
        _namespace: &ModuleNamespace<Self>,
        code_size: u32,
    ) -> Result<Self::CompiledFunction, CtonError> {
        let size = code_size as usize;
        let ptr = self.code_memory.allocate(size).expect(
            "TODO: handle OOM etc.",
        );
        let mut reloc_sink = SimpleJITRelocSink::new();
        let mut trap_sink = SimpleJITTrapSink {};
        ctx.emit_to_memory(ptr, &mut reloc_sink, &mut trap_sink, &*self.isa);

        Ok(Self::CompiledFunction {
            code: ptr,
            size,
            relocs: reloc_sink.relocs,
        })
    }

    fn define_data(&mut self, _name: &str, _data: &DataContext) -> Self::CompiledData {
        unimplemented!();
    }

    fn write_data_funcaddr(
        &mut self,
        _data: &mut Self::CompiledData,
        _offset: usize,
        _what: ir::FuncRef,
    ) {
        unimplemented!();
    }

    fn write_data_dataaddr(
        &mut self,
        _data: &mut Self::CompiledData,
        _offset: usize,
        _what: ir::GlobalVar,
        _usize: Addend,
    ) {
        unimplemented!();
    }

    fn finalize_function(
        &mut self,
        func: &Self::CompiledFunction,
        namespace: &ModuleNamespace<Self>,
    ) -> Self::FinalizedFunction {
        use std::ptr::write_unaligned;

        for record in &func.relocs {
            match *record {
                RelocRecord::Func {
                    ref reloc,
                    offset,
                    ref name,
                    addend,
                } => {
                    let ptr = func.code;
                    debug_assert!((offset as usize) < func.size);
                    let at = unsafe { ptr.offset(offset as isize) };
                    let (def, name_str, _signature) = namespace.get_function_definition(&name);
                    let base = match def {
                        Some(compiled) => compiled.code,
                        None => lookup_with_dlsym(name_str),
                    };
                    // TODO: Handle overflow.
                    let what = unsafe { base.offset(addend as isize) };
                    match *reloc {
                        Reloc::X86PCRel4 => {
                            // TODO: Handle overflow.
                            let pcrel = ((what as isize) - (at as isize)) as i32;
                            unsafe { write_unaligned(at as *mut i32, pcrel) };
                        }
                        Reloc::X86Abs4 => {
                            // TODO: Handle overflow.
                            unsafe { write_unaligned(at as *mut u32, what as u32) };
                        }
                        Reloc::X86Abs8 => {
                            unsafe { write_unaligned(at as *mut u64, what as u64) };
                        }
                        Reloc::X86GOTPCRel4 |
                        Reloc::X86PLTRel4 => panic!("unexpected PIC relocation"),
                        _ => unimplemented!(),
                    }
                }
            }
        }

        // Now that we're done patching, make the memory executable.
        self.code_memory.make_executable();
        func.code
    }

    fn finalize_data(
        &mut self,
        _data: &Self::CompiledData,
        _namespace: &ModuleNamespace<Self>,
    ) -> Self::FinalizedData {
        unimplemented!()
    }
}

fn lookup_with_dlsym(name: &str) -> *const u8 {
    let c_str = CString::new(name).unwrap();
    let c_str_ptr = c_str.as_ptr();
    let sym = unsafe { libc::dlsym(libc::RTLD_DEFAULT, c_str_ptr) };
    if sym.is_null() {
        panic!("can't resolve symbol {}", name);
    }
    sym as *const u8
}

struct SimpleJITRelocSink {
    pub relocs: Vec<RelocRecord>,
}

impl SimpleJITRelocSink {
    pub fn new() -> Self {
        Self { relocs: Vec::new() }
    }
}

impl RelocSink for SimpleJITRelocSink {
    fn reloc_ebb(&mut self, _offset: CodeOffset, _reloc: Reloc, _ebb_offset: CodeOffset) {
        unimplemented!();
    }

    fn reloc_external(
        &mut self,
        offset: CodeOffset,
        reloc: Reloc,
        name: &ir::ExternalName,
        addend: Addend,
    ) {
        self.relocs.push(RelocRecord::Func {
            offset,
            reloc,
            name: name.clone(),
            addend,
        });
    }

    fn reloc_jt(&mut self, _offset: CodeOffset, _reloc: Reloc, _jt: ir::JumpTable) {
        unimplemented!();
    }
}

struct SimpleJITTrapSink {}

impl TrapSink for SimpleJITTrapSink {
    // Ignore traps for now. For now, frontends should just avoid generating code that traps.
    fn trap(&mut self, _offset: CodeOffset, _srcloc: ir::SourceLoc, _code: ir::TrapCode) {}
}
