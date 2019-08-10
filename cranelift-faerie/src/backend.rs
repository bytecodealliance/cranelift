//! Defines `FaerieBackend`.

use crate::container;
use crate::traps::{FaerieTrapManifest, FaerieTrapSink};
use cranelift_codegen::binemit::{
    Addend, CodeOffset, NullStackmapSink, NullTrapSink, Reloc, RelocSink, Stackmap, StackmapSink,
};
use cranelift_codegen::isa::{RegInfo, RegUnit, TargetIsa};
use cranelift_codegen::{self, binemit, ir, isa};
use cranelift_module::{
    Backend, DataContext, DataDescription, DataId, FuncId, Init, Linkage, ModuleError,
    ModuleNamespace, ModuleResult,
};
use faerie;
use failure::Error;
use std::fs::File;
use target_lexicon::Triple;

use gimli::write::Address;
use gimli::write::CallFrameInstruction;
use gimli::write::CommonInformationEntry;
use gimli::write::FrameDescriptionEntry;

#[derive(Debug)]
/// Setting to enable collection of traps. Setting this to `Enabled` in
/// `FaerieBuilder` means that a `FaerieTrapManifest` will be present
/// in the `FaerieProduct`.
pub enum FaerieTrapCollection {
    /// `FaerieProduct::trap_manifest` will be `None`
    Disabled,
    /// `FaerieProduct::trap_manifest` will be `Some`
    Enabled,
}

/// A builder for `FaerieBackend`.
pub struct FaerieBuilder {
    isa: Box<dyn TargetIsa>,
    name: String,
    collect_traps: FaerieTrapCollection,
    libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
}

impl FaerieBuilder {
    /// Create a new `FaerieBuilder` using the given Cranelift target, that
    /// can be passed to
    /// [`Module::new`](cranelift_module::Module::new)
    ///
    /// Faerie output requires that TargetIsa have PIC (Position Independent Code) enabled.
    ///
    /// `collect_traps` setting determines whether trap information is collected in a
    /// `FaerieTrapManifest` available in the `FaerieProduct`.
    ///
    /// The `libcall_names` function provides a way to translate `cranelift_codegen`'s `ir::LibCall`
    /// enum to symbols. LibCalls are inserted in the IR as part of the legalization for certain
    /// floating point instructions, and for stack probes. If you don't know what to use for this
    /// argument, use `cranelift_module::default_libcall_names()`.
    pub fn new(
        isa: Box<dyn TargetIsa>,
        name: String,
        collect_traps: FaerieTrapCollection,
        libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
    ) -> ModuleResult<Self> {
        if !isa.flags().is_pic() {
            return Err(ModuleError::Backend(
                "faerie requires TargetIsa be PIC".to_owned(),
            ));
        }
        Ok(Self {
            isa,
            name,
            collect_traps,
            libcall_names,
        })
    }
}

/// A `FaerieBackend` implements `Backend` and emits ".o" files using the `faerie` library.
///
/// See the `FaerieBuilder` for a convenient way to construct `FaerieBackend` instances.
pub struct FaerieBackend {
    isa: Box<dyn TargetIsa>,
    artifact: faerie::Artifact,
    trap_manifest: Option<FaerieTrapManifest>,
    frame_sink: Option<FrameSink>,
    libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
}

struct FrameSink {
    // we need to retain function names to hand out usize identifiers for FDE addresses,
    // which are then used to look up function names again for relocations, when `write_address` is
    // called.
    fn_names: Vec<String>,
    table: gimli::write::FrameTable,
    default_cie: gimli::write::CieId,
}

impl FrameSink {
    pub fn new() -> FrameSink {
        let mut table = gimli::write::FrameTable::default();
        let mut cie = CommonInformationEntry::new(
            gimli::Encoding {
                format: gimli::Format::Dwarf32,
                version: 1,
                address_size: 4,
            },
            // code alignment factor
            0x01,
            // data alignment factor
            -0x08,
            // ISA-specific, return address register
            gimli::Register(0x10),
        );

        cie.fde_address_encoding = gimli::DwEhPe(0x1b);

        cie.add_instruction(CallFrameInstruction::Cfa(gimli::Register(7), 8));
        cie.add_instruction(CallFrameInstruction::Offset(gimli::Register(0x10), -8));
        let cie_id = table.add_cie(cie);

        FrameSink {
            fn_names: vec![],
            table,
            default_cie: cie_id,
        }
    }

    pub fn address_for(&mut self, name: &str) -> Address {
        // adding a FrameDescriptionEntry for a function twice would be a bug,
        // so we can confidently expect that `name` will not be provided more than once.
        // So `name` is always new, meaning we can just add it and return its index
        self.fn_names.push(name.to_string());
        Address::Symbol {
            symbol: self.fn_names.len() - 1,
            addend: 0,
        }
    }

    /// Add a FrameDescriptionEntry to the FrameTable we're constructing
    ///
    /// This will always use the default CIE (which was build with this `FrameSink`).
    pub fn add_fde(&mut self, fd_entry: FrameDescriptionEntry) {
        self.table.add_fde(self.default_cie, fd_entry);
    }
}

struct FaerieDebugSink<'a> {
    pub data: &'a mut Vec<u8>,
    pub functions: &'a [String],
    pub artifact: &'a mut faerie::Artifact,
}

impl<'a> gimli::write::Writer for FaerieDebugSink<'a> {
    type Endian = gimli::LittleEndian;

    fn endian(&self) -> Self::Endian {
        gimli::LittleEndian
    }
    fn len(&self) -> usize {
        self.data.len()
    }
    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.data.extend_from_slice(bytes);
        Ok(())
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        if offset + bytes.len() > self.data.len() {
            return Err(gimli::write::Error::LengthOutOfBounds);
        }
        self.data[offset..][..bytes.len()].copy_from_slice(bytes);
        Ok(())
    }

    fn write_eh_pointer(
        &mut self,
        address: Address,
        eh_pe: gimli::DwEhPe,
        size: u8,
    ) -> gimli::write::Result<()> {
        // we only support PC-relative 4byte signed offsets for eh_frame pointers currently. Other
        // encodings may be permissible, but aren't seen even by gcc/clang/etc, and have not been
        // tested. Currently, relocations used for addresses expect to be relocating four bytes,
        // PC-relative, and larger pointer sizes would require selection of other relocation types.
        assert!(eh_pe.0 == 0x1b);

        // if size is not 4, then the size indicated by `eh_pe` doesn't match with the pointer
        // we're trying to encode. That's a logical bug, possibly in gimli?
        assert!(size == 4);

        self.write_address(address, size)
    }

    fn write_address(&mut self, address: Address, size: u8) -> gimli::write::Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                assert!(addend == 0);

                let name = self.functions[symbol].as_str();

                let reloc = faerie::artifact::Reloc::Raw {
                    reloc: goblin::elf::reloc::R_X86_64_PC32,
                    addend: 0,
                };

                self.artifact
                    .link_with(
                        faerie::Link {
                            to: name,
                            from: ".eh_frame",
                            at: self.data.len() as u64,
                        },
                        reloc,
                    )
                    .map_err(|_link_err| gimli::write::Error::InvalidAddress)?;

                self.write_udata(0, size)
            }
        }
    }
}

pub struct FaerieCompiledFunction {
    code_length: u32,
}

impl FaerieCompiledFunction {
    pub fn code_length(&self) -> u32 {
        self.code_length
    }
}

struct CFIEncoder {
    cfa_def_reg: Option<isa::RegUnit>,
    cfa_def_offset: Option<isize>,
}

struct DwarfRegMapper<'a> {
    isa: &'a Box<dyn TargetIsa>,
    reg_info: RegInfo,
}

impl<'a> DwarfRegMapper<'a> {
    pub fn for_isa(isa: &'a Box<dyn TargetIsa>) -> Self {
        DwarfRegMapper {
            isa,
            reg_info: isa.register_info(),
        }
    }

    /// Translate a Cranelift `RegUnit` to its matching `Register` for DWARF use.
    ///
    /// panics if `reg` cannot be translated - the requested debug information would be
    /// unencodable.
    pub fn translate_reg(&self, reg: RegUnit) -> gimli::Register {
        match self.isa.name() {
            "x86" => {
                const X86_GP_REG_MAP: [u16; 16] = [
                    // cranelift rax == 0 -> dwarf rax == 0
                    0, // cranelift rcx == 1 -> dwarf rcx == 2
                    2, // cranelift rdx == 2 -> dwarf rdx == 1
                    1, // cranelift rbx == 3 -> dwarf rbx == 3
                    3, // cranelift rsp == 4 -> dwarf rsp == 7
                    7, // cranelift rbp == 5 -> dwarf rbp == 6
                    6, // cranelift rsi == 6 -> dwarf rsi == 4
                    4, // cranelift rdi == 7 -> dwarf rdi == 5
                    5, // all of r8 to r15 do map directly over
                    8, 9, 10, 11, 12, 13, 14, 15,
                ];
                let bank = self.reg_info.bank_containing_regunit(reg).unwrap();
                match bank.name {
                    "IntRegs" => {
                        // x86 GP registers have a weird mapping to DWARF registers, so we use a
                        // lookup table.
                        gimli::Register(X86_GP_REG_MAP[(reg - bank.first_unit) as usize])
                    }
                    "FloatRegs" => {
                        // xmm registers are all contiguous, but a bit offset
                        let xmm_num = reg - bank.first_unit;
                        // Cranelift only knows about sse4
                        assert!(xmm_num < 16);
                        gimli::Register(17 + xmm_num)
                    }
                    _ => {
                        panic!("unsupported register bank: {}", bank.name);
                    }
                }
            }
            /*
             * Other architectures, like "arm32", "arm64", and "riscv", do not have mappings to
             * DWARF register numbers yet.
             */
            name => {
                panic!("don't know how to encode registers for isa {}", name);
            }
        }
    }

    /// Get the DWARF location describing the call frame's return address.
    ///
    /// panics if that location is unknown - the requested debug information would be unencodable.
    pub fn return_address(&self) -> gimli::Register {
        match self.isa.name() {
            "x86" => gimli::Register(0x10),
            "arm32" => {
                // unlike AArch64, there is no explicit DWARF number for a return address
                // so trying to encode a return address in arm32 is a logical error.
                panic!("arm32 DWARF has no distinct return address register - this is a FrameChange bug, or you may want to have specified lr (r14)");
            }
            "arm64" => {
                // from "DWARF for the ARM 64-bit architecture (AArch64)"
                //
                // this is actually the "current mode exception link register". ARM uses LR for
                // return address purposes and CFI directives to preserve the parent call frame
                // should have been performed by preserving LR.
                gimli::Register(33)
            }
            "riscv" => {
                // Taking a guess from reading
                // https://github.com/riscv/riscv-elf-psabi-doc/blob/master/riscv-elf.md#dwarf-register-numbers
                //
                // which says dwarf number 64 is the "Alternate Frame Return Column", talking about
                // use for unwinding from signal handlers, recording the address the signal handler
                // will return to.
                gimli::Register(64)
            }
            name => {
                panic!("don't know how to encode registers for isa {}", name);
            }
        }
    }
}

impl CFIEncoder {
    pub fn new() -> Self {
        CFIEncoder {
            // this is rsp, which is really ISA- and CIE-defined. another CIE could have rax as the
            // CFA-referencing register, as unlikely as it would be.
            cfa_def_reg: Some(4),
            // this should be -8, but same as above is ISA and CIE-defined. `None` as a default
            // means the first location to set it will set it to the correct offset.
            cfa_def_offset: None,
        }
    }

    pub fn encode(
        &mut self,
        fd_entry: &mut FrameDescriptionEntry,
        reg_map: &DwarfRegMapper,
        changes: impl Iterator<Item = (u32, ir::FrameLayoutChange)>,
    ) {
        for (addr, change) in changes {
            match change {
                ir::FrameLayoutChange::CallFrameAddressAt { reg, offset } => {
                    // if your call frame is more than 2gb, or -2gb.. sorry?
                    assert_eq!(
                        offset, offset as i32 as isize,
                        "call frame offset beyond i32 range"
                    );
                    match (
                        Some(reg) == self.cfa_def_reg,
                        Some(offset) == self.cfa_def_offset,
                    ) {
                        (true, true) => {
                            /*
                             * this "change" would change nothing, so we don't have to
                             * do anything.
                             */
                        }
                        (false, true) => {
                            // reg pointing to the call frame has changed
                            fd_entry.add_instruction(
                                addr,
                                CallFrameInstruction::CfaRegister(reg_map.translate_reg(reg)),
                            );
                        }
                        (true, false) => {
                            // the offset has changed, so emit CfaOffset
                            fd_entry.add_instruction(
                                addr,
                                CallFrameInstruction::CfaOffset(offset as i32),
                            );
                        }
                        (false, false) => {
                            // the register and cfa offset have changed, so update both
                            fd_entry.add_instruction(
                                addr,
                                CallFrameInstruction::Cfa(
                                    reg_map.translate_reg(reg),
                                    offset as i32,
                                ),
                            );
                        }
                    }
                    self.cfa_def_offset = Some(offset);
                    self.cfa_def_reg = Some(reg);
                }
                ir::FrameLayoutChange::RegAt { reg, cfa_offset } => {
                    fd_entry.add_instruction(
                        addr,
                        CallFrameInstruction::Offset(reg_map.translate_reg(reg), cfa_offset as i32),
                    );
                }
                ir::FrameLayoutChange::ReturnAddressAt { cfa_offset } => {
                    fd_entry.add_instruction(
                        addr,
                        CallFrameInstruction::Offset(reg_map.return_address(), cfa_offset as i32),
                    );
                }
                ir::FrameLayoutChange::Preserve => {
                    fd_entry.add_instruction(
                        addr,
                        CallFrameInstruction::RememberState,
                    );
                },
                ir::FrameLayoutChange::Restore => {
                    fd_entry.add_instruction(
                        addr,
                        CallFrameInstruction::RestoreState,
                    );
                },
            }
        }
    }
}

pub struct FaerieCompiledData {}

impl Backend for FaerieBackend {
    type Builder = FaerieBuilder;

    type CompiledFunction = FaerieCompiledFunction;
    type CompiledData = FaerieCompiledData;

    // There's no need to return individual artifacts; we're writing them into
    // the output file instead.
    type FinalizedFunction = ();
    type FinalizedData = ();

    /// The returned value here provides functions for emitting object files
    /// to memory and files.
    type Product = FaerieProduct;

    /// Create a new `FaerieBackend` using the given Cranelift target.
    fn new(builder: FaerieBuilder) -> Self {
        Self {
            artifact: faerie::Artifact::new(builder.isa.triple().clone(), builder.name),
            isa: builder.isa,
            trap_manifest: match builder.collect_traps {
                FaerieTrapCollection::Enabled => Some(FaerieTrapManifest::new()),
                FaerieTrapCollection::Disabled => None,
            },
            frame_sink: Some(FrameSink::new()),
            libcall_names: builder.libcall_names,
        }
    }

    fn isa(&self) -> &dyn TargetIsa {
        &*self.isa
    }

    fn declare_function(&mut self, _id: FuncId, name: &str, linkage: Linkage) {
        self.artifact
            .declare(name, translate_function_linkage(linkage))
            .expect("inconsistent declarations");
    }

    fn declare_data(
        &mut self,
        _id: DataId,
        name: &str,
        linkage: Linkage,
        writable: bool,
        align: Option<u8>,
    ) {
        self.artifact
            .declare(name, translate_data_linkage(linkage, writable, align))
            .expect("inconsistent declarations");
    }

    fn define_function(
        &mut self,
        _id: FuncId,
        name: &str,
        ctx: &cranelift_codegen::Context,
        namespace: &ModuleNamespace<Self>,
        total_size: u32,
    ) -> ModuleResult<FaerieCompiledFunction> {
        let mut code: Vec<u8> = vec![0; total_size as usize];
        // TODO: Replace this with FaerieStackmapSink once it is implemented.
        let mut stackmap_sink = NullStackmapSink {};

        // Non-lexical lifetimes would obviate the braces here.
        {
            let mut reloc_sink = FaerieRelocSink {
                triple: self.isa.triple().clone(),
                artifact: &mut self.artifact,
                name,
                namespace,
                libcall_names: &*self.libcall_names,
            };

            if let Some(ref mut trap_manifest) = self.trap_manifest {
                let mut trap_sink = FaerieTrapSink::new(name, total_size);
                unsafe {
                    ctx.emit_to_memory(
                        &*self.isa,
                        code.as_mut_ptr(),
                        &mut reloc_sink,
                        &mut trap_sink,
                        &mut stackmap_sink,
                    )
                };
                trap_manifest.add_sink(trap_sink);
            } else {
                let mut trap_sink = NullTrapSink {};
                unsafe {
                    ctx.emit_to_memory(
                        &*self.isa,
                        code.as_mut_ptr(),
                        &mut reloc_sink,
                        &mut trap_sink,
                        &mut stackmap_sink,
                    )
                };
            }
        }

        // because `define` will take ownership of code, this is our last chance
        let code_length = code.len() as u32;

        if let Some(ref mut frame_sink) = self.frame_sink {
            if let Some(layout) = ctx.func.frame_layout.as_ref() {
                let mut fd_entry =
                    FrameDescriptionEntry::new(frame_sink.address_for(name), code_length);

                let mut frame_changes = vec![];
                for ebb in ctx.func.layout.ebbs() {
                    for (offset, inst, size) in ctx.func.inst_offsets(ebb, &self.isa.encoding_info()) {
                        if let Some(changes) = layout.instructions.get(&inst) {
                            for change in changes.iter() {
                                frame_changes.push((offset + size, change.clone()));
                            }
                        }
                    }
                }

                frame_changes.sort_by(|a, b| a.0.cmp(&b.0));

                CFIEncoder::new().encode(
                    &mut fd_entry,
                    &DwarfRegMapper::for_isa(&self.isa),
                    frame_changes.into_iter(),
                );

                frame_sink.add_fde(fd_entry);
            } else {
                // we have a frame sink to write .eh_frames into, but are not collecting debug
                // information for at least the current function. This might be a bug in the code
                // using cranelift-faerie?
            }
        }

        self.artifact
            .define(name, code)
            .expect("inconsistent declaration");

        Ok(FaerieCompiledFunction { code_length })
    }

    fn define_data(
        &mut self,
        _id: DataId,
        name: &str,
        _writable: bool,
        _align: Option<u8>,
        data_ctx: &DataContext,
        namespace: &ModuleNamespace<Self>,
    ) -> ModuleResult<FaerieCompiledData> {
        let &DataDescription {
            ref init,
            ref function_decls,
            ref data_decls,
            ref function_relocs,
            ref data_relocs,
        } = data_ctx.description();

        let size = init.size();
        let mut bytes = Vec::with_capacity(size);
        match *init {
            Init::Uninitialized => {
                panic!("data is not initialized yet");
            }
            Init::Zeros { .. } => {
                bytes.resize(size, 0);
            }
            Init::Bytes { ref contents } => {
                bytes.extend_from_slice(contents);
            }
        }

        for &(offset, id) in function_relocs {
            let to = &namespace.get_function_decl(&function_decls[id]).name;
            self.artifact
                .link(faerie::Link {
                    from: name,
                    to,
                    at: u64::from(offset),
                })
                .map_err(|e| ModuleError::Backend(e.to_string()))?;
        }
        for &(offset, id, addend) in data_relocs {
            debug_assert_eq!(
                addend, 0,
                "faerie doesn't support addends in data section relocations yet"
            );
            let to = &namespace.get_data_decl(&data_decls[id]).name;
            self.artifact
                .link(faerie::Link {
                    from: name,
                    to,
                    at: u64::from(offset),
                })
                .map_err(|e| ModuleError::Backend(e.to_string()))?;
        }

        self.artifact
            .define(name, bytes)
            .expect("inconsistent declaration");
        Ok(FaerieCompiledData {})
    }

    fn write_data_funcaddr(
        &mut self,
        _data: &mut FaerieCompiledData,
        _offset: usize,
        _what: ir::FuncRef,
    ) {
        unimplemented!()
    }

    fn write_data_dataaddr(
        &mut self,
        _data: &mut FaerieCompiledData,
        _offset: usize,
        _what: ir::GlobalValue,
        _usize: binemit::Addend,
    ) {
        unimplemented!()
    }

    fn finalize_function(
        &mut self,
        _id: FuncId,
        _func: &FaerieCompiledFunction,
        _namespace: &ModuleNamespace<Self>,
    ) {
        // Nothing to do.
    }

    fn get_finalized_function(&self, _func: &FaerieCompiledFunction) {
        // Nothing to do.
    }

    fn finalize_data(
        &mut self,
        _id: DataId,
        _data: &FaerieCompiledData,
        _namespace: &ModuleNamespace<Self>,
    ) {
        // Nothing to do.
    }

    fn get_finalized_data(&self, _data: &FaerieCompiledData) {
        // Nothing to do.
    }

    fn publish(&mut self) {
        if let Some(ref mut frame_sink) = self.frame_sink {
            self.artifact
                .declare(
                    ".eh_frame",
                    faerie::Decl::section(faerie::SectionKind::Data),
                )
                .unwrap();

            let mut eh_frame_bytes = Vec::new();

            let mut eh_frame_writer = gimli::write::EhFrame(FaerieDebugSink {
                data: &mut eh_frame_bytes,
                functions: frame_sink.fn_names.as_slice(),
                artifact: &mut self.artifact,
            });
            frame_sink
                .table
                .write_eh_frame(&mut eh_frame_writer)
                .unwrap();

            self.artifact.define(".eh_frame", eh_frame_bytes).unwrap();
        }
    }

    fn finish(self) -> FaerieProduct {
        FaerieProduct {
            artifact: self.artifact,
            trap_manifest: self.trap_manifest,
        }
    }
}

/// This is the output of `Module`'s
/// [`finish`](../cranelift_module/struct.Module.html#method.finish) function.
/// It provides functions for writing out the object file to memory or a file.
pub struct FaerieProduct {
    /// Faerie artifact with all functions, data, and links from the module defined
    pub artifact: faerie::Artifact,
    /// Optional trap manifest. Contains `FaerieTrapManifest` when `FaerieBuilder.collect_traps` is
    /// set to `FaerieTrapCollection::Enabled`.
    pub trap_manifest: Option<FaerieTrapManifest>,
}

impl FaerieProduct {
    /// Return the name of the output file. This is the name passed into `new`.
    pub fn name(&self) -> &str {
        &self.artifact.name
    }

    /// Call `emit` on the faerie `Artifact`, producing bytes in memory.
    pub fn emit(&self) -> Result<Vec<u8>, Error> {
        self.artifact.emit()
    }

    /// Call `write` on the faerie `Artifact`, writing to a file.
    pub fn write(&self, sink: File) -> Result<(), Error> {
        self.artifact.write(sink)
    }
}

fn translate_function_linkage(linkage: Linkage) -> faerie::Decl {
    match linkage {
        Linkage::Import => faerie::Decl::function_import().into(),
        Linkage::Local => faerie::Decl::function().into(),
        Linkage::Export => faerie::Decl::function().global().into(),
        Linkage::Preemptible => faerie::Decl::function().weak().into(),
    }
}

fn translate_data_linkage(linkage: Linkage, writable: bool, align: Option<u8>) -> faerie::Decl {
    let align = align.map(u64::from);
    match linkage {
        Linkage::Import => faerie::Decl::data_import().into(),
        Linkage::Local => faerie::Decl::data()
            .with_writable(writable)
            .with_align(align)
            .into(),
        Linkage::Export => faerie::Decl::data()
            .global()
            .with_writable(writable)
            .with_align(align)
            .into(),
        Linkage::Preemptible => faerie::Decl::data()
            .weak()
            .with_writable(writable)
            .with_align(align)
            .into(),
    }
}

struct FaerieRelocSink<'a> {
    triple: Triple,
    artifact: &'a mut faerie::Artifact,
    name: &'a str,
    namespace: &'a ModuleNamespace<'a, FaerieBackend>,
    libcall_names: &'a dyn Fn(ir::LibCall) -> String,
}

impl<'a> RelocSink for FaerieRelocSink<'a> {
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
        let ref_name: String = match *name {
            ir::ExternalName::User { .. } => {
                if self.namespace.is_function(name) {
                    self.namespace.get_function_decl(name).name.clone()
                } else {
                    self.namespace.get_data_decl(name).name.clone()
                }
            }
            ir::ExternalName::LibCall(ref libcall) => {
                let sym = (self.libcall_names)(*libcall);
                self.artifact
                    .declare(sym.clone(), faerie::Decl::function_import())
                    .expect("faerie declaration of libcall");
                sym
            }
            _ => panic!("invalid ExternalName {}", name),
        };
        let (raw_reloc, raw_addend) = container::raw_relocation(reloc, &self.triple);
        // TODO: Handle overflow.
        let final_addend = addend + raw_addend;
        let addend_i32 = final_addend as i32;
        debug_assert!(i64::from(addend_i32) == final_addend);
        self.artifact
            .link_with(
                faerie::Link {
                    from: self.name,
                    to: &ref_name,
                    at: u64::from(offset),
                },
                faerie::Reloc::Raw {
                    reloc: raw_reloc,
                    addend: addend_i32,
                },
            )
            .expect("faerie relocation error");
    }

    fn reloc_jt(&mut self, _offset: CodeOffset, reloc: Reloc, _jt: ir::JumpTable) {
        match reloc {
            Reloc::X86PCRelRodata4 => {
                // Not necessary to record this unless we are going to split apart code and its
                // jumptbl/rodata.
            }
            _ => {
                panic!("Unhandled reloc");
            }
        }
    }

    fn reloc_constant(&mut self, _offset: CodeOffset, reloc: Reloc, _jt: ir::ConstantOffset) {
        match reloc {
            Reloc::X86PCRelRodata4 => {
                // Not necessary to record this unless we are going to split apart code and its
                // jumptbl/rodata.
            }
            _ => {
                panic!("Unhandled reloc");
            }
        }
    }
}

#[allow(dead_code)]
struct FaerieStackmapSink<'a> {
    artifact: &'a mut faerie::Artifact,
    namespace: &'a ModuleNamespace<'a, FaerieBackend>,
}

/// Faerie is currently not used in SpiderMonkey. Methods are unimplemented.
impl<'a> StackmapSink for FaerieStackmapSink<'a> {
    fn add_stackmap(&mut self, _: CodeOffset, _: Stackmap) {
        unimplemented!("faerie support for stackmaps");
    }
}
