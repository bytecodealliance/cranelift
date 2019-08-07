//! Defines `FaerieBackend`.

use crate::container;
use crate::traps::{FaerieTrapManifest, FaerieTrapSink};
use cranelift_codegen::binemit::{Addend, CodeOffset, NullTrapSink, Reloc, RelocSink};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::{self, binemit, ir};
use cranelift_module::{
    Backend, DataContext, DataDescription, Init, Linkage, ModuleError, ModuleNamespace,
    ModuleResult,
};
use faerie;
use failure::Error;
use std::fs::File;
use std::io::Cursor;
use target_lexicon::Triple;

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
    /// [`Module::new`](cranelift_module/struct.Module.html#method.new].
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
    eh_frame_data: Option<eh_frame::ExceptionFrameInfo>,
    libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
}

mod eh_frame {
    use std::io::Cursor;
    use byteorder::LittleEndian;
    use byteorder::WriteBytesExt;

    pub struct ExceptionFrameInfo {
        pub cie: CommonInformationEntry,
        pub fdes: Vec<FrameDescriptorEntry>,
    }

    impl ExceptionFrameInfo {
        pub fn new() -> Self {
            ExceptionFrameInfo {
                cie: CommonInformationEntry::v1(
                        // code alignment
                        0x01,
                        // data alignment
                        -0x08,
                        0x10, // x86-specific!!
                    ).with_augmentation(
                        // this sets that there _are_ augmentations,
                        // 0x1b may need to be enum'd -
                        // means DW_EH_PE_pcrel (relative to addr), encoded as
                        //  DW_EH_PE_sdata4 (4byte signed value)
                        CIEAugmentation::PointerEncoding(0x1b)
                    ).with_initial_instructions(
                        // now the initial CFE instructions to set up rows in the table
                        vec![0x0c, 0x07, 0x08, 0x90, 0x01]
                    ),
                fdes: vec![]
            }
        }
    }

    pub struct CommonInformationEntry {
        version: u8,
        code_alignment: u32,
        data_alignment: i32,
        return_register: u8,
        augmentations: Option<Vec<CIEAugmentation>>,
        initial_instructions: Vec<u8>,
    }

    impl CommonInformationEntry {
        pub fn v1(code_alignment: u32, data_alignment: i32, return_register: u8) -> Self {
            CommonInformationEntry {
                version: 1,
                code_alignment,
                data_alignment,
                return_register,
                augmentations: None,
                initial_instructions: vec![]
            }
        }
        pub fn with_augmentation(mut self, aug: CIEAugmentation) -> Self {
            if let Some(ref mut augmentations) = &mut self.augmentations {
                augmentations.push(aug);
            } else {
                self.augmentations = Some(vec![aug]);
            }
            self
        }
        pub fn with_initial_instructions(mut self, instructions: Vec<u8>) -> Self {
            self.initial_instructions = instructions;
            self
        }

        /*
        let cie = [
            0x14, 0x00, 0x00, 0x00, // size (after this field): 0x14
            0x00, 0x00, 0x00, 0x00, // identifies this as a CIE
            0x01, // CIE version
            0x7a, 0x52, 0x00, // augmentation string (zR)
                // 'z' means that there _is_ aumentation data
                //   including that there is an unsigned LEB128 for data length
                // 'R' means that data is an FDE encoding with a DW_EH_PE_xxx value in data
            0x01, // code alignment factor
            0x78, // data alignment factor (-8)
            0x10, // return register
            0x01, // augmentation data length
            0x1b, // FDE pointer encoding
            // and 0x00 to pad out to 0x10 alignment
        ];
        */
        pub fn encode_to(&self, data: &mut Cursor<Vec<u8>>) {
            let mut size =
                4 + // CIE identifier
                1 + // version
                1 + // augmentation string terminator
                3 + // code alignment, data alignment, and return register
                self.initial_instructions.len(); // and the initial CFI instructions

            let mut augmentation_data_len = 0;

            // count additional data for augmentations...
            if let Some(augmentations) = self.augmentations.as_ref() {
                // augmentation string is `z.*\x00`, but we already counted the
                // terminator unconditionally
                size += 1 + augmentations.len();

                augmentation_data_len = augmentations.iter().map(|x| x.data_len()).sum();

                size += 1; // augmentation data length field

                size += augmentation_data_len;
            }

            if size & 0x03 != 0 {
                // round up for padding
                size += 4 - (size & 0x03);
            }

            // size
            data.write_u32::<LittleEndian>(size as u32).unwrap();
            // CIE identifier
            data.write_u32::<LittleEndian>(0).unwrap();
            data.write_u8(self.version).unwrap();
            if let Some(augmentations) = self.augmentations.as_ref() {
                // write 'z' to begin an augmentation string
                data.write_u8(0x7a).unwrap();
                for augmentation in augmentations {
                    data.write_u8(augmentation.data_type());
                }
            }
            // augmentation strings are null-terminated (empty string is just null)
            data.write_u8(0x00).unwrap();
            data.write_u8(self.code_alignment as u8 & 0x7f).unwrap();
            data.write_u8(self.data_alignment as u8 & 0x7f).unwrap();
            data.write_u8(self.return_register).unwrap();
            if let Some(augmentations) = self.augmentations.as_ref() {
                data.write_u8(augmentation_data_len as u8).unwrap();
                for augmentation in augmentations {
                    augmentation.write_to(data);
                }
            }

            for inst in self.initial_instructions.iter() {
                data.write_u8(*inst);
            }

            // and pad out to an even 4-byte offset
            while data.position() & 0x3 != 0 {
                data.write_u8(0x00);
            }
        }
    }

    // Possible CIE augmentation data. The only supported kind currently is pointer encoding
    #[derive(Debug)]
    pub enum CIEAugmentation {
        PointerEncoding(u8),
    }

    impl CIEAugmentation {
        pub fn write_to(&self, data: &mut Cursor<Vec<u8>>) {
            match self {
                CIEAugmentation::PointerEncoding(encoding) => {
                    data.write_u8(*encoding);
                }
            }
        }

        pub fn data_type(&self) -> u8 {
            match self {
                CIEAugmentation::PointerEncoding(_) => {
                    0x52 // 'R'
                }
            }
        }

        pub fn data_len(&self) -> usize{
            match self {
                CIEAugmentation::PointerEncoding(_) => 1
            }
        }
    }

    pub struct FrameDescriptorEntry {
        pub function: String,
        augmentations: Option<Vec<FDEAugmentation>>,
        cfe_instructions: Vec<u8>,
        function_size: usize,
    }

    impl FrameDescriptorEntry {
        pub fn new(function: String, cfe_instructions: Vec<u8>, function_size: usize) -> Self {
            FrameDescriptorEntry {
                function,
                augmentations: Some(vec![]),
                cfe_instructions,
                function_size,
            }
        }
        /*
        let fde = [
            0x34, 0x00, 0x00, 0x00, // size (after this field)
            0xXX, 0xXX, 0xXX, 0xXX, // CIE pointer (negative, from this field)
            0xYY, 0xYY, 0xYY, 0xYY, // initial location (relative reloc to start of function)
            0xZZ, 0xZZ, 0xZZ, 0xZZ, // range length (size of function)
            0x00, // augmentation data length - no supported FDE augmentations so this will be 0.
            II, II, II, ... // CFE instructions (up to size)
        ]
        */
        pub fn encode_to(&self, data: &mut Cursor<Vec<u8>>, cie: &CommonInformationEntry) {
            let mut size =
                4 + // CIE pointer
                4 + // start of FDE range
                4;  // length of FDE range

            // from augmentation data being present
            // present if CIE augmentation string begins iwth 'a'
            if cie.augmentations.is_some() {
                size += 1;
            }

            size += self.cfe_instructions.len();

            if size & 0x03 != 0 {
                // round out to 4-byte aligned address
                size += 4 - (size & 0x03);
            }

            data.write_u32::<LittleEndian>(size as u32);
            // we can be kind of clever about the CIE pointer:
            // the CIE pointer is to get to the CIE from this FDE, but
            // is written as its negation (meaning, this field holds the
            // offset from CIE to DIE.cie_offset, rather than the reverse).
            // Since the CIE begins at byte 0, the correct value to write
            // here is `DIE.cie_offset - CIE_offset`, or `DIE.cie_offset - 0`
            // which is the current cursor position.
            //
            // the alternative is a relocation in faerie from a section to itself, which I think
            // might cause issues?
            data.write_u32::<LittleEndian>(data.position() as u32);
            // write 0 here where a relocation will point to the function later
            data.write_u32::<LittleEndian>(0x00000000);
            data.write_u32::<LittleEndian>(self.function_size as u32);

            // no frame descriptor entry augmentations are currently supported, so is always 0
            data.write_u8(0x00);
            for inst in self.cfe_instructions.iter() {
                data.write_u8(*inst);
            }
            while data.position() & 3 != 0 {
                data.write_u8(0x00);
            }
        }
    }

    /// The only FDE augmentation is to provide a pointer to
    /// a language-specific data area (LSDA), which we don't
    /// need to do and thus do not support (yet?).
    enum FDEAugmentation { }
}

pub struct FaerieCompiledFunction {
    code_length: u32,
}

impl FaerieCompiledFunction {
    pub fn code_length(&self) -> u32 {
        self.code_length
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
            eh_frame_data: Some(eh_frame::ExceptionFrameInfo::new()),
            libcall_names: builder.libcall_names,
        }
    }

    fn isa(&self) -> &dyn TargetIsa {
        &*self.isa
    }

    fn declare_function(&mut self, name: &str, linkage: Linkage) {
        self.artifact
            .declare(name, translate_function_linkage(linkage))
            .expect("inconsistent declarations");
    }

    fn declare_data(&mut self, name: &str, linkage: Linkage, writable: bool, align: Option<u8>) {
        self.artifact
            .declare(name, translate_data_linkage(linkage, writable, align))
            .expect("inconsistent declarations");
    }

    fn define_function(
        &mut self,
        name: &str,
        ctx: &cranelift_codegen::Context,
        namespace: &ModuleNamespace<Self>,
        total_size: u32,
    ) -> ModuleResult<FaerieCompiledFunction> {
        let mut code: Vec<u8> = vec![0; total_size as usize];

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
                    )
                };
            }
        }

        if let Some(ref mut eh_frame_data) = self.eh_frame_data {
            use byteorder::LittleEndian;
            use byteorder::WriteBytesExt;

            let mut cfi_instructions = vec![0x41, 0x0e, 0x10, 0x86, 0x02, 0x43, 0x0d, 0x06];
            let advance: u32 = code.len() as u32 - (4 + 1);
            cfi_instructions.push(0x04); // DW_CFA_advance_loc4
            cfi_instructions.write_u32::<LittleEndian>(advance);

            cfi_instructions.extend_from_slice(&[0x0c, 0x07, 0x08]);
            eh_frame_data.fdes.push(eh_frame::FrameDescriptorEntry::new(
                name.to_string(),
                cfi_instructions,
                code.len(),
            ));
        }

        // because `define` will take ownership of code, this is our last chance
        let code_length = code.len() as u32;

        self.artifact
            .define(name, code)
            .expect("inconsistent declaration");

        Ok(FaerieCompiledFunction { code_length })
    }

    fn define_data(
        &mut self,
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
        _func: &FaerieCompiledFunction,
        _namespace: &ModuleNamespace<Self>,
    ) {
        // Nothing to do.
    }

    fn get_finalized_function(&self, _func: &FaerieCompiledFunction) {
        // Nothing to do.
    }

    fn finalize_data(&mut self, _data: &FaerieCompiledData, _namespace: &ModuleNamespace<Self>) {
        // Nothing to do.
    }

    fn get_finalized_data(&self, _data: &FaerieCompiledData) {
        // Nothing to do.
    }

    fn publish(&mut self) {
        if let Some(ref mut eh_frame_data) = self.eh_frame_data {
            self.artifact
                .declare(".eh_frame", faerie::Decl::section(faerie::SectionKind::Data)).unwrap();

            let mut eh_frame_bytes = Cursor::new(Vec::new());

            eh_frame_data.cie.encode_to(&mut eh_frame_bytes);

            for fde in eh_frame_data.fdes.iter() {
                // Faerie requires all function references go through a PLT entry by default,
                // but we need a direct offset to the function, so explicitly construct an absolute
                // relocation and pass that. Because it's a reloc to an internal function, `ld`
                // should turn this into a const offset and discard the relocation.
                let absolute_reloc = match self.artifact.target.binary_format {
                    target_lexicon::BinaryFormat::Elf => faerie::artifact::Reloc::Raw {
                        reloc: goblin::elf::reloc::R_X86_64_PC32,
                        addend: 0,
                    },
                    target_lexicon::BinaryFormat::Macho => faerie::artifact::Reloc::Raw {
                        // TODO: how do we get a 32bit relocaion here, instead of 64?
                        reloc: goblin::mach::relocation::X86_64_RELOC_UNSIGNED as u32,
                        addend: 0,
                    },
                    _ => panic!("unsupported target format"),
                };
                self.artifact.link_with(
                    faerie::Link {
                        to: &fde.function,
                        from: ".eh_frame",
                        at: eh_frame_bytes.position() + 8,
                    },
                    absolute_reloc
                );

                fde.encode_to(&mut eh_frame_bytes, &eh_frame_data.cie);
            }

            self.artifact
                .define(".eh_frame", eh_frame_bytes.into_inner()).unwrap();
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
    let align = align.map(|align| usize::from(align));
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
}
