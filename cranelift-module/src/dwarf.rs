use cranelift_codegen::isa::{RegInfo, RegUnit, TargetIsa};
use cranelift_codegen::{ir, isa};

use std::boxed::Box;
use std::string::String;
use std::string::ToString;
use std::vec::Vec;

use gimli;
use gimli::constants::{DW_EH_PE_pcrel, DW_EH_PE_sdata4};
use gimli::write::Address;
use gimli::write::CallFrameInstruction;
use gimli::write::CommonInformationEntry;
use gimli::write::EhFrame;
use gimli::write::Error;
use gimli::write::FrameDescriptionEntry;

const PC_SDATA4: u8 = DW_EH_PE_pcrel.0 | DW_EH_PE_sdata4.0;

/// FrameSink maintains state for and an interface to construct DWARF frame information for a
/// cranelift-produced module.
pub struct FrameSink {
    // we need to retain function names to hand out usize identifiers for FDE addresses,
    // which are then used to look up function names again for relocations, when `write_address` is
    // called.
    fn_names: Vec<String>,
    table: gimli::write::FrameTable,
    reg_mapper: DwarfRegMapper,
}

impl FrameSink {
    /// Construct a new `FrameSink`.
    pub fn new(isa: &Box<dyn TargetIsa>) -> FrameSink {
        FrameSink {
            fn_names: vec![],
            table: gimli::write::FrameTable::default(),
            reg_mapper: DwarfRegMapper::for_isa(isa),
        }
    }

    /// Retrieve `gimli::write::Address` for some function name. Typically necessary in backends
    /// that need to retrieve symbolic addresses.
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

    /// Get the list of functions declared into this frame sink in order they were written. This
    /// order must be preserved for `gimli::Address` symbols to refer to the right functions.
    pub fn fn_names_slice(&self) -> &[String] {
        &self.fn_names
    }

    /// Write out data for this `FrameSink` through the provided `writer`.
    pub fn write_to<T: gimli::write::Writer>(&self, writer: &mut EhFrame<T>) -> Result<(), Error> {
        self.table.write_eh_frame(writer)
    }

    /// Find a CIE appropriate for the register mapper and initial state provided. This will
    /// construct a CIE and rely on `gimli` to return an id for an appropriate existing CIE, if
    /// one exists.
    ///
    /// This function also returns a `CFIEncoder` already initialized to the state matching the
    /// initial CFI instructions for this CIE, ready for use to encode an FDE.
    pub fn cie_for(
        &mut self,
        initial_state: &[ir::FrameLayoutChange],
    ) -> (gimli::write::CieId, CFIEncoder) {
        let mut cie = CommonInformationEntry::new(
            gimli::Encoding {
                format: gimli::Format::Dwarf32,
                version: 1,
                address_size: 4,
            },
            // Code alignment factor. Is this right for non-x86_64 ISAs? Probably could be 2 or 4
            // elsewhere.
            0x01,
            // Data alignment factor. Same question for non-x86_64 ISAs. -8 is a mostly-arbitrary
            // choice, selected here for equivalence with other DWARF-generating toolchains, such
            // as gcc and llvm.
            -0x08,
            // ISA-specific, column for the return address (may be a register, may not)
            self.reg_mapper.return_address(),
        );

        cie.fde_address_encoding = gimli::DwEhPe(PC_SDATA4);

        let mut encoder = CFIEncoder::new(&self.reg_mapper);

        for inst in initial_state
            .iter()
            .flat_map(|change| encoder.translate(change))
        {
            cie.add_instruction(inst);
        }

        let cie_id = self.table.add_cie(cie);

        (cie_id, encoder)
    }

    /// Add a FrameDescriptionEntry to the FrameTable we're constructing
    ///
    /// This will always use the default CIE (which was build with this `FrameSink`).
    pub fn add_fde(&mut self, cie_id: gimli::write::CieId, fd_entry: FrameDescriptionEntry) {
        self.table.add_fde(cie_id, fd_entry);
    }
}

/// `CFIEncoder` is used to translate from `FrameLayoutChange` to DWARF Call Frame Instructions
pub struct CFIEncoder {
    reg_map: DwarfRegMapper,
    cfa_def_reg: Option<isa::RegUnit>,
    cfa_def_offset: Option<isize>,
}

/// `DwarfRegMapper` maps cranelift `RegUnit` to DWARF-appropriate register numbers.
#[derive(Clone)]
struct DwarfRegMapper {
    isa_name: &'static str,
    reg_info: RegInfo,
}

impl DwarfRegMapper {
    fn for_isa(isa: &Box<dyn TargetIsa>) -> Self {
        DwarfRegMapper {
            isa_name: isa.name(),
            reg_info: isa.register_info(),
        }
    }

    /// Translate a Cranelift `RegUnit` to its matching `Register` for DWARF use.
    ///
    /// panics if `reg` cannot be translated - the requested debug information would be
    /// unencodable.
    pub fn translate_reg(&self, reg: RegUnit) -> gimli::Register {
        match self.isa_name {
            "x86" => {
                const X86_GP_REG_MAP: [gimli::Register; 16] = [
                    gimli::X86_64::RAX,
                    gimli::X86_64::RCX,
                    gimli::X86_64::RDX,
                    gimli::X86_64::RBX,
                    gimli::X86_64::RSP,
                    gimli::X86_64::RBP,
                    gimli::X86_64::RSI,
                    gimli::X86_64::RDI,
                    gimli::X86_64::R8,
                    gimli::X86_64::R9,
                    gimli::X86_64::R10,
                    gimli::X86_64::R11,
                    gimli::X86_64::R12,
                    gimli::X86_64::R13,
                    gimli::X86_64::R14,
                    gimli::X86_64::R15,
                ];
                const X86_XMM_REG_MAP: [gimli::Register; 16] = [
                    gimli::X86_64::XMM0,
                    gimli::X86_64::XMM1,
                    gimli::X86_64::XMM2,
                    gimli::X86_64::XMM3,
                    gimli::X86_64::XMM4,
                    gimli::X86_64::XMM5,
                    gimli::X86_64::XMM6,
                    gimli::X86_64::XMM7,
                    gimli::X86_64::XMM8,
                    gimli::X86_64::XMM9,
                    gimli::X86_64::XMM10,
                    gimli::X86_64::XMM11,
                    gimli::X86_64::XMM12,
                    gimli::X86_64::XMM13,
                    gimli::X86_64::XMM14,
                    gimli::X86_64::XMM15,
                ];
                let bank = self.reg_info.bank_containing_regunit(reg).unwrap();
                match bank.name {
                    "IntRegs" => {
                        // x86 GP registers have a weird mapping to DWARF registers, so we use a
                        // lookup table.
                        X86_GP_REG_MAP[(reg - bank.first_unit) as usize]
                    }
                    "FloatRegs" => X86_XMM_REG_MAP[(reg - bank.first_unit) as usize],
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
        match self.isa_name {
            "x86" => gimli::X86_64::RA,
            "arm32" => {
                panic!("don't know the DWARF register for arm32 return address");
            }
            "arm64" => {
                panic!("don't know the DWARF register for arm64 return address");
            }
            "riscv" => {
                panic!("don't know the DWARF register for riscv return address");
            }
            name => {
                panic!("don't know how to encode registers for isa {}", name);
            }
        }
    }
}

impl CFIEncoder {
    /// Construct a new `CFIEncoder`. `CFIEncoder` should not be reused across multiple functions.
    fn new(reg_map: &DwarfRegMapper) -> Self {
        CFIEncoder {
            reg_map: reg_map.clone(),
            // Both of the below are typically defined by per-CIE initial instructions, such that
            // neither are `None` when encoding instructions for an FDE. It is, however, likely not
            // an error for these to be `None` when encoding an FDE *AS LONG AS* they are
            // initialized before the CFI for an FDE advance into the function.
            cfa_def_reg: None,
            cfa_def_offset: None,
        }
    }

    /// Given the current `CFIEncoder` and a `FrameLayoutChange`, update the encoder to match the
    /// layout after this change and emit a corresponding `CallFrameInstruction` if one is needed.
    pub fn translate(&mut self, change: &ir::FrameLayoutChange) -> Option<CallFrameInstruction> {
        match change {
            ir::FrameLayoutChange::CallFrameAddressAt { reg, offset } => {
                // if your call frame is more than 2gb, or -2gb.. sorry? I don't think .eh_frame
                // can express that? Maybe chaining `cfa_advance_loc4`, or something..
                assert_eq!(
                    *offset, *offset as i32 as isize,
                    "call frame offset beyond i32 range"
                );
                let (reg_updated, offset_updated) = (
                    Some(*reg) != self.cfa_def_reg,
                    Some(*offset) != self.cfa_def_offset,
                );
                self.cfa_def_offset = Some(*offset);
                self.cfa_def_reg = Some(*reg);
                match (reg_updated, offset_updated) {
                    (false, false) => {
                        /*
                         * this "change" would change nothing, so we don't have to
                         * do anything.
                         */
                        None
                    }
                    (true, false) => {
                        // reg pointing to the call frame has changed
                        Some(CallFrameInstruction::CfaRegister(
                            self.reg_map.translate_reg(*reg),
                        ))
                    }
                    (false, true) => {
                        // the offset has changed, so emit CfaOffset
                        Some(CallFrameInstruction::CfaOffset(*offset as i32))
                    }
                    (true, true) => {
                        // the register and cfa offset have changed, so update both
                        Some(CallFrameInstruction::Cfa(
                            self.reg_map.translate_reg(*reg),
                            *offset as i32,
                        ))
                    }
                }
            }
            ir::FrameLayoutChange::Preserve => Some(CallFrameInstruction::RememberState),
            ir::FrameLayoutChange::Restore => Some(CallFrameInstruction::RestoreState),
            ir::FrameLayoutChange::RegAt { reg, cfa_offset } => Some(CallFrameInstruction::Offset(
                self.reg_map.translate_reg(*reg),
                *cfa_offset as i32,
            )),
            ir::FrameLayoutChange::ReturnAddressAt { cfa_offset } => Some(
                CallFrameInstruction::Offset(self.reg_map.return_address(), *cfa_offset as i32),
            ),
        }
    }
}
