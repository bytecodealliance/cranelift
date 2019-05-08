//! Writing native DWARF sections.

use std::string::String;
use std::string::ToString;
use std::vec::Vec;

use gimli::write::{Address, Dwarf, EndianVec, Result, Sections, Writer};
use gimli::{RunTimeEndian, SectionId};

use faerie::artifact::Decl;
use faerie::*;
use std::result;

#[derive(Clone)]
struct DebugReloc {
    offset: u32,
    size: u8,
    name: String,
    addend: i64,
}

/// Address or relocation entry of a symbol.
pub enum ResolvedSymbol {
    /// Symbol is physical address (in file or memory).
    PhysicalAddress(u64),

    /// Symbol is relocation entry in relation to the symbol name.
    Reloc {
        /// Object file symbol.
        name: String,

        /// Offset from the object file symbol.
        addend: i64,
    },
}

/// Utility to resolve symbols into an address or relocation entry.
pub trait SymbolResolver {
    /// Resolves symbols using its index and addend/offset.
    fn resolve_symbol(&self, symbol: usize, addend: i64) -> ResolvedSymbol;
}

/// Emits DWARF sections into the faerie `Artifact`.
pub fn emit_dwarf(
    artifact: &mut Artifact,
    mut dwarf: Dwarf,
    symbol_resolver: &SymbolResolver,
) -> result::Result<(), failure::Error> {
    let endian = RunTimeEndian::Little;
    let mut sections = Sections::new(WriterRelocate::new(endian, symbol_resolver));

    let debug_str_offsets = dwarf.strings.write(&mut sections.debug_str).unwrap();
    let debug_line_str_offsets = dwarf
        .line_strings
        .write(&mut sections.debug_line_str)
        .unwrap();
    dwarf
        .units
        .write(&mut sections, &debug_line_str_offsets, &debug_str_offsets)
        .unwrap();

    sections.for_each_mut(|id, section| -> result::Result<(), failure::Error> {
        if section.writer.slice().is_empty() {
            return Ok(());
        }
        artifact.declare_with(id.name(), Decl::debug_section(), section.writer.take())
    })?;

    sections.for_each(|id, section| -> result::Result<(), failure::Error> {
        for reloc in &section.relocs {
            artifact.link_with(
                faerie::Link {
                    from: id.name(),
                    to: &reloc.name,
                    at: u64::from(reloc.offset),
                },
                faerie::Reloc::Debug {
                    size: reloc.size,
                    addend: reloc.addend as i32,
                },
            )?;
        }
        Ok(())
    })
}

#[derive(Clone)]
struct WriterRelocate<'a> {
    relocs: Vec<DebugReloc>,
    writer: EndianVec<RunTimeEndian>,
    symbol_resolver: &'a SymbolResolver,
}

impl<'a> WriterRelocate<'a> {
    fn new(endian: RunTimeEndian, symbol_resolver: &'a SymbolResolver) -> Self {
        WriterRelocate {
            relocs: Vec::new(),
            writer: EndianVec::new(endian),
            symbol_resolver,
        }
    }
}

impl<'a> Writer for WriterRelocate<'a> {
    type Endian = RunTimeEndian;

    fn endian(&self) -> Self::Endian {
        self.writer.endian()
    }

    fn len(&self) -> usize {
        self.writer.len()
    }

    fn write(&mut self, bytes: &[u8]) -> Result<()> {
        self.writer.write(bytes)
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> Result<()> {
        self.writer.write_at(offset, bytes)
    }

    fn write_address(&mut self, address: Address, size: u8) -> Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                match self.symbol_resolver.resolve_symbol(symbol, addend) {
                    ResolvedSymbol::PhysicalAddress(addr) => self.write_udata(addr, size),
                    ResolvedSymbol::Reloc { name, addend } => {
                        let offset = self.len() as u64;
                        self.relocs.push(DebugReloc {
                            offset: offset as u32,
                            size,
                            name,
                            addend,
                        });
                        self.write_udata(addend as u64, size)
                    }
                }
            }
        }
    }

    fn write_offset(&mut self, val: usize, section: SectionId, size: u8) -> Result<()> {
        let offset = self.len() as u32;
        let name = section.name().to_string();
        self.relocs.push(DebugReloc {
            offset,
            size,
            name,
            addend: val as i64,
        });
        self.write_udata(val as u64, size)
    }

    fn write_offset_at(
        &mut self,
        offset: usize,
        val: usize,
        section: SectionId,
        size: u8,
    ) -> Result<()> {
        let name = section.name().to_string();
        self.relocs.push(DebugReloc {
            offset: offset as u32,
            size,
            name,
            addend: val as i64,
        });
        self.write_udata_at(offset, val as u64, size)
    }
}
