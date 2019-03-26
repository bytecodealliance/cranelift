//! Writing native DWARF sections.

use crate::transform::TransformedDwarf;

use gimli::write::{Address, EndianVec, Result, SectionId, Sections, Writer};
use gimli::RunTimeEndian;

use faerie::artifact::Decl;
use faerie::*;

#[derive(Clone)]
struct DebugReloc {
    offset: u32,
    size: u8,
    name: String,
    addend: i64,
}

pub enum ResolvedSymbol {
    PhysicalAddress(u64),
    Reloc { name: String, addend: i64 },
}

pub trait SymbolResolver {
    fn resolve_symbol(&self, symbol: usize, addend: i64) -> ResolvedSymbol;
}

pub fn emit_dwarf(
    artifact: &mut Artifact,
    mut dwarf: TransformedDwarf,
    symbol_resolver: &SymbolResolver,
) {
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

    type SectionsFnResult = std::result::Result<(), failure::Error>;

    let result: SectionsFnResult = sections.for_each_mut(|id, section| {
        if section.writer.slice().is_empty() {
            return Ok(());
        }
        artifact.declare_with(id.name(), Decl::debug_section(), section.writer.take())
    });
    result.expect("no errors during sections declare_with");

    let result: SectionsFnResult = sections.for_each(|id, section| {
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
    });
    result.expect("no errors during sections link_with");
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
            Address::Absolute(val) => self.write_word(val, size),
            Address::Relative { symbol, addend } => {
                match self.symbol_resolver.resolve_symbol(symbol, addend as i64) {
                    ResolvedSymbol::PhysicalAddress(addr) => self.write_word(addr, size),
                    ResolvedSymbol::Reloc { name, addend } => {
                        let offset = self.len() as u64;
                        self.relocs.push(DebugReloc {
                            offset: offset as u32,
                            size,
                            name,
                            addend,
                        });
                        self.write_word(addend as u64, size)
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
        self.write_word(val as u64, size)
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
        self.write_word_at(offset, val as u64, size)
    }
}
