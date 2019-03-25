use cfg_if::cfg_if;
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::{binemit, ir};

pub struct PrintRelocs {
    pub flag_print: bool,
}

impl binemit::RelocSink for PrintRelocs {
    fn reloc_ebb(
        &mut self,
        where_: binemit::CodeOffset,
        r: binemit::Reloc,
        offset: binemit::CodeOffset,
    ) {
        if self.flag_print {
            println!("reloc_ebb: {} {} at {}", r, offset, where_);
        }
    }

    fn reloc_external(
        &mut self,
        where_: binemit::CodeOffset,
        r: binemit::Reloc,
        name: &ir::ExternalName,
        addend: binemit::Addend,
    ) {
        if self.flag_print {
            println!("reloc_external: {} {} {} at {}", r, name, addend, where_);
        }
    }

    fn reloc_jt(&mut self, where_: binemit::CodeOffset, r: binemit::Reloc, jt: ir::JumpTable) {
        if self.flag_print {
            println!("reloc_jt: {} {} at {}", r, jt, where_);
        }
    }
}

pub struct PrintTraps {
    pub flag_print: bool,
}

impl binemit::TrapSink for PrintTraps {
    fn trap(&mut self, offset: binemit::CodeOffset, _srcloc: ir::SourceLoc, code: ir::TrapCode) {
        if self.flag_print {
            println!("trap: {} at {}", code, offset);
        }
    }
}

cfg_if! {
    if #[cfg(feature = "disas")] {
        use capstone::prelude::*;
        use target_lexicon::Architecture;
        use std::fmt::Write;

        fn get_disassembler(isa: &TargetIsa) -> Result<Capstone, String> {
            let cs = match isa.triple().architecture {
                Architecture::Riscv32 | Architecture::Riscv64 => {
                    return Err(String::from("No disassembler for RiscV"))
                }
                Architecture::I386 | Architecture::I586 | Architecture::I686 => Capstone::new()
                    .x86()
                    .mode(arch::x86::ArchMode::Mode32)
                    .build(),
                Architecture::X86_64 => Capstone::new()
                    .x86()
                    .mode(arch::x86::ArchMode::Mode64)
                    .build(),
                Architecture::Arm
                | Architecture::Armv4t
                | Architecture::Armv5te
                | Architecture::Armv7
                | Architecture::Armv7s => Capstone::new().arm().mode(arch::arm::ArchMode::Arm).build(),
                Architecture::Thumbv6m | Architecture::Thumbv7em | Architecture::Thumbv7m => Capstone::new(
                ).arm()
                    .mode(arch::arm::ArchMode::Thumb)
                    .build(),
                Architecture::Aarch64 => Capstone::new()
                    .arm64()
                    .mode(arch::arm64::ArchMode::Arm)
                    .build(),
                _ => return Err(String::from("Unknown ISA")),
            };

            cs.map_err(|err| err.to_string())
        }

        pub fn print_disassembly(isa: &TargetIsa, mem: &[u8]) -> Result<(), String> {
            let mut cs = get_disassembler(isa)?;

            println!("\nDisassembly of {} bytes:", mem.len());
            let insns = cs.disasm_all(&mem, 0x0).unwrap();
            for i in insns.iter() {
                let mut line = String::new();

                write!(&mut line, "{:4x}:\t", i.address()).unwrap();

                let mut bytes_str = String::new();
                let mut len = 0;
                let mut first = true;
                for b in i.bytes() {
                    write!(&mut bytes_str, "{:02x}", b).unwrap();
                    if !first {
                        write!(&mut bytes_str, " ").unwrap();
                    }
                    len += 1;
                    first = false;
                }
                write!(&mut line, "{:21}\t", bytes_str).unwrap();
                if len > 8 {
                    write!(&mut line, "\n\t\t\t\t").unwrap();
                }

                if let Some(s) = i.mnemonic() {
                    write!(&mut line, "{}\t", s).unwrap();
                }

                if let Some(s) = i.op_str() {
                    write!(&mut line, "{}", s).unwrap();
                }

                println!("{}", line);
            }
            Ok(())
        }
    } else {
        pub fn print_disassembly(_: &TargetIsa, _: &[u8]) -> Result<(), String> {
            println!("\nNo disassembly available.");
            Ok(())
        }
    }
}

pub fn print_bytes(mem: &[u8]) {
    print!(".byte ");
    let mut first = true;
    for byte in mem.iter() {
        if first {
            first = false;
        } else {
            print!(", ");
        }
        print!("{}", byte);
    }
    println!();
}

pub fn print_readonly_data(mem: &[u8]) {
    if mem.is_empty() {
        return;
    }

    println!("\nFollowed by {} bytes of read-only data:", mem.len());

    for (i, byte) in mem.iter().enumerate() {
        if i % 16 == 0 {
            if i != 0 {
                println!();
            }
            print!("{:4}: ", i);
        }
        if i % 4 == 0 {
            print!(" ");
        }
        print!("{:02x} ", byte);
    }
    println!();
}
