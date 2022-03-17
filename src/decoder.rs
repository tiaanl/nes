use crate::instruction::{Instruction, Offset, Operand};

#[derive(Debug)]
pub struct DecodedInstruction {
    pub instruction: Instruction,
    pub cycles: u8,
}

pub fn decode_instruction(it: &mut impl Iterator<Item = u8>) -> Option<DecodedInstruction> {
    use data::AddressingMode;

    let op_code = it.next()?;

    let instruction_detail = &data::INSTRUCTIONS[op_code as usize];

    let operand = match instruction_detail.1 {
        AddressingMode::Implied => Operand::None,

        AddressingMode::Immediate => Operand::Immediate(it.next()?),

        AddressingMode::Zp0 => Operand::ZeroPage(it.next()?, Offset::None),

        AddressingMode::Zpx => Operand::ZeroPage(it.next()?, Offset::X),

        AddressingMode::Zpy => Operand::ZeroPage(it.next()?, Offset::Y),

        AddressingMode::Relative => Operand::Relative(it.next()? as i8),

        AddressingMode::Absolute => {
            Operand::Absolute(u16::from_le_bytes([it.next()?, it.next()?]), Offset::None)
        }

        AddressingMode::Abx => {
            Operand::Absolute(u16::from_le_bytes([it.next()?, it.next()?]), Offset::X)
        }

        AddressingMode::Aby => {
            Operand::Absolute(u16::from_le_bytes([it.next()?, it.next()?]), Offset::Y)
        }

        AddressingMode::Indirect => {
            Operand::Indirect(u16::from_le_bytes([it.next()?, it.next()?]), Offset::None)
        }

        AddressingMode::Izx => {
            Operand::Indirect(u16::from_le_bytes([it.next()?, it.next()?]), Offset::X)
        }

        AddressingMode::Izy => {
            Operand::Indirect(u16::from_le_bytes([it.next()?, it.next()?]), Offset::Y)
        }
    };

    Some(DecodedInstruction {
        instruction: Instruction {
            operation: instruction_detail.0,
            operand,
        },
        cycles: 2,
    })
}

pub mod data {
    use crate::instruction::Operation;

    #[derive(Clone, Copy, Debug)]
    #[repr(u8)]
    pub enum AddressingMode {
        Implied,
        Immediate,
        Zp0,
        Zpx,
        Zpy,
        Relative,
        Absolute,
        Abx,
        Aby,
        Indirect,
        Izx,
        Izy,
    }

    #[derive(Debug)]
    pub struct InstructionDetail(pub Operation, pub AddressingMode, pub u8);

    #[rustfmt::skip]
    pub const INSTRUCTIONS: [InstructionDetail; 256] = [
        /* 00 */ InstructionDetail(Operation::BRK, AddressingMode::Implied,   7), // BRK  impl
        /* 01 */ InstructionDetail(Operation::ORA, AddressingMode::Izx,       6), // ORA  X,ind
        /* 02 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 03 */ InstructionDetail(Operation::SLO, AddressingMode::Izx,       8), // SLO  X,ind
        /* 04 */ InstructionDetail(Operation::NOP, AddressingMode::Zp0,       3), // NOP  zpg
        /* 05 */ InstructionDetail(Operation::ORA, AddressingMode::Zp0,       3), // ORA  zpg
        /* 06 */ InstructionDetail(Operation::ASL, AddressingMode::Zp0,       5), // ASL  zpg
        /* 07 */ InstructionDetail(Operation::SLO, AddressingMode::Zp0,       5), // SLO  zpg
        /* 08 */ InstructionDetail(Operation::PHP, AddressingMode::Implied,   3), // PHP  impl
        /* 09 */ InstructionDetail(Operation::ORA, AddressingMode::Immediate, 2), // ORA  #
        /* 0A */ InstructionDetail(Operation::ASL, AddressingMode::Implied,   2), // ASL  A
        /* 0B */ InstructionDetail(Operation::ANC, AddressingMode::Immediate, 2), // ANC  #
        /* 0C */ InstructionDetail(Operation::NOP, AddressingMode::Absolute,  4), // NOP  abs
        /* 0D */ InstructionDetail(Operation::ORA, AddressingMode::Absolute,  4), // ORA  abs
        /* 0E */ InstructionDetail(Operation::ASL, AddressingMode::Absolute,  6), // ASL  abs
        /* 0F */ InstructionDetail(Operation::SLO, AddressingMode::Absolute,  6), // SLO  abs
        /* 10 */ InstructionDetail(Operation::BPL, AddressingMode::Relative,  2), // BPL  rel
        /* 11 */ InstructionDetail(Operation::ORA, AddressingMode::Izy,       5), // ORA  ind,Y
        /* 12 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 13 */ InstructionDetail(Operation::SLO, AddressingMode::Izy,       8), // SLO  ind,Y
        /* 14 */ InstructionDetail(Operation::NOP, AddressingMode::Zpx,       4), // NOP  zpg,X
        /* 15 */ InstructionDetail(Operation::ORA, AddressingMode::Zpx,       4), // ORA  zpg,X
        /* 16 */ InstructionDetail(Operation::ASL, AddressingMode::Zpx,       6), // ASL  zpg,X
        /* 17 */ InstructionDetail(Operation::SLO, AddressingMode::Zpx,       6), // SLO  zpg,X
        /* 18 */ InstructionDetail(Operation::CLC, AddressingMode::Implied,   2), // CLC  impl
        /* 19 */ InstructionDetail(Operation::ORA, AddressingMode::Aby,       4), // ORA  abs,Y
        /* 1A */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* 1B */ InstructionDetail(Operation::SLO, AddressingMode::Aby,       7), // SLO  abs,Y
        /* 1C */ InstructionDetail(Operation::NOP, AddressingMode::Abx,       4), // NOP  abs,X
        /* 1D */ InstructionDetail(Operation::ORA, AddressingMode::Abx,       4), // ORA  abs,X
        /* 1E */ InstructionDetail(Operation::ASL, AddressingMode::Abx,       7), // ASL  abs,X
        /* 1F */ InstructionDetail(Operation::SLO, AddressingMode::Abx,       7), // SLO  abs,X
        /* 20 */ InstructionDetail(Operation::JSR, AddressingMode::Absolute,  6), // JSR  abs
        /* 21 */ InstructionDetail(Operation::AND, AddressingMode::Izx,       6), // AND  X,ind
        /* 22 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 23 */ InstructionDetail(Operation::RLA, AddressingMode::Izx,       8), // RLA  X,ind
        /* 24 */ InstructionDetail(Operation::BIT, AddressingMode::Zp0,       3), // BIT  zpg
        /* 25 */ InstructionDetail(Operation::AND, AddressingMode::Zp0,       3), // AND  zpg
        /* 26 */ InstructionDetail(Operation::ROL, AddressingMode::Zp0,       5), // ROL  zpg
        /* 27 */ InstructionDetail(Operation::RLA, AddressingMode::Zp0,       5), // RLA  zpg
        /* 28 */ InstructionDetail(Operation::PLP, AddressingMode::Implied,   4), // PLP  impl
        /* 29 */ InstructionDetail(Operation::AND, AddressingMode::Immediate, 2), // AND  #
        /* 2A */ InstructionDetail(Operation::ROL, AddressingMode::Implied,   2), // ROL  A
        /* 2B */ InstructionDetail(Operation::ANC, AddressingMode::Immediate, 2), // ANC  #
        /* 2C */ InstructionDetail(Operation::BIT, AddressingMode::Absolute,  4), // BIT  abs
        /* 2D */ InstructionDetail(Operation::AND, AddressingMode::Absolute,  4), // AND  abs
        /* 2E */ InstructionDetail(Operation::ROL, AddressingMode::Absolute,  6), // ROL  abs
        /* 2F */ InstructionDetail(Operation::RLA, AddressingMode::Absolute,  6), // RLA  abs
        /* 30 */ InstructionDetail(Operation::BMI, AddressingMode::Relative,  2), // BMI  rel
        /* 31 */ InstructionDetail(Operation::AND, AddressingMode::Izy,       5), // AND  ind,Y
        /* 32 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 33 */ InstructionDetail(Operation::RLA, AddressingMode::Izy,       8), // RLA  ind,Y
        /* 34 */ InstructionDetail(Operation::NOP, AddressingMode::Zpx,       4), // NOP  zpg,X
        /* 35 */ InstructionDetail(Operation::AND, AddressingMode::Zpx,       4), // AND  zpg,X
        /* 36 */ InstructionDetail(Operation::ROL, AddressingMode::Zpx,       6), // ROL  zpg,X
        /* 37 */ InstructionDetail(Operation::RLA, AddressingMode::Zpx,       6), // RLA  zpg,X
        /* 38 */ InstructionDetail(Operation::SEC, AddressingMode::Implied,   2), // SEC  impl
        /* 39 */ InstructionDetail(Operation::AND, AddressingMode::Aby,       4), // AND  abs,Y
        /* 3A */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* 3B */ InstructionDetail(Operation::RLA, AddressingMode::Aby,       7), // RLA  abs,Y
        /* 3C */ InstructionDetail(Operation::NOP, AddressingMode::Abx,       4), // NOP  abs,X
        /* 3D */ InstructionDetail(Operation::AND, AddressingMode::Abx,       4), // AND  abs,X
        /* 3E */ InstructionDetail(Operation::ROL, AddressingMode::Abx,       7), // ROL  abs,X
        /* 3F */ InstructionDetail(Operation::RLA, AddressingMode::Abx,       7), // RLA  abs,X
        /* 40 */ InstructionDetail(Operation::RTI, AddressingMode::Implied,   6), // RTI  impl
        /* 41 */ InstructionDetail(Operation::EOR, AddressingMode::Izx,       6), // EOR  X,ind
        /* 42 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 43 */ InstructionDetail(Operation::SRE, AddressingMode::Izx,       8), // SRE  X,ind
        /* 44 */ InstructionDetail(Operation::NOP, AddressingMode::Zp0,       3), // NOP  zpg
        /* 45 */ InstructionDetail(Operation::EOR, AddressingMode::Zp0,       3), // EOR  zpg
        /* 46 */ InstructionDetail(Operation::LSR, AddressingMode::Zp0,       5), // LSR  zpg
        /* 47 */ InstructionDetail(Operation::SRE, AddressingMode::Zp0,       5), // SRE  zpg
        /* 48 */ InstructionDetail(Operation::PHA, AddressingMode::Implied,   3), // PHA  impl
        /* 49 */ InstructionDetail(Operation::EOR, AddressingMode::Immediate, 2), // EOR  #
        /* 4A */ InstructionDetail(Operation::LSR, AddressingMode::Implied,   2), // LSR  A
        /* 4B */ InstructionDetail(Operation::ALR, AddressingMode::Immediate, 2), // ALR  #
        /* 4C */ InstructionDetail(Operation::JMP, AddressingMode::Absolute,  3), // JMP  abs
        /* 4D */ InstructionDetail(Operation::EOR, AddressingMode::Absolute,  4), // EOR  abs
        /* 4E */ InstructionDetail(Operation::LSR, AddressingMode::Absolute,  6), // LSR  abs
        /* 4F */ InstructionDetail(Operation::SRE, AddressingMode::Absolute,  6), // SRE  abs
        /* 50 */ InstructionDetail(Operation::BVC, AddressingMode::Relative,  2), // BVC  rel
        /* 51 */ InstructionDetail(Operation::EOR, AddressingMode::Izy,       5), // EOR  ind,Y
        /* 52 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 53 */ InstructionDetail(Operation::SRE, AddressingMode::Izy,       8), // SRE  ind,Y
        /* 54 */ InstructionDetail(Operation::NOP, AddressingMode::Zpx,       4), // NOP  zpg,X
        /* 55 */ InstructionDetail(Operation::EOR, AddressingMode::Zpx,       4), // EOR  zpg,X
        /* 56 */ InstructionDetail(Operation::LSR, AddressingMode::Zpx,       6), // LSR  zpg,X
        /* 57 */ InstructionDetail(Operation::SRE, AddressingMode::Zpx,       6), // SRE  zpg,X
        /* 58 */ InstructionDetail(Operation::CLI, AddressingMode::Implied,   2), // CLI  impl
        /* 59 */ InstructionDetail(Operation::EOR, AddressingMode::Aby,       4), // EOR  abs,Y
        /* 5A */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* 5B */ InstructionDetail(Operation::SRE, AddressingMode::Aby,       7), // SRE  abs,Y
        /* 5C */ InstructionDetail(Operation::NOP, AddressingMode::Abx,       4), // NOP  abs,X
        /* 5D */ InstructionDetail(Operation::EOR, AddressingMode::Abx,       4), // EOR  abs,X
        /* 5E */ InstructionDetail(Operation::LSR, AddressingMode::Abx,       7), // LSR  abs,X
        /* 5F */ InstructionDetail(Operation::SRE, AddressingMode::Abx,       7), // SRE  abs,X
        /* 60 */ InstructionDetail(Operation::RTS, AddressingMode::Implied,   6), // RTS  impl
        /* 61 */ InstructionDetail(Operation::ADC, AddressingMode::Izx,       6), // ADC  X,ind
        /* 62 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 63 */ InstructionDetail(Operation::RRA, AddressingMode::Izx,       8), // RRA  X,ind
        /* 64 */ InstructionDetail(Operation::NOP, AddressingMode::Zp0,       3), // NOP  zpg
        /* 65 */ InstructionDetail(Operation::ADC, AddressingMode::Zp0,       3), // ADC  zpg
        /* 66 */ InstructionDetail(Operation::ROR, AddressingMode::Zp0,       5), // ROR  zpg
        /* 67 */ InstructionDetail(Operation::RRA, AddressingMode::Zp0,       5), // RRA  zpg
        /* 68 */ InstructionDetail(Operation::PLA, AddressingMode::Implied,   4), // PLA  impl
        /* 69 */ InstructionDetail(Operation::ADC, AddressingMode::Immediate, 2), // ADC  #
        /* 6A */ InstructionDetail(Operation::ROR, AddressingMode::Implied,   2), // ROR  A
        /* 6B */ InstructionDetail(Operation::ARR, AddressingMode::Immediate, 2), // ARR  #
        /* 6C */ InstructionDetail(Operation::JMP, AddressingMode::Indirect,  5), // JMP  ind
        /* 6D */ InstructionDetail(Operation::ADC, AddressingMode::Absolute,  4), // ADC  abs
        /* 6E */ InstructionDetail(Operation::ROR, AddressingMode::Absolute,  6), // ROR  abs
        /* 6F */ InstructionDetail(Operation::RRA, AddressingMode::Absolute,  6), // RRA  abs
        /* 70 */ InstructionDetail(Operation::BVS, AddressingMode::Relative,  2), // BVS  rel
        /* 71 */ InstructionDetail(Operation::ADC, AddressingMode::Izy,       5), // ADC  ind,Y
        /* 72 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 73 */ InstructionDetail(Operation::RRA, AddressingMode::Izy,       8), // RRA  ind,Y
        /* 74 */ InstructionDetail(Operation::NOP, AddressingMode::Zpx,       4), // NOP  zpg,X
        /* 75 */ InstructionDetail(Operation::ADC, AddressingMode::Zpx,       4), // ADC  zpg,X
        /* 76 */ InstructionDetail(Operation::ROR, AddressingMode::Zpx,       6), // ROR  zpg,X
        /* 77 */ InstructionDetail(Operation::RRA, AddressingMode::Zpx,       6), // RRA  zpg,X
        /* 78 */ InstructionDetail(Operation::SEI, AddressingMode::Implied,   2), // SEI  impl
        /* 79 */ InstructionDetail(Operation::ADC, AddressingMode::Aby,       4), // ADC  abs,Y
        /* 7A */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* 7B */ InstructionDetail(Operation::RRA, AddressingMode::Aby,       7), // RRA  abs,Y
        /* 7C */ InstructionDetail(Operation::NOP, AddressingMode::Abx,       4), // NOP  abs,X
        /* 7D */ InstructionDetail(Operation::ADC, AddressingMode::Abx,       4), // ADC  abs,X
        /* 7E */ InstructionDetail(Operation::ROR, AddressingMode::Abx,       7), // ROR  abs,X
        /* 7F */ InstructionDetail(Operation::RRA, AddressingMode::Abx,       7), // RRA  abs,X
        /* 80 */ InstructionDetail(Operation::NOP, AddressingMode::Immediate, 2), // NOP  #
        /* 81 */ InstructionDetail(Operation::STA, AddressingMode::Izx,       6), // STA  X,ind
        /* 82 */ InstructionDetail(Operation::NOP, AddressingMode::Immediate, 2), // NOP  #
        /* 83 */ InstructionDetail(Operation::SAX, AddressingMode::Izx,       6), // SAX  X,ind
        /* 84 */ InstructionDetail(Operation::STY, AddressingMode::Zp0,       3), // STY  zpg
        /* 85 */ InstructionDetail(Operation::STA, AddressingMode::Zp0,       3), // STA  zpg
        /* 86 */ InstructionDetail(Operation::STX, AddressingMode::Zp0,       3), // STX  zpg
        /* 87 */ InstructionDetail(Operation::SAX, AddressingMode::Zp0,       3), // SAX  zpg
        /* 88 */ InstructionDetail(Operation::DEY, AddressingMode::Implied,   2), // DEY  impl
        /* 89 */ InstructionDetail(Operation::NOP, AddressingMode::Immediate, 2), // NOP  #
        /* 8A */ InstructionDetail(Operation::TXA, AddressingMode::Implied,   2), // TXA  impl
        /* 8B */ InstructionDetail(Operation::ANE, AddressingMode::Immediate, 2), // ANE  #
        /* 8C */ InstructionDetail(Operation::STY, AddressingMode::Absolute,  4), // STY  abs
        /* 8D */ InstructionDetail(Operation::STA, AddressingMode::Absolute,  4), // STA  abs
        /* 8E */ InstructionDetail(Operation::STX, AddressingMode::Absolute,  4), // STX  abs
        /* 8F */ InstructionDetail(Operation::SAX, AddressingMode::Absolute,  4), // SAX  abs
        /* 90 */ InstructionDetail(Operation::BCC, AddressingMode::Relative,  2), // BCC  rel
        /* 91 */ InstructionDetail(Operation::STA, AddressingMode::Izy,       6), // STA  ind,Y
        /* 92 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* 93 */ InstructionDetail(Operation::SHA, AddressingMode::Izy,       6), // SHA  ind,Y
        /* 94 */ InstructionDetail(Operation::STY, AddressingMode::Zpx,       4), // STY  zpg,X
        /* 95 */ InstructionDetail(Operation::STA, AddressingMode::Zpx,       4), // STA  zpg,X
        /* 96 */ InstructionDetail(Operation::STX, AddressingMode::Zpy,       4), // STX  zpg,Y
        /* 97 */ InstructionDetail(Operation::SAX, AddressingMode::Zpy,       4), // SAX  zpg,Y
        /* 98 */ InstructionDetail(Operation::TYA, AddressingMode::Implied,   2), // TYA  impl
        /* 99 */ InstructionDetail(Operation::STA, AddressingMode::Aby,       5), // STA  abs,Y
        /* 9A */ InstructionDetail(Operation::TXS, AddressingMode::Implied,   2), // TXS  impl
        /* 9B */ InstructionDetail(Operation::TAS, AddressingMode::Aby,       5), // TAS  abs,Y
        /* 9C */ InstructionDetail(Operation::SHY, AddressingMode::Abx,       5), // SHY  abs,X
        /* 9D */ InstructionDetail(Operation::STA, AddressingMode::Abx,       5), // STA  abs,X
        /* 9E */ InstructionDetail(Operation::SHX, AddressingMode::Aby,       5), // SHX  abs,Y
        /* 9F */ InstructionDetail(Operation::SHA, AddressingMode::Aby,       5), // SHA  abs,Y
        /* A0 */ InstructionDetail(Operation::LDY, AddressingMode::Immediate, 2), // LDY  #
        /* A1 */ InstructionDetail(Operation::LDA, AddressingMode::Izx,       6), // LDA  X,ind
        /* A2 */ InstructionDetail(Operation::LDX, AddressingMode::Immediate, 2), // LDX  #
        /* A3 */ InstructionDetail(Operation::LAX, AddressingMode::Izx,       6), // LAX  X,ind
        /* A4 */ InstructionDetail(Operation::LDY, AddressingMode::Zp0,       3), // LDY  zpg
        /* A5 */ InstructionDetail(Operation::LDA, AddressingMode::Zp0,       3), // LDA  zpg
        /* A6 */ InstructionDetail(Operation::LDX, AddressingMode::Zp0,       3), // LDX  zpg
        /* A7 */ InstructionDetail(Operation::LAX, AddressingMode::Zp0,       3), // LAX  zpg
        /* A8 */ InstructionDetail(Operation::TAY, AddressingMode::Implied,   2), // TAY  impl
        /* A9 */ InstructionDetail(Operation::LDA, AddressingMode::Immediate, 2), // LDA  #
        /* AA */ InstructionDetail(Operation::TAX, AddressingMode::Implied,   2), // TAX  impl
        /* AB */ InstructionDetail(Operation::LXA, AddressingMode::Immediate, 2), // LXA  #
        /* AC */ InstructionDetail(Operation::LDY, AddressingMode::Absolute,  4), // LDY  abs
        /* AD */ InstructionDetail(Operation::LDA, AddressingMode::Absolute,  4), // LDA  abs
        /* AE */ InstructionDetail(Operation::LDX, AddressingMode::Absolute,  4), // LDX  abs
        /* AF */ InstructionDetail(Operation::LAX, AddressingMode::Absolute,  4), // LAX  abs
        /* B0 */ InstructionDetail(Operation::BCS, AddressingMode::Relative,  2), // BCS  rel
        /* B1 */ InstructionDetail(Operation::LDA, AddressingMode::Izy,       5), // LDA  ind,Y
        /* B2 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* B3 */ InstructionDetail(Operation::LAX, AddressingMode::Izy,       5), // LAX  ind,Y
        /* B4 */ InstructionDetail(Operation::LDY, AddressingMode::Zpx,       4), // LDY  zpg,X
        /* B5 */ InstructionDetail(Operation::LDA, AddressingMode::Zpx,       4), // LDA  zpg,X
        /* B6 */ InstructionDetail(Operation::LDX, AddressingMode::Zpy,       4), // LDX  zpg,Y
        /* B7 */ InstructionDetail(Operation::LAX, AddressingMode::Zpy,       4), // LAX  zpg,Y
        /* B8 */ InstructionDetail(Operation::CLV, AddressingMode::Implied,   2), // CLV  impl
        /* B9 */ InstructionDetail(Operation::LDA, AddressingMode::Aby,       4), // LDA  abs,Y
        /* BA */ InstructionDetail(Operation::TSX, AddressingMode::Implied,   2), // TSX  impl
        /* BB */ InstructionDetail(Operation::LAS, AddressingMode::Aby,       4), // LAS  abs,Y
        /* BC */ InstructionDetail(Operation::LDY, AddressingMode::Abx,       4), // LDY  abs,X
        /* BD */ InstructionDetail(Operation::LDA, AddressingMode::Abx,       4), // LDA  abs,X
        /* BE */ InstructionDetail(Operation::LDX, AddressingMode::Aby,       4), // LDX  abs,Y
        /* BF */ InstructionDetail(Operation::LAX, AddressingMode::Aby,       4), // LAX  abs,Y
        /* C0 */ InstructionDetail(Operation::CPY, AddressingMode::Immediate, 2), // CPY  #
        /* C1 */ InstructionDetail(Operation::CMP, AddressingMode::Izx,       6), // CMP  X,ind
        /* C2 */ InstructionDetail(Operation::NOP, AddressingMode::Immediate, 2), // NOP  #
        /* C3 */ InstructionDetail(Operation::DCP, AddressingMode::Izx,       8), // DCP  X,ind
        /* C4 */ InstructionDetail(Operation::CPY, AddressingMode::Zp0,       3), // CPY  zpg
        /* C5 */ InstructionDetail(Operation::CMP, AddressingMode::Zp0,       3), // CMP  zpg
        /* C6 */ InstructionDetail(Operation::DEC, AddressingMode::Zp0,       5), // DEC  zpg
        /* C7 */ InstructionDetail(Operation::DCP, AddressingMode::Zp0,       5), // DCP  zpg
        /* C8 */ InstructionDetail(Operation::INY, AddressingMode::Implied,   2), // INY  impl
        /* C9 */ InstructionDetail(Operation::CMP, AddressingMode::Immediate, 2), // CMP  #
        /* CA */ InstructionDetail(Operation::DEX, AddressingMode::Implied,   2), // DEX  impl
        /* CB */ InstructionDetail(Operation::SBX, AddressingMode::Immediate, 2), // SBX  #
        /* CC */ InstructionDetail(Operation::CPY, AddressingMode::Absolute,  4), // CPY  abs
        /* CD */ InstructionDetail(Operation::CMP, AddressingMode::Absolute,  4), // CMP  abs
        /* CE */ InstructionDetail(Operation::DEC, AddressingMode::Absolute,  6), // DEC  abs
        /* CF */ InstructionDetail(Operation::DCP, AddressingMode::Absolute,  6), // DCP  abs
        /* D0 */ InstructionDetail(Operation::BNE, AddressingMode::Relative,  2), // BNE  rel
        /* D1 */ InstructionDetail(Operation::CMP, AddressingMode::Izy,       5), // CMP  ind,Y
        /* D2 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* D3 */ InstructionDetail(Operation::DCP, AddressingMode::Izy,       8), // DCP  ind,Y
        /* D4 */ InstructionDetail(Operation::NOP, AddressingMode::Zpx,       4), // NOP  zpg,X
        /* D5 */ InstructionDetail(Operation::CMP, AddressingMode::Zpx,       4), // CMP  zpg,X
        /* D6 */ InstructionDetail(Operation::DEC, AddressingMode::Zpx,       6), // DEC  zpg,X
        /* D7 */ InstructionDetail(Operation::DCP, AddressingMode::Zpx,       6), // DCP  zpg,X
        /* D8 */ InstructionDetail(Operation::CLD, AddressingMode::Implied,   2), // CLD  impl
        /* D9 */ InstructionDetail(Operation::CMP, AddressingMode::Aby,       4), // CMP  abs,Y
        /* DA */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* DB */ InstructionDetail(Operation::DCP, AddressingMode::Aby,       7), // DCP  abs,Y
        /* DC */ InstructionDetail(Operation::NOP, AddressingMode::Abx,       4), // NOP  abs,X
        /* DD */ InstructionDetail(Operation::CMP, AddressingMode::Abx,       4), // CMP  abs,X
        /* DE */ InstructionDetail(Operation::DEC, AddressingMode::Abx,       7), // DEC  abs,X
        /* DF */ InstructionDetail(Operation::DCP, AddressingMode::Abx,       7), // DCP  abs,X
        /* E0 */ InstructionDetail(Operation::CPX, AddressingMode::Immediate, 2), // CPX  #
        /* E1 */ InstructionDetail(Operation::SBC, AddressingMode::Izx,       6), // SBC  X,ind
        /* E2 */ InstructionDetail(Operation::NOP, AddressingMode::Immediate, 2), // NOP  #
        /* E3 */ InstructionDetail(Operation::ISC, AddressingMode::Izx,       8), // ISC  X,ind
        /* E4 */ InstructionDetail(Operation::CPX, AddressingMode::Zp0,       3), // CPX  zpg
        /* E5 */ InstructionDetail(Operation::SBC, AddressingMode::Zp0,       3), // SBC  zpg
        /* E6 */ InstructionDetail(Operation::INC, AddressingMode::Zp0,       5), // INC  zpg
        /* E7 */ InstructionDetail(Operation::ISC, AddressingMode::Zp0,       5), // ISC  zpg
        /* E8 */ InstructionDetail(Operation::INX, AddressingMode::Implied,   2), // INX  impl
        /* E9 */ InstructionDetail(Operation::SBC, AddressingMode::Immediate, 2), // SBC  #
        /* EA */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* EB */ InstructionDetail(Operation::USB, AddressingMode::Immediate, 2), // USBC #
        /* EC */ InstructionDetail(Operation::CPX, AddressingMode::Absolute,  4), // CPX  abs
        /* ED */ InstructionDetail(Operation::SBC, AddressingMode::Absolute,  4), // SBC  abs
        /* EE */ InstructionDetail(Operation::INC, AddressingMode::Absolute,  6), // INC  abs
        /* EF */ InstructionDetail(Operation::ISC, AddressingMode::Absolute,  6), // ISC  abs
        /* F0 */ InstructionDetail(Operation::BEQ, AddressingMode::Relative,  2), // BEQ  rel
        /* F1 */ InstructionDetail(Operation::SBC, AddressingMode::Izy,       5), // SBC  ind,Y
        /* F2 */ InstructionDetail(Operation::JAM, AddressingMode::Implied,   2), // JAM
        /* F3 */ InstructionDetail(Operation::ISC, AddressingMode::Izy,       8), // ISC  ind,Y
        /* F4 */ InstructionDetail(Operation::NOP, AddressingMode::Zpx,       4), // NOP  zpg,X
        /* F5 */ InstructionDetail(Operation::SBC, AddressingMode::Zpx,       4), // SBC  zpg,X
        /* F6 */ InstructionDetail(Operation::INC, AddressingMode::Zpx,       6), // INC  zpg,X
        /* F7 */ InstructionDetail(Operation::ISC, AddressingMode::Zpx,       6), // ISC  zpg,X
        /* F8 */ InstructionDetail(Operation::SED, AddressingMode::Implied,   2), // SED  impl
        /* F9 */ InstructionDetail(Operation::SBC, AddressingMode::Aby,       4), // SBC  abs,Y
        /* FA */ InstructionDetail(Operation::NOP, AddressingMode::Implied,   2), // NOP  impl
        /* FB */ InstructionDetail(Operation::ISC, AddressingMode::Aby,       7), // ISC  abs,Y
        /* FC */ InstructionDetail(Operation::NOP, AddressingMode::Abx,       4), // NOP  abs,X
        /* FD */ InstructionDetail(Operation::SBC, AddressingMode::Abx,       4), // SBC  abs,X
        /* FE */ InstructionDetail(Operation::INC, AddressingMode::Abx,       7), // INC  abs,X
        /* FF */ InstructionDetail(Operation::ISC, AddressingMode::Abx,       7), // ISC  abs,X
    ];
}

#[cfg(test)]
mod tests {
    use super::decode_instruction;
    use crate::instruction::{Offset, Operand, Operation};

    #[test]
    fn test_decode_program() {
        let mut it = [
            0xA2, 0x0A, // LDX #10
            0x8E, 0x00, 0x00, // STX $0000
            0xA2, 0x03, // LDX #3
            0x8E, 0x01, 0x00, // STX $0001
            0xAC, 0x00, 0x00, // LDY $0000
            0xA9, 0x00, // LDA #0
            0x18, // CLC
            0x6D, 0x01, 0x00, // ADC $0001
            0x88, // DEY
            0xD0, 0xFA, // BNE loop
            0x8D, 0x02, 0x00,    // STA $0002
            0xEA,    // NOP
            0xEA,    // NOP
            0xEA_u8, // NOP
        ]
        .into_iter();

        // A2 0A
        let decoded = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::LDX, decoded.instruction.operation);
        assert_eq!(Operand::Immediate(0x0A), decoded.instruction.operand);

        // 8E 00 00
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::STX, instruction.instruction.operation);
        assert_eq!(
            Operand::Absolute(0x0000, Offset::None),
            instruction.instruction.operand
        );

        // A2 03
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::LDX, instruction.instruction.operation);
        assert_eq!(Operand::Immediate(0x03), instruction.instruction.operand);

        // 8E 01 00
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::STX, instruction.instruction.operation);
        assert_eq!(
            Operand::Absolute(0x0001, Offset::None),
            instruction.instruction.operand
        );

        // AC 00 00
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::LDY, instruction.instruction.operation);
        assert_eq!(
            Operand::Absolute(0x0000, Offset::None),
            instruction.instruction.operand
        );

        // A9 00
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::LDA, instruction.instruction.operation);
        assert_eq!(Operand::Immediate(0x00), instruction.instruction.operand);

        // 18
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::CLC, instruction.instruction.operation);
        assert_eq!(Operand::None, instruction.instruction.operand);

        // 6D 01 00
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::ADC, instruction.instruction.operation);
        assert_eq!(
            Operand::Absolute(0x0001, Offset::None),
            instruction.instruction.operand
        );

        // 88
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::DEY, instruction.instruction.operation);
        assert_eq!(Operand::None, instruction.instruction.operand);

        // D0 FA
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::BNE, instruction.instruction.operation);
        assert_eq!(Operand::Relative(-6), instruction.instruction.operand);

        // 8D 02 00
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::STA, instruction.instruction.operation);
        assert_eq!(
            Operand::Absolute(0x0002, Offset::None),
            instruction.instruction.operand
        );

        // EA
        let instruction = decode_instruction(&mut it).unwrap();
        assert_eq!(Operation::NOP, instruction.instruction.operation);
        assert_eq!(Operand::None, instruction.instruction.operand);
    }

    macro_rules! test_decode {
        ($bytes:expr,$operation:expr,$operand:expr) => {{
            let mut it = $bytes.into_iter();
            let decoded = decode_instruction(&mut it).unwrap();
            assert_eq!($operation, decoded.instruction.operation);
            assert_eq!($operand, decoded.instruction.operand);
        }};
    }

    #[test]
    #[rustfmt::skip]
    fn test_decode_instruction_op_codes() {
        test_decode!([0x00],             Operation::BRK, Operand::None);
        test_decode!([0x01, 0x34, 0x12], Operation::ORA, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x02],             Operation::JAM, Operand::None);
        test_decode!([0x03, 0x34, 0x12], Operation::SLO, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x04, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x05, 0x12],       Operation::ORA, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x06, 0x12],       Operation::ASL, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x07, 0x12],       Operation::SLO, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x08],             Operation::PHP, Operand::None);
        test_decode!([0x09, 0x34],       Operation::ORA, Operand::Immediate(0x34));
        test_decode!([0x0A],             Operation::ASL, Operand::None);
        test_decode!([0x0B, 0x34],       Operation::ANC, Operand::Immediate(0x34));
        test_decode!([0x0C, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x0D, 0x34, 0x12], Operation::ORA, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x0E, 0x34, 0x12], Operation::ASL, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x0F, 0x34, 0x12], Operation::SLO, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0x10, 0xFD],       Operation::BPL, Operand::Relative(-3));
        test_decode!([0x11, 0x34, 0x12], Operation::ORA, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x12],             Operation::JAM, Operand::None);
        test_decode!([0x13, 0x34, 0x12], Operation::SLO, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x14, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x15, 0x12],       Operation::ORA, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x16, 0x12],       Operation::ASL, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x17, 0x12],       Operation::SLO, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x18],             Operation::CLC, Operand::None);
        test_decode!([0x19, 0x34, 0x12], Operation::ORA, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x1A],             Operation::NOP, Operand::None);
        test_decode!([0x1B, 0x34, 0x12], Operation::SLO, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x1C, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x1D, 0x34, 0x12], Operation::ORA, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x1E, 0x34, 0x12], Operation::ASL, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x1F, 0x34, 0x12], Operation::SLO, Operand::Absolute(0x1234, Offset::X));

        test_decode!([0x20, 0x34, 0x12], Operation::JSR, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x21, 0x34, 0x12], Operation::AND, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x22],             Operation::JAM, Operand::None);
        test_decode!([0x23, 0x34, 0x12], Operation::RLA, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x24, 0x12],       Operation::BIT, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x25, 0x12],       Operation::AND, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x26, 0x12],       Operation::ROL, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x27, 0x12],       Operation::RLA, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x28],             Operation::PLP, Operand::None);
        test_decode!([0x29, 0x12],       Operation::AND, Operand::Immediate(0x12));
        test_decode!([0x2A],             Operation::ROL, Operand::None);
        test_decode!([0x2B, 0x12],       Operation::ANC, Operand::Immediate(0x12));
        test_decode!([0x2C, 0x34, 0x12], Operation::BIT, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x2D, 0x34, 0x12], Operation::AND, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x2E, 0x34, 0x12], Operation::ROL, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x2F, 0x34, 0x12], Operation::RLA, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0x30, 0xFD],       Operation::BMI, Operand::Relative(-3));
        test_decode!([0x31, 0x34, 0x12], Operation::AND, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x32],             Operation::JAM, Operand::None);
        test_decode!([0x33, 0x34, 0x12], Operation::RLA, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x34, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x35, 0x12],       Operation::AND, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x36, 0x12],       Operation::ROL, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x37, 0x12],       Operation::RLA, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x38],             Operation::SEC, Operand::None);
        test_decode!([0x39, 0x34, 0x12], Operation::AND, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x3A],             Operation::NOP, Operand::None);
        test_decode!([0x3B, 0x34, 0x12], Operation::RLA, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x3C, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x3D, 0x34, 0x12], Operation::AND, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x3E, 0x34, 0x12], Operation::ROL, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x3F, 0x34, 0x12], Operation::RLA, Operand::Absolute(0x1234, Offset::X));

        test_decode!([0x40],             Operation::RTI, Operand::None);
        test_decode!([0x41, 0x34, 0x12], Operation::EOR, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x42],             Operation::JAM, Operand::None);
        test_decode!([0x43, 0x34, 0x12], Operation::SRE, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x44, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x45, 0x12],       Operation::EOR, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x46, 0x12],       Operation::LSR, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x47, 0x12],       Operation::SRE, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x48],             Operation::PHA, Operand::None);
        test_decode!([0x49, 0x12],       Operation::EOR, Operand::Immediate(0x12));
        test_decode!([0x4A],             Operation::LSR, Operand::None);
        test_decode!([0x4B, 0x12],       Operation::ALR, Operand::Immediate(0x12));
        test_decode!([0x4C, 0x34, 0x12], Operation::JMP, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x4D, 0x34, 0x12], Operation::EOR, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x4E, 0x34, 0x12], Operation::LSR, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x4F, 0x34, 0x12], Operation::SRE, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0x50, 0xFD],       Operation::BVC, Operand::Relative(-3));
        test_decode!([0x51, 0x34, 0x12], Operation::EOR, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x52],             Operation::JAM, Operand::None);
        test_decode!([0x53, 0x34, 0x12], Operation::SRE, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x54, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x55, 0x12],       Operation::EOR, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x56, 0x12],       Operation::LSR, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x57, 0x12],       Operation::SRE, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x58],             Operation::CLI, Operand::None);
        test_decode!([0x59, 0x34, 0x12], Operation::EOR, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x5A],             Operation::NOP, Operand::None);
        test_decode!([0x5B, 0x34, 0x12], Operation::SRE, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x5C, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x5D, 0x34, 0x12], Operation::EOR, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x5E, 0x34, 0x12], Operation::LSR, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x5F, 0x34, 0x12], Operation::SRE, Operand::Absolute(0x1234, Offset::X));

        test_decode!([0x60],             Operation::RTS, Operand::None);
        test_decode!([0x61, 0x34, 0x12], Operation::ADC, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x62],             Operation::JAM, Operand::None);
        test_decode!([0x63, 0x34, 0x12], Operation::RRA, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x64, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x65, 0x12],       Operation::ADC, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x66, 0x12],       Operation::ROR, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x67, 0x12],       Operation::RRA, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x68],             Operation::PLA, Operand::None);
        test_decode!([0x69, 0x12],       Operation::ADC, Operand::Immediate(0x12));
        test_decode!([0x6A],             Operation::ROR, Operand::None);
        test_decode!([0x6B, 0x12],       Operation::ARR, Operand::Immediate(0x12));
        test_decode!([0x6C, 0x34, 0x12], Operation::JMP, Operand::Indirect(0x1234, Offset::None));
        test_decode!([0x6D, 0x34, 0x12], Operation::ADC, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x6E, 0x34, 0x12], Operation::ROR, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x6F, 0x34, 0x12], Operation::RRA, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0x70, 0xFD],       Operation::BVS, Operand::Relative(-3));
        test_decode!([0x71, 0x34, 0x12], Operation::ADC, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x72],             Operation::JAM, Operand::None);
        test_decode!([0x73, 0x34, 0x12], Operation::RRA, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x74, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x75, 0x12],       Operation::ADC, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x76, 0x12],       Operation::ROR, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x77, 0x12],       Operation::RRA, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x78],             Operation::SEI, Operand::None);
        test_decode!([0x79, 0x34, 0x12], Operation::ADC, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x7A],             Operation::NOP, Operand::None);
        test_decode!([0x7B, 0x34, 0x12], Operation::RRA, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x7C, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x7D, 0x34, 0x12], Operation::ADC, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x7E, 0x34, 0x12], Operation::ROR, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x7F, 0x34, 0x12], Operation::RRA, Operand::Absolute(0x1234, Offset::X));

        test_decode!([0x80, 0x12],       Operation::NOP, Operand::Immediate(0x12));
        test_decode!([0x81, 0x34, 0x12], Operation::STA, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x82, 0x12],       Operation::NOP, Operand::Immediate(0x12));
        test_decode!([0x83, 0x34, 0x12], Operation::SAX, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0x84, 0x12],       Operation::STY, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x85, 0x12],       Operation::STA, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x86, 0x12],       Operation::STX, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x87, 0x12],       Operation::SAX, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0x88],             Operation::DEY, Operand::None);
        test_decode!([0x89, 0x12],       Operation::NOP, Operand::Immediate(0x12));
        test_decode!([0x8A],             Operation::TXA, Operand::None);
        test_decode!([0x8B, 0x12],       Operation::ANE, Operand::Immediate(0x12));
        test_decode!([0x8C, 0x34, 0x12], Operation::STY, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x8D, 0x34, 0x12], Operation::STA, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x8E, 0x34, 0x12], Operation::STX, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0x8F, 0x34, 0x12], Operation::SAX, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0x90, 0xFD],       Operation::BCC, Operand::Relative(-3));
        test_decode!([0x91, 0x34, 0x12], Operation::STA, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x92],             Operation::JAM, Operand::None);
        test_decode!([0x93, 0x34, 0x12], Operation::SHA, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0x94, 0x12],       Operation::STY, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x95, 0x12],       Operation::STA, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0x96, 0x12],       Operation::STX, Operand::ZeroPage(0x12, Offset::Y));
        test_decode!([0x97, 0x12],       Operation::SAX, Operand::ZeroPage(0x12, Offset::Y));
        test_decode!([0x98],             Operation::TYA, Operand::None);
        test_decode!([0x99, 0x34, 0x12], Operation::STA, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x9A],             Operation::TXS, Operand::None);
        test_decode!([0x9B, 0x34, 0x12], Operation::TAS, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x9C, 0x34, 0x12], Operation::SHY, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x9D, 0x34, 0x12], Operation::STA, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0x9E, 0x34, 0x12], Operation::SHX, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0x9F, 0x34, 0x12], Operation::SHA, Operand::Absolute(0x1234, Offset::Y));

        test_decode!([0xA0, 0x12],       Operation::LDY, Operand::Immediate(0x12));
        test_decode!([0xA1, 0x34, 0x12], Operation::LDA, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0xA2, 0x12],       Operation::LDX, Operand::Immediate(0x12));
        test_decode!([0xA3, 0x34, 0x12], Operation::LAX, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0xA4, 0x12],       Operation::LDY, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xA5, 0x12],       Operation::LDA, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xA6, 0x12],       Operation::LDX, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xA7, 0x12],       Operation::LAX, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xA8],             Operation::TAY, Operand::None);
        test_decode!([0xA9, 0x12],       Operation::LDA, Operand::Immediate(0x12));
        test_decode!([0xAA],             Operation::TAX, Operand::None);
        test_decode!([0xAB, 0x12],       Operation::LXA, Operand::Immediate(0x12));
        test_decode!([0xAC, 0x34, 0x12], Operation::LDY, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xAD, 0x34, 0x12], Operation::LDA, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xAE, 0x34, 0x12], Operation::LDX, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xAF, 0x34, 0x12], Operation::LAX, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0xB0, 0xFD],       Operation::BCS, Operand::Relative(-3));
        test_decode!([0xB1, 0x34, 0x12], Operation::LDA, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0xB2],             Operation::JAM, Operand::None);
        test_decode!([0xB3, 0x34, 0x12], Operation::LAX, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0xB4, 0x12],       Operation::LDY, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xB5, 0x12],       Operation::LDA, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xB6, 0x12],       Operation::LDX, Operand::ZeroPage(0x12, Offset::Y));
        test_decode!([0xB7, 0x12],       Operation::LAX, Operand::ZeroPage(0x12, Offset::Y));
        test_decode!([0xB8],             Operation::CLV, Operand::None);
        test_decode!([0xB9, 0x34, 0x12], Operation::LDA, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xBA],             Operation::TSX, Operand::None);
        test_decode!([0xBB, 0x34, 0x12], Operation::LAS, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xBC, 0x34, 0x12], Operation::LDY, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xBD, 0x34, 0x12], Operation::LDA, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xBE, 0x34, 0x12], Operation::LDX, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xBF, 0x34, 0x12], Operation::LAX, Operand::Absolute(0x1234, Offset::Y));

        test_decode!([0xC0, 0x12],       Operation::CPY, Operand::Immediate(0x12));
        test_decode!([0xC1, 0x34, 0x12], Operation::CMP, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0xC2, 0x12],       Operation::NOP, Operand::Immediate(0x12));
        test_decode!([0xC3, 0x34, 0x12], Operation::DCP, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0xC4, 0x12],       Operation::CPY, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xC5, 0x12],       Operation::CMP, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xC6, 0x12],       Operation::DEC, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xC7, 0x12],       Operation::DCP, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xC8],             Operation::INY, Operand::None);
        test_decode!([0xC9, 0x12],       Operation::CMP, Operand::Immediate(0x12));
        test_decode!([0xCA],             Operation::DEX, Operand::None);
        test_decode!([0xCB, 0x12],       Operation::SBX, Operand::Immediate(0x12));
        test_decode!([0xCC, 0x34, 0x12], Operation::CPY, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xCD, 0x34, 0x12], Operation::CMP, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xCE, 0x34, 0x12], Operation::DEC, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xCF, 0x34, 0x12], Operation::DCP, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0xD0, 0xFD],       Operation::BNE, Operand::Relative(-3));
        test_decode!([0xD1, 0x34, 0x12], Operation::CMP, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0xD2],             Operation::JAM, Operand::None);
        test_decode!([0xD3, 0x34, 0x12], Operation::DCP, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0xD4, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xD5, 0x12],       Operation::CMP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xD6, 0x12],       Operation::DEC, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xD7, 0x12],       Operation::DCP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xD8],             Operation::CLD, Operand::None);
        test_decode!([0xD9, 0x34, 0x12], Operation::CMP, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xDA],             Operation::NOP, Operand::None);
        test_decode!([0xDB, 0x34, 0x12], Operation::DCP, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xDC, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xDD, 0x34, 0x12], Operation::CMP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xDE, 0x34, 0x12], Operation::DEC, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xDF, 0x34, 0x12], Operation::DCP, Operand::Absolute(0x1234, Offset::X));

        test_decode!([0xE0, 0x12],       Operation::CPX, Operand::Immediate(0x12));
        test_decode!([0xE1, 0x34, 0x12], Operation::SBC, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0xE2, 0x12],       Operation::NOP, Operand::Immediate(0x12));
        test_decode!([0xE3, 0x34, 0x12], Operation::ISC, Operand::Indirect(0x1234, Offset::X));
        test_decode!([0xE4, 0x12],       Operation::CPX, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xE5, 0x12],       Operation::SBC, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xE6, 0x12],       Operation::INC, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xE7, 0x12],       Operation::ISC, Operand::ZeroPage(0x12, Offset::None));
        test_decode!([0xE8],             Operation::INX, Operand::None);
        test_decode!([0xE9, 0x12],       Operation::SBC, Operand::Immediate(0x12));
        test_decode!([0xEA],             Operation::NOP, Operand::None);
        test_decode!([0xEB, 0x12],       Operation::USB, Operand::Immediate(0x12));
        test_decode!([0xEC, 0x34, 0x12], Operation::CPX, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xED, 0x34, 0x12], Operation::SBC, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xEE, 0x34, 0x12], Operation::INC, Operand::Absolute(0x1234, Offset::None));
        test_decode!([0xEF, 0x34, 0x12], Operation::ISC, Operand::Absolute(0x1234, Offset::None));

        test_decode!([0xF0, 0xFD],       Operation::BEQ, Operand::Relative(-3));
        test_decode!([0xF1, 0x34, 0x12], Operation::SBC, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0xF2],             Operation::JAM, Operand::None);
        test_decode!([0xF3, 0x34, 0x12], Operation::ISC, Operand::Indirect(0x1234, Offset::Y));
        test_decode!([0xF4, 0x12],       Operation::NOP, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xF5, 0x12],       Operation::SBC, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xF6, 0x12],       Operation::INC, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xF7, 0x12],       Operation::ISC, Operand::ZeroPage(0x12, Offset::X));
        test_decode!([0xF8],             Operation::SED, Operand::None);
        test_decode!([0xF9, 0x34, 0x12], Operation::SBC, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xFA],             Operation::NOP, Operand::None);
        test_decode!([0xFB, 0x34, 0x12], Operation::ISC, Operand::Absolute(0x1234, Offset::Y));
        test_decode!([0xFC, 0x34, 0x12], Operation::NOP, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xFD, 0x34, 0x12], Operation::SBC, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xFE, 0x34, 0x12], Operation::INC, Operand::Absolute(0x1234, Offset::X));
        test_decode!([0xFF, 0x34, 0x12], Operation::ISC, Operand::Absolute(0x1234, Offset::X));
    }
}
