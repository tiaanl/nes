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

        AddressingMode::Indirect => Operand::Indirect(u16::from_le_bytes([it.next()?, it.next()?])),

        AddressingMode::Izx => Operand::IndirectZeroPage(it.next()?, Offset::X),

        AddressingMode::Izy => Operand::IndirectZeroPage(it.next()?, Offset::Y),
    };

    Some(DecodedInstruction {
        instruction: if instruction_detail.3 {
            Instruction::new(instruction_detail.0, operand)
        } else {
            Instruction::new_illegal(instruction_detail.0, operand)
        },
        cycles: instruction_detail.2,
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
    pub struct InstructionDetail(pub Operation, pub AddressingMode, pub u8, pub bool);

    const fn id(
        operation: Operation,
        addressing_mode: AddressingMode,
        cycles_required: u8,
        legal: bool,
    ) -> InstructionDetail {
        InstructionDetail(operation, addressing_mode, cycles_required, legal)
    }

    #[rustfmt::skip]
    pub const INSTRUCTIONS: [InstructionDetail; 256] = [
        /* 00 */ id(Operation::BRK, AddressingMode::Implied,   7, true ), // BRK  impl
        /* 01 */ id(Operation::ORA, AddressingMode::Izx,       6, true ), // ORA  X,ind
        /* 02 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 03 */ id(Operation::SLO, AddressingMode::Izx,       8, false), // SLO  X,ind
        /* 04 */ id(Operation::NOP, AddressingMode::Zp0,       3, false), // NOP  zpg
        /* 05 */ id(Operation::ORA, AddressingMode::Zp0,       3, true ), // ORA  zpg
        /* 06 */ id(Operation::ASL, AddressingMode::Zp0,       5, true ), // ASL  zpg
        /* 07 */ id(Operation::SLO, AddressingMode::Zp0,       5, false), // SLO  zpg
        /* 08 */ id(Operation::PHP, AddressingMode::Implied,   3, true ), // PHP  impl
        /* 09 */ id(Operation::ORA, AddressingMode::Immediate, 2, true ), // ORA  #
        /* 0A */ id(Operation::ASL, AddressingMode::Implied,   2, true ), // ASL  A
        /* 0B */ id(Operation::ANC, AddressingMode::Immediate, 2, false), // ANC  #
        /* 0C */ id(Operation::NOP, AddressingMode::Absolute,  4, false), // NOP  abs
        /* 0D */ id(Operation::ORA, AddressingMode::Absolute,  4, true ), // ORA  abs
        /* 0E */ id(Operation::ASL, AddressingMode::Absolute,  6, true ), // ASL  abs
        /* 0F */ id(Operation::SLO, AddressingMode::Absolute,  6, false), // SLO  abs
        /* 10 */ id(Operation::BPL, AddressingMode::Relative,  2, true ), // BPL  rel
        /* 11 */ id(Operation::ORA, AddressingMode::Izy,       5, true ), // ORA  ind,Y
        /* 12 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 13 */ id(Operation::SLO, AddressingMode::Izy,       8, false), // SLO  ind,Y
        /* 14 */ id(Operation::NOP, AddressingMode::Zpx,       4, false), // NOP  zpg,X
        /* 15 */ id(Operation::ORA, AddressingMode::Zpx,       4, true ), // ORA  zpg,X
        /* 16 */ id(Operation::ASL, AddressingMode::Zpx,       6, true ), // ASL  zpg,X
        /* 17 */ id(Operation::SLO, AddressingMode::Zpx,       6, false), // SLO  zpg,X
        /* 18 */ id(Operation::CLC, AddressingMode::Implied,   2, true ), // CLC  impl
        /* 19 */ id(Operation::ORA, AddressingMode::Aby,       4, true ), // ORA  abs,Y
        /* 1A */ id(Operation::NOP, AddressingMode::Implied,   2, false), // NOP  impl
        /* 1B */ id(Operation::SLO, AddressingMode::Aby,       7, false), // SLO  abs,Y
        /* 1C */ id(Operation::NOP, AddressingMode::Abx,       4, false), // NOP  abs,X
        /* 1D */ id(Operation::ORA, AddressingMode::Abx,       4, true ), // ORA  abs,X
        /* 1E */ id(Operation::ASL, AddressingMode::Abx,       7, true ), // ASL  abs,X
        /* 1F */ id(Operation::SLO, AddressingMode::Abx,       7, false), // SLO  abs,X
        /* 20 */ id(Operation::JSR, AddressingMode::Absolute,  6, true ), // JSR  abs
        /* 21 */ id(Operation::AND, AddressingMode::Izx,       6, true ), // AND  X,ind
        /* 22 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 23 */ id(Operation::RLA, AddressingMode::Izx,       8, false), // RLA  X,ind
        /* 24 */ id(Operation::BIT, AddressingMode::Zp0,       3, true ), // BIT  zpg
        /* 25 */ id(Operation::AND, AddressingMode::Zp0,       3, true ), // AND  zpg
        /* 26 */ id(Operation::ROL, AddressingMode::Zp0,       5, true ), // ROL  zpg
        /* 27 */ id(Operation::RLA, AddressingMode::Zp0,       5, false), // RLA  zpg
        /* 28 */ id(Operation::PLP, AddressingMode::Implied,   4, true ), // PLP  impl
        /* 29 */ id(Operation::AND, AddressingMode::Immediate, 2, true ), // AND  #
        /* 2A */ id(Operation::ROL, AddressingMode::Implied,   2, true ), // ROL  A
        /* 2B */ id(Operation::ANC, AddressingMode::Immediate, 2, false), // ANC  #
        /* 2C */ id(Operation::BIT, AddressingMode::Absolute,  4, true ), // BIT  abs
        /* 2D */ id(Operation::AND, AddressingMode::Absolute,  4, true ), // AND  abs
        /* 2E */ id(Operation::ROL, AddressingMode::Absolute,  6, true ), // ROL  abs
        /* 2F */ id(Operation::RLA, AddressingMode::Absolute,  6, false), // RLA  abs
        /* 30 */ id(Operation::BMI, AddressingMode::Relative,  2, true ), // BMI  rel
        /* 31 */ id(Operation::AND, AddressingMode::Izy,       5, true ), // AND  ind,Y
        /* 32 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 33 */ id(Operation::RLA, AddressingMode::Izy,       8, false), // RLA  ind,Y
        /* 34 */ id(Operation::NOP, AddressingMode::Zpx,       4, false), // NOP  zpg,X
        /* 35 */ id(Operation::AND, AddressingMode::Zpx,       4, true ), // AND  zpg,X
        /* 36 */ id(Operation::ROL, AddressingMode::Zpx,       6, true ), // ROL  zpg,X
        /* 37 */ id(Operation::RLA, AddressingMode::Zpx,       6, false), // RLA  zpg,X
        /* 38 */ id(Operation::SEC, AddressingMode::Implied,   2, true ), // SEC  impl
        /* 39 */ id(Operation::AND, AddressingMode::Aby,       4, true ), // AND  abs,Y
        /* 3A */ id(Operation::NOP, AddressingMode::Implied,   2, false), // NOP  impl
        /* 3B */ id(Operation::RLA, AddressingMode::Aby,       7, false), // RLA  abs,Y
        /* 3C */ id(Operation::NOP, AddressingMode::Abx,       4, false), // NOP  abs,X
        /* 3D */ id(Operation::AND, AddressingMode::Abx,       4, true ), // AND  abs,X
        /* 3E */ id(Operation::ROL, AddressingMode::Abx,       7, true ), // ROL  abs,X
        /* 3F */ id(Operation::RLA, AddressingMode::Abx,       7, false), // RLA  abs,X
        /* 40 */ id(Operation::RTI, AddressingMode::Implied,   6, true ), // RTI  impl
        /* 41 */ id(Operation::EOR, AddressingMode::Izx,       6, true ), // EOR  X,ind
        /* 42 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 43 */ id(Operation::SRE, AddressingMode::Izx,       8, false), // SRE  X,ind
        /* 44 */ id(Operation::NOP, AddressingMode::Zp0,       3, false), // NOP  zpg
        /* 45 */ id(Operation::EOR, AddressingMode::Zp0,       3, true ), // EOR  zpg
        /* 46 */ id(Operation::LSR, AddressingMode::Zp0,       5, true ), // LSR  zpg
        /* 47 */ id(Operation::SRE, AddressingMode::Zp0,       5, false), // SRE  zpg
        /* 48 */ id(Operation::PHA, AddressingMode::Implied,   3, true ), // PHA  impl
        /* 49 */ id(Operation::EOR, AddressingMode::Immediate, 2, true ), // EOR  #
        /* 4A */ id(Operation::LSR, AddressingMode::Implied,   2, true ), // LSR  A
        /* 4B */ id(Operation::ALR, AddressingMode::Immediate, 2, false), // ALR  #
        /* 4C */ id(Operation::JMP, AddressingMode::Absolute,  3, true ), // JMP  abs
        /* 4D */ id(Operation::EOR, AddressingMode::Absolute,  4, true ), // EOR  abs
        /* 4E */ id(Operation::LSR, AddressingMode::Absolute,  6, true ), // LSR  abs
        /* 4F */ id(Operation::SRE, AddressingMode::Absolute,  6, false), // SRE  abs
        /* 50 */ id(Operation::BVC, AddressingMode::Relative,  2, true ), // BVC  rel
        /* 51 */ id(Operation::EOR, AddressingMode::Izy,       5, true ), // EOR  ind,Y
        /* 52 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 53 */ id(Operation::SRE, AddressingMode::Izy,       8, false), // SRE  ind,Y
        /* 54 */ id(Operation::NOP, AddressingMode::Zpx,       4, false), // NOP  zpg,X
        /* 55 */ id(Operation::EOR, AddressingMode::Zpx,       4, true ), // EOR  zpg,X
        /* 56 */ id(Operation::LSR, AddressingMode::Zpx,       6, true ), // LSR  zpg,X
        /* 57 */ id(Operation::SRE, AddressingMode::Zpx,       6, false), // SRE  zpg,X
        /* 58 */ id(Operation::CLI, AddressingMode::Implied,   2, true ), // CLI  impl
        /* 59 */ id(Operation::EOR, AddressingMode::Aby,       4, true ), // EOR  abs,Y
        /* 5A */ id(Operation::NOP, AddressingMode::Implied,   2, false), // NOP  impl
        /* 5B */ id(Operation::SRE, AddressingMode::Aby,       7, false), // SRE  abs,Y
        /* 5C */ id(Operation::NOP, AddressingMode::Abx,       4, false), // NOP  abs,X
        /* 5D */ id(Operation::EOR, AddressingMode::Abx,       4, true ), // EOR  abs,X
        /* 5E */ id(Operation::LSR, AddressingMode::Abx,       7, true ), // LSR  abs,X
        /* 5F */ id(Operation::SRE, AddressingMode::Abx,       7, false), // SRE  abs,X
        /* 60 */ id(Operation::RTS, AddressingMode::Implied,   6, true ), // RTS  impl
        /* 61 */ id(Operation::ADC, AddressingMode::Izx,       6, true ), // ADC  X,ind
        /* 62 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 63 */ id(Operation::RRA, AddressingMode::Izx,       8, false), // RRA  X,ind
        /* 64 */ id(Operation::NOP, AddressingMode::Zp0,       3, false), // NOP  zpg
        /* 65 */ id(Operation::ADC, AddressingMode::Zp0,       3, true ), // ADC  zpg
        /* 66 */ id(Operation::ROR, AddressingMode::Zp0,       5, true ), // ROR  zpg
        /* 67 */ id(Operation::RRA, AddressingMode::Zp0,       5, false), // RRA  zpg
        /* 68 */ id(Operation::PLA, AddressingMode::Implied,   4, true ), // PLA  impl
        /* 69 */ id(Operation::ADC, AddressingMode::Immediate, 2, true ), // ADC  #
        /* 6A */ id(Operation::ROR, AddressingMode::Implied,   2, true ), // ROR  A
        /* 6B */ id(Operation::ARR, AddressingMode::Immediate, 2, false), // ARR  #
        /* 6C */ id(Operation::JMP, AddressingMode::Indirect,  5, true ), // JMP  ind
        /* 6D */ id(Operation::ADC, AddressingMode::Absolute,  4, true ), // ADC  abs
        /* 6E */ id(Operation::ROR, AddressingMode::Absolute,  6, true ), // ROR  abs
        /* 6F */ id(Operation::RRA, AddressingMode::Absolute,  6, false), // RRA  abs
        /* 70 */ id(Operation::BVS, AddressingMode::Relative,  2, true ), // BVS  rel
        /* 71 */ id(Operation::ADC, AddressingMode::Izy,       5, true ), // ADC  ind,Y
        /* 72 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 73 */ id(Operation::RRA, AddressingMode::Izy,       8, false), // RRA  ind,Y
        /* 74 */ id(Operation::NOP, AddressingMode::Zpx,       4, false), // NOP  zpg,X
        /* 75 */ id(Operation::ADC, AddressingMode::Zpx,       4, true ), // ADC  zpg,X
        /* 76 */ id(Operation::ROR, AddressingMode::Zpx,       6, true ), // ROR  zpg,X
        /* 77 */ id(Operation::RRA, AddressingMode::Zpx,       6, false), // RRA  zpg,X
        /* 78 */ id(Operation::SEI, AddressingMode::Implied,   2, true ), // SEI  impl
        /* 79 */ id(Operation::ADC, AddressingMode::Aby,       4, true ), // ADC  abs,Y
        /* 7A */ id(Operation::NOP, AddressingMode::Implied,   2, false), // NOP  impl
        /* 7B */ id(Operation::RRA, AddressingMode::Aby,       7, false), // RRA  abs,Y
        /* 7C */ id(Operation::NOP, AddressingMode::Abx,       4, false), // NOP  abs,X
        /* 7D */ id(Operation::ADC, AddressingMode::Abx,       4, true ), // ADC  abs,X
        /* 7E */ id(Operation::ROR, AddressingMode::Abx,       7, true ), // ROR  abs,X
        /* 7F */ id(Operation::RRA, AddressingMode::Abx,       7, false), // RRA  abs,X
        /* 80 */ id(Operation::NOP, AddressingMode::Immediate, 2, false), // NOP  #
        /* 81 */ id(Operation::STA, AddressingMode::Izx,       6, true ), // STA  X,ind
        /* 82 */ id(Operation::NOP, AddressingMode::Immediate, 2, false), // NOP  #
        /* 83 */ id(Operation::SAX, AddressingMode::Izx,       6, false), // SAX  X,ind
        /* 84 */ id(Operation::STY, AddressingMode::Zp0,       3, true ), // STY  zpg
        /* 85 */ id(Operation::STA, AddressingMode::Zp0,       3, true ), // STA  zpg
        /* 86 */ id(Operation::STX, AddressingMode::Zp0,       3, true ), // STX  zpg
        /* 87 */ id(Operation::SAX, AddressingMode::Zp0,       3, false), // SAX  zpg
        /* 88 */ id(Operation::DEY, AddressingMode::Implied,   2, true ), // DEY  impl
        /* 89 */ id(Operation::NOP, AddressingMode::Immediate, 2, false), // NOP  #
        /* 8A */ id(Operation::TXA, AddressingMode::Implied,   2, true ), // TXA  impl
        /* 8B */ id(Operation::ANE, AddressingMode::Immediate, 2, false), // ANE  #
        /* 8C */ id(Operation::STY, AddressingMode::Absolute,  4, true ), // STY  abs
        /* 8D */ id(Operation::STA, AddressingMode::Absolute,  4, true ), // STA  abs
        /* 8E */ id(Operation::STX, AddressingMode::Absolute,  4, true ), // STX  abs
        /* 8F */ id(Operation::SAX, AddressingMode::Absolute,  4, false), // SAX  abs
        /* 90 */ id(Operation::BCC, AddressingMode::Relative,  2, true ), // BCC  rel
        /* 91 */ id(Operation::STA, AddressingMode::Izy,       6, true ), // STA  ind,Y
        /* 92 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* 93 */ id(Operation::SHA, AddressingMode::Izy,       6, false), // SHA  ind,Y
        /* 94 */ id(Operation::STY, AddressingMode::Zpx,       4, true ), // STY  zpg,X
        /* 95 */ id(Operation::STA, AddressingMode::Zpx,       4, true ), // STA  zpg,X
        /* 96 */ id(Operation::STX, AddressingMode::Zpy,       4, true ), // STX  zpg,Y
        /* 97 */ id(Operation::SAX, AddressingMode::Zpy,       4, false), // SAX  zpg,Y
        /* 98 */ id(Operation::TYA, AddressingMode::Implied,   2, true ), // TYA  impl
        /* 99 */ id(Operation::STA, AddressingMode::Aby,       5, true ), // STA  abs,Y
        /* 9A */ id(Operation::TXS, AddressingMode::Implied,   2, true ), // TXS  impl
        /* 9B */ id(Operation::TAS, AddressingMode::Aby,       5, false), // TAS  abs,Y
        /* 9C */ id(Operation::SHY, AddressingMode::Abx,       5, false), // SHY  abs,X
        /* 9D */ id(Operation::STA, AddressingMode::Abx,       5, true ), // STA  abs,X
        /* 9E */ id(Operation::SHX, AddressingMode::Aby,       5, false), // SHX  abs,Y
        /* 9F */ id(Operation::SHA, AddressingMode::Aby,       5, false), // SHA  abs,Y
        /* A0 */ id(Operation::LDY, AddressingMode::Immediate, 2, true ), // LDY  #
        /* A1 */ id(Operation::LDA, AddressingMode::Izx,       6, true ), // LDA  X,ind
        /* A2 */ id(Operation::LDX, AddressingMode::Immediate, 2, true ), // LDX  #
        /* A3 */ id(Operation::LAX, AddressingMode::Izx,       6, false), // LAX  X,ind
        /* A4 */ id(Operation::LDY, AddressingMode::Zp0,       3, true ), // LDY  zpg
        /* A5 */ id(Operation::LDA, AddressingMode::Zp0,       3, true ), // LDA  zpg
        /* A6 */ id(Operation::LDX, AddressingMode::Zp0,       3, true ), // LDX  zpg
        /* A7 */ id(Operation::LAX, AddressingMode::Zp0,       3, false), // LAX  zpg
        /* A8 */ id(Operation::TAY, AddressingMode::Implied,   2, true ), // TAY  impl
        /* A9 */ id(Operation::LDA, AddressingMode::Immediate, 2, true ), // LDA  #
        /* AA */ id(Operation::TAX, AddressingMode::Implied,   2, true ), // TAX  impl
        /* AB */ id(Operation::LXA, AddressingMode::Immediate, 2, false), // LXA  #
        /* AC */ id(Operation::LDY, AddressingMode::Absolute,  4, true ), // LDY  abs
        /* AD */ id(Operation::LDA, AddressingMode::Absolute,  4, true ), // LDA  abs
        /* AE */ id(Operation::LDX, AddressingMode::Absolute,  4, true ), // LDX  abs
        /* AF */ id(Operation::LAX, AddressingMode::Absolute,  4, false), // LAX  abs
        /* B0 */ id(Operation::BCS, AddressingMode::Relative,  2, true ), // BCS  rel
        /* B1 */ id(Operation::LDA, AddressingMode::Izy,       5, true ), // LDA  ind,Y
        /* B2 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* B3 */ id(Operation::LAX, AddressingMode::Izy,       5, false), // LAX  ind,Y
        /* B4 */ id(Operation::LDY, AddressingMode::Zpx,       4, true ), // LDY  zpg,X
        /* B5 */ id(Operation::LDA, AddressingMode::Zpx,       4, true ), // LDA  zpg,X
        /* B6 */ id(Operation::LDX, AddressingMode::Zpy,       4, true ), // LDX  zpg,Y
        /* B7 */ id(Operation::LAX, AddressingMode::Zpy,       4, false), // LAX  zpg,Y
        /* B8 */ id(Operation::CLV, AddressingMode::Implied,   2, true ), // CLV  impl
        /* B9 */ id(Operation::LDA, AddressingMode::Aby,       4, true ), // LDA  abs,Y
        /* BA */ id(Operation::TSX, AddressingMode::Implied,   2, true ), // TSX  impl
        /* BB */ id(Operation::LAS, AddressingMode::Aby,       4, false), // LAS  abs,Y
        /* BC */ id(Operation::LDY, AddressingMode::Abx,       4, true ), // LDY  abs,X
        /* BD */ id(Operation::LDA, AddressingMode::Abx,       4, true ), // LDA  abs,X
        /* BE */ id(Operation::LDX, AddressingMode::Aby,       4, true ), // LDX  abs,Y
        /* BF */ id(Operation::LAX, AddressingMode::Aby,       4, false), // LAX  abs,Y
        /* C0 */ id(Operation::CPY, AddressingMode::Immediate, 2, true ), // CPY  #
        /* C1 */ id(Operation::CMP, AddressingMode::Izx,       6, true ), // CMP  X,ind
        /* C2 */ id(Operation::NOP, AddressingMode::Immediate, 2, false), // NOP  #
        /* C3 */ id(Operation::DCP, AddressingMode::Izx,       8, false), // DCP  X,ind
        /* C4 */ id(Operation::CPY, AddressingMode::Zp0,       3, true ), // CPY  zpg
        /* C5 */ id(Operation::CMP, AddressingMode::Zp0,       3, true ), // CMP  zpg
        /* C6 */ id(Operation::DEC, AddressingMode::Zp0,       5, true ), // DEC  zpg
        /* C7 */ id(Operation::DCP, AddressingMode::Zp0,       5, false), // DCP  zpg
        /* C8 */ id(Operation::INY, AddressingMode::Implied,   2, true ), // INY  impl
        /* C9 */ id(Operation::CMP, AddressingMode::Immediate, 2, true ), // CMP  #
        /* CA */ id(Operation::DEX, AddressingMode::Implied,   2, true ), // DEX  impl
        /* CB */ id(Operation::SBX, AddressingMode::Immediate, 2, false), // SBX  #
        /* CC */ id(Operation::CPY, AddressingMode::Absolute,  4, true ), // CPY  abs
        /* CD */ id(Operation::CMP, AddressingMode::Absolute,  4, true ), // CMP  abs
        /* CE */ id(Operation::DEC, AddressingMode::Absolute,  6, true ), // DEC  abs
        /* CF */ id(Operation::DCP, AddressingMode::Absolute,  6, false), // DCP  abs
        /* D0 */ id(Operation::BNE, AddressingMode::Relative,  2, true ), // BNE  rel
        /* D1 */ id(Operation::CMP, AddressingMode::Izy,       5, true ), // CMP  ind,Y
        /* D2 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* D3 */ id(Operation::DCP, AddressingMode::Izy,       8, false), // DCP  ind,Y
        /* D4 */ id(Operation::NOP, AddressingMode::Zpx,       4, false), // NOP  zpg,X
        /* D5 */ id(Operation::CMP, AddressingMode::Zpx,       4, true ), // CMP  zpg,X
        /* D6 */ id(Operation::DEC, AddressingMode::Zpx,       6, true ), // DEC  zpg,X
        /* D7 */ id(Operation::DCP, AddressingMode::Zpx,       6, false), // DCP  zpg,X
        /* D8 */ id(Operation::CLD, AddressingMode::Implied,   2, true ), // CLD  impl
        /* D9 */ id(Operation::CMP, AddressingMode::Aby,       4, true ), // CMP  abs,Y
        /* DA */ id(Operation::NOP, AddressingMode::Implied,   2, false), // NOP  impl
        /* DB */ id(Operation::DCP, AddressingMode::Aby,       7, false), // DCP  abs,Y
        /* DC */ id(Operation::NOP, AddressingMode::Abx,       4, false), // NOP  abs,X
        /* DD */ id(Operation::CMP, AddressingMode::Abx,       4, true ), // CMP  abs,X
        /* DE */ id(Operation::DEC, AddressingMode::Abx,       7, true ), // DEC  abs,X
        /* DF */ id(Operation::DCP, AddressingMode::Abx,       7, false), // DCP  abs,X
        /* E0 */ id(Operation::CPX, AddressingMode::Immediate, 2, true ), // CPX  #
        /* E1 */ id(Operation::SBC, AddressingMode::Izx,       6, true ), // SBC  X,ind
        /* E2 */ id(Operation::NOP, AddressingMode::Immediate, 2, false), // NOP  #
        /* E3 */ id(Operation::ISC, AddressingMode::Izx,       8, false), // ISC  X,ind
        /* E4 */ id(Operation::CPX, AddressingMode::Zp0,       3, true ), // CPX  zpg
        /* E5 */ id(Operation::SBC, AddressingMode::Zp0,       3, true ), // SBC  zpg
        /* E6 */ id(Operation::INC, AddressingMode::Zp0,       5, true ), // INC  zpg
        /* E7 */ id(Operation::ISC, AddressingMode::Zp0,       5, false), // ISC  zpg
        /* E8 */ id(Operation::INX, AddressingMode::Implied,   2, true ), // INX  impl
        /* E9 */ id(Operation::SBC, AddressingMode::Immediate, 2, true ), // SBC  #
        /* EA */ id(Operation::NOP, AddressingMode::Implied,   2, true ), // NOP  impl
        /* EB */ id(Operation::SBC, AddressingMode::Immediate, 2, false), // USBC #
        /* EC */ id(Operation::CPX, AddressingMode::Absolute,  4, true ), // CPX  abs
        /* ED */ id(Operation::SBC, AddressingMode::Absolute,  4, true ), // SBC  abs
        /* EE */ id(Operation::INC, AddressingMode::Absolute,  6, true ), // INC  abs
        /* EF */ id(Operation::ISC, AddressingMode::Absolute,  6, false), // ISC  abs
        /* F0 */ id(Operation::BEQ, AddressingMode::Relative,  2, true ), // BEQ  rel
        /* F1 */ id(Operation::SBC, AddressingMode::Izy,       5, true ), // SBC  ind,Y
        /* F2 */ id(Operation::JAM, AddressingMode::Implied,   2, false), // JAM
        /* F3 */ id(Operation::ISC, AddressingMode::Izy,       8, false), // ISC  ind,Y
        /* F4 */ id(Operation::NOP, AddressingMode::Zpx,       4, false), // NOP  zpg,X
        /* F5 */ id(Operation::SBC, AddressingMode::Zpx,       4, true ), // SBC  zpg,X
        /* F6 */ id(Operation::INC, AddressingMode::Zpx,       6, true ), // INC  zpg,X
        /* F7 */ id(Operation::ISC, AddressingMode::Zpx,       6, false), // ISC  zpg,X
        /* F8 */ id(Operation::SED, AddressingMode::Implied,   2, true ), // SED  impl
        /* F9 */ id(Operation::SBC, AddressingMode::Aby,       4, true ), // SBC  abs,Y
        /* FA */ id(Operation::NOP, AddressingMode::Implied,   2, false), // NOP  impl
        /* FB */ id(Operation::ISC, AddressingMode::Aby,       7, false), // ISC  abs,Y
        /* FC */ id(Operation::NOP, AddressingMode::Abx,       4, false), // NOP  abs,X
        /* FD */ id(Operation::SBC, AddressingMode::Abx,       4, true ), // SBC  abs,X
        /* FE */ id(Operation::INC, AddressingMode::Abx,       7, true ), // INC  abs,X
        /* FF */ id(Operation::ISC, AddressingMode::Abx,       7, false), // ISC  abs,X
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
        test_decode!([0xEB, 0x12],       Operation::SBC, Operand::Immediate(0x12));
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
