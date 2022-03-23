#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Operation {
    ADC,
    ALR,
    ANC,
    AND,
    ANE,
    ARR,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DCP,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    ISC,
    JAM,
    JMP,
    JSR,
    LAS,
    LAX,
    LDA,
    LDX,
    LDY,
    LSR,
    LXA,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    RLA,
    ROL,
    ROR,
    RRA,
    RTI,
    RTS,
    SAX,
    SBC,
    SBX,
    SEC,
    SED,
    SEI,
    SHA,
    SHX,
    SHY,
    SLO,
    SRE,
    STA,
    STX,
    STY,
    TAS,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    USB,
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operation::ADC => "ADC",
                Operation::ALR => "ALR",
                Operation::ANC => "ANC",
                Operation::AND => "AND",
                Operation::ANE => "ANE",
                Operation::ARR => "ARR",
                Operation::ASL => "ASL",
                Operation::BCC => "BCC",
                Operation::BCS => "BCS",
                Operation::BEQ => "BEQ",
                Operation::BIT => "BIT",
                Operation::BMI => "BMI",
                Operation::BNE => "BNE",
                Operation::BPL => "BPL",
                Operation::BRK => "BRK",
                Operation::BVC => "BVC",
                Operation::BVS => "BVS",
                Operation::CLC => "CLC",
                Operation::CLD => "CLD",
                Operation::CLI => "CLI",
                Operation::CLV => "CLV",
                Operation::CMP => "CMP",
                Operation::CPX => "CPX",
                Operation::CPY => "CPY",
                Operation::DCP => "DCP",
                Operation::DEC => "DEC",
                Operation::DEX => "DEX",
                Operation::DEY => "DEY",
                Operation::EOR => "EOR",
                Operation::INC => "INC",
                Operation::INX => "INX",
                Operation::INY => "INY",
                Operation::ISC => "ISC",
                Operation::JAM => "JAM",
                Operation::JMP => "JMP",
                Operation::JSR => "JSR",
                Operation::LAS => "LAS",
                Operation::LAX => "LAX",
                Operation::LDA => "LDA",
                Operation::LDX => "LDX",
                Operation::LDY => "LDY",
                Operation::LSR => "LSR",
                Operation::LXA => "LXA",
                Operation::NOP => "NOP",
                Operation::ORA => "ORA",
                Operation::PHA => "PHA",
                Operation::PHP => "PHP",
                Operation::PLA => "PLA",
                Operation::PLP => "PLP",
                Operation::RLA => "RLA",
                Operation::ROL => "ROL",
                Operation::ROR => "ROR",
                Operation::RRA => "RRA",
                Operation::RTI => "RTI",
                Operation::RTS => "RTS",
                Operation::SAX => "SAX",
                Operation::SBC => "SBC",
                Operation::SBX => "SBX",
                Operation::SEC => "SEC",
                Operation::SED => "SED",
                Operation::SEI => "SEI",
                Operation::SHA => "SHA",
                Operation::SHX => "SHX",
                Operation::SHY => "SHY",
                Operation::SLO => "SLO",
                Operation::SRE => "SRE",
                Operation::STA => "STA",
                Operation::STX => "STX",
                Operation::STY => "STY",
                Operation::TAS => "TAS",
                Operation::TAX => "TAX",
                Operation::TAY => "TAY",
                Operation::TSX => "TSX",
                Operation::TXA => "TXA",
                Operation::TXS => "TXS",
                Operation::TYA => "TYA",
                Operation::USB => "USB",
            }
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Offset {
    None,
    X,
    Y,
}

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Offset::None => Ok(()),
            Offset::X => write!(f, ",X"),
            Offset::Y => write!(f, ",Y"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operand {
    None,
    Immediate(u8),
    Relative(i8),
    Absolute(u16, Offset),
    ZeroPage(u8, Offset),
    Indirect(u16, Offset),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::None => write!(f, ""),
            Operand::Immediate(value) => write!(f, "#${:02X}", value),
            Operand::Relative(value) => write!(f, "#{}", value),
            Operand::Absolute(addr, offset) => write!(f, "${:04X}{}", addr, offset),
            Operand::ZeroPage(addr, offset) => write!(f, "${:02X}{}", addr, offset),
            Operand::Indirect(addr, offset) => {
                write!(f, "(${:04X}", addr)?;
                match offset {
                    Offset::None => write!(f, ")"),
                    Offset::X => write!(f, ",X)"),
                    Offset::Y => write!(f, "),Y"),
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub operation: Operation,
    pub operand: Operand,
}

impl Instruction {
    pub fn new(operation: Operation, operand: Operand) -> Self {
        Self { operation, operand }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.operation, self.operand)
    }
}
