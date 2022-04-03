use crate::{Bus, ClockSignal, Instruction, Offset, Operand, Operation, Processor};
use bitflags::bitflags;
use std::ops::{BitAnd, BitOr, BitXor};

// A4 = 10100100
// E4 = 11100100

bitflags! {
    pub struct Flags: u8 {
        const CARRY         = 1 << 0;  // C
        const ZERO          = 1 << 1;  // Z
        const INTERRUPTS    = 1 << 2;  // I
        const DECIMAL       = 1 << 3;  // D
        const BREAK         = 1 << 4;  // B
        const UNUSED        = 1 << 5;  // U
        const OVERFLOW      = 1 << 6;  // V
        const NEGATIVE      = 1 << 7;  // N
    }
}

pub struct State {
    accumulator: u8,
    x_register: u8,
    y_register: u8,
    stack_pointer: u8,
    pub program_counter: u16,
    flags: Flags,
}

impl Default for State {
    fn default() -> Self {
        Self {
            accumulator: 0,
            x_register: 0,
            y_register: 0,
            stack_pointer: 0xFD,
            program_counter: 0xFFFC,
            flags: Flags::UNUSED | Flags::INTERRUPTS,
        }
    }
}

pub struct Cpu<B: Bus> {
    pub state: State,
    bus: B,

    /// Total amount of cycles that the emulator has ran.
    total_cycles: usize,

    /// If not 0, then we are still executing a previous instruction.
    cycles_remaining: u8,
}

fn same_page(address1: u16, address2: u16) -> bool {
    address1 & 0xFF00 == address2 & 0xFF00
}

struct BusIterator<'a, B: Bus> {
    bus: &'a B,
    position: u16,
    bytes_read: u16,
}

impl<'a, B: Bus> BusIterator<'a, B> {
    pub fn new(bus: &'a B, position: u16) -> Self {
        Self {
            bus,
            position,
            bytes_read: 0,
        }
    }
}

impl<'a, B: Bus> Iterator for BusIterator<'a, B> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.bus.read(self.position);
        self.position = self.position.wrapping_add(1);
        self.bytes_read += 1;
        Some(value)
    }
}

fn instruction_to_string<B: Bus>(instruction: &Instruction, cpu: &Cpu<B>) -> String {
    let operand_part = match instruction.operand {
        Operand::Immediate(value) => format!(" #${:02X}", value),

        Operand::Absolute(absolute_address, offset) => {
            format!(
                " ${:04X}{}",
                absolute_address,
                match offset {
                    Offset::None => "",
                    Offset::X => ",X",
                    Offset::Y => ",Y",
                }
            )
        }

        Operand::Indirect(indirect_address) => {
            format!(" (${:04X})", indirect_address,)
        }

        Operand::IndirectZeroPage(indirect_address, offset) => {
            format!(
                " (${:02X}{}",
                indirect_address,
                match offset {
                    Offset::None => ")",
                    Offset::X => ",X)",
                    Offset::Y => "),Y",
                }
            )
        }

        Operand::ZeroPage(address, offset) => {
            format!(
                " ${:02X}{}",
                address,
                match offset {
                    Offset::None => "",
                    Offset::X => ",X",
                    Offset::Y => ",Y",
                }
            )
        }

        Operand::Relative(offset) => {
            let address = cpu.state.program_counter.wrapping_add(offset as u16);
            format!(" ${:04X}", address)
        }

        _ => format!(""),
    };

    format!("{}{}", instruction.operation, operand_part)
}

impl<B: Bus> Cpu<B> {
    pub fn new(bus: B) -> Self {
        Self {
            state: State::default(),
            bus,
            total_cycles: 7,
            cycles_remaining: 0,
        }
    }

    pub fn jump_to(&mut self, address: u16) {
        self.state.program_counter = address;
    }

    pub fn single_cycle(&mut self) {
        if self.cycles_remaining == 0 {
            let program_counter = self.state.program_counter;

            let mut it = BusIterator::new(&self.bus, self.state.program_counter);
            let decoded = crate::decoder::decode_instruction(&mut it).unwrap();
            self.state.program_counter += it.bytes_read;

            /*
            macro_rules! pf {
                ($flag:tt,$symbol:tt) => {{
                    if self.state.flags.contains(Flags::$flag) {
                        $symbol
                    } else {
                        "."
                    }
                }};
            }

            print!("{:04X} | ", program_counter);
            print!(
                "A:{:02X} X:{:02X} Y:{:02X} | ",
                self.state.accumulator, self.state.x_register, self.state.y_register
            );
            print!(
                "PC:{:04X} SP:{:02X} | ",
                self.state.program_counter, self.state.stack_pointer
            );
            print!(
                "{}{}{}{}{}{}{}{} | ",
                pf!(NEGATIVE, "N"),
                pf!(OVERFLOW, "V"),
                pf!(UNUSED, "U"),
                pf!(BREAK, "B"),
                pf!(DECIMAL, "D"),
                pf!(INTERRUPTS, "I"),
                pf!(ZERO, "Z"),
                pf!(CARRY, "C")
            );
            println!("{}", &decoded.instruction);
            */

            // C000  4C F5 C5  JMP $C5F5                       A:00 X:00 Y:00 P:24 SP:FD PPU:  0, 21 CYC:7
            print!("{:04X}  ", program_counter);
            match it.bytes_read {
                0 => print!("          "),
                1 => print!("{:02X}        ", self.bus.read(program_counter)),
                2 => print!(
                    "{:02X} {:02X}     ",
                    self.bus.read(program_counter),
                    self.bus.read(program_counter + 1)
                ),
                3 => print!(
                    "{:02X} {:02X} {:02X}  ",
                    self.bus.read(program_counter),
                    self.bus.read(program_counter + 1),
                    self.bus.read(program_counter + 2)
                ),
                _ => unreachable!(),
            }
            print!(
                "{:<32}",
                format!(
                    "{}{}",
                    instruction_to_string(&decoded.instruction, self),
                    match decoded.instruction.operand {
                        oper @ Operand::Absolute(_, offset)
                            if !matches!(
                                decoded.instruction.operation,
                                Operation::JMP | Operation::JSR
                            ) =>
                        {
                            let (addr, _) = self.get_absolute_address_for_operand(&oper);
                            let value = self.bus.read(addr);
                            match offset {
                                Offset::None => format!(" = {:02X}", value),
                                Offset::X => format!(" @ {:04X} = {:02X}", addr, value),
                                Offset::Y => format!(" @ {:04X} = {:02X}", addr, value),
                            }
                        }

                        oper @ Operand::ZeroPage(_, offset)
                            if !matches!(
                                decoded.instruction.operation,
                                Operation::JMP | Operation::JSR
                            ) =>
                        {
                            let (addr, _) = self.get_absolute_address_for_operand(&oper);
                            let value = self.bus.read(addr);
                            match offset {
                                Offset::None => format!(" = {:02X}", value),
                                Offset::X => format!(" @ {:02X} = {:02X}", addr, value),
                                Offset::Y => format!(" @ {:02X} = {:02X}", addr, value),
                            }
                        }

                        oper @ Operand::Indirect(indirect_address) => {
                            let (absolute_address, _) =
                                self.get_absolute_address_for_operand(&oper);
                            let value = self.bus.read(absolute_address);

                            if matches!(
                                decoded.instruction.operation,
                                Operation::JMP | Operation::JSR
                            ) {
                                format!(" = {:04X}", absolute_address)
                            } else {
                                format!(
                                    " @ {:02X} = {:04X} = {:02X}",
                                    indirect_address as u8, absolute_address, value
                                )
                            }
                        }
                        oper @ Operand::IndirectZeroPage(indirect_address, offset) => {
                            let (absolute_address, _) =
                                self.get_absolute_address_for_operand(&oper);
                            let value = self.bus.read(absolute_address);

                            match offset {
                                Offset::None => format!(""),
                                Offset::X => {
                                    let with_offset =
                                        indirect_address.wrapping_add(self.state.x_register);
                                    format!(
                                        " @ {:02X} = {:04X} = {:02X}",
                                        with_offset, absolute_address, value
                                    )
                                }
                                Offset::Y => {
                                    let pointer = indirect_address as u16;
                                    let lo = self.bus.read(pointer);
                                    let hi = self.bus.read((pointer.wrapping_add(1)) & 0xFF);

                                    let absolute_address = u16::from_le_bytes([lo, hi])
                                        .wrapping_add(self.state.y_register as u16);

                                    format!(
                                        " = {:04X} @ {:04X} = {:02X}",
                                        u16::from_le_bytes([lo, hi]),
                                        absolute_address,
                                        value
                                    )
                                }
                            }
                        }
                        _ => format!(""),
                    }
                )
            );

            print!(
                "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} ",
                self.state.accumulator,
                self.state.x_register,
                self.state.y_register,
                self.state.flags.bits(),
                self.state.stack_pointer
            );

            {
                let total = self.total_cycles * 3;
                const WIDTH: usize = 341;
                print!("PPU:{:>3},{:>3} ", total / WIDTH, total % WIDTH);
            }
            print!("CYC:{}", self.total_cycles);
            println!();

            self.cycles_remaining = decoded.cycles;
            self.execute(&decoded.instruction);
        }

        // At this point we should have set the amount of cycles we need for the instruction we just
        // started to execute.
        debug_assert!(self.cycles_remaining > 0);

        self.total_cycles += 1;
        self.cycles_remaining -= 1;
    }

    pub fn _reset(&mut self) {
        let address: u16 = 0xFFFC;
        let lo = self.bus.read(address);
        let hi = self.bus.read(address + 1);
        let program_counter = u16::from_le_bytes([lo, hi]);

        self.state = State {
            program_counter,
            ..Default::default()
        };

        // TODO: Resetting takes 8 cycles?
        self.cycles_remaining = 8;
    }

    pub fn _interrupt_request(&mut self) {
        if self.state.flags.contains(Flags::INTERRUPTS) {
            self._process_interrupt(0xFFFE);
            self.cycles_remaining += 7;
        }
    }

    pub fn _non_maskable_interrupt_request(&mut self) {
        self._process_interrupt(0xFFFA);
        self.cycles_remaining = 8;
    }

    fn _process_interrupt(&mut self, address: u16) {
        // Push the program counter onto the stack.
        let bytes = self.state.program_counter.to_le_bytes();
        self.push(bytes[1]);
        self.push(bytes[0]);

        // Adjust the flags and then push the flags byte onto the stack.
        self.state.flags.remove(Flags::BREAK);
        self.state.flags.insert(Flags::UNUSED | Flags::INTERRUPTS);
        self.push(self.state.flags.bits());

        // Get the next program counter value from the specified address location.
        let lo = self.bus.read(address);
        let hi = self.bus.read(address + 1);
        self.state.program_counter = u16::from_le_bytes([lo, hi]);
    }

    /// Execute an instruction with the given description.
    fn execute(&mut self, instruction: &Instruction) {
        match instruction.operation {
            // AND Memory with Accumulator
            //
            // A AND M -> A
            // N Z C I D V
            // + + - - - -
            // addressing       assembler       opc     bytes   cycles
            // immediate        AND #oper       29      2       2
            // zeropage         AND oper        25      2       3
            // zeropage,X       AND oper,X      35      2       4
            // absolute         AND oper        2D      3       4
            // absolute,X       AND oper,X      3D      3       4*
            // absolute,Y       AND oper,Y      39      3       4*
            // (indirect,X)     AND (oper,X)    21      2       6
            // (indirect),Y     AND (oper),Y    31      2       5*
            Operation::AND => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = self.state.accumulator.bitand(value);
                self.flags_from_result(result);
                self.state.accumulator = result;
            }

            // OR Memory with Accumulator
            //
            // A OR M -> A
            // N Z C I D V
            // + + - - - -
            // addressing       assembler       opc     bytes   cycles
            // immediate        ORA #oper       09      2       2
            // zeropage         ORA oper        05      2       3
            // zeropage,X       ORA oper,X      15      2       4
            // absolute         ORA oper        0D      3       4
            // absolute,X       ORA oper,X      1D      3       4*
            // absolute,Y       ORA oper,Y      19      3       4*
            // (indirect,X)     ORA (oper,X)    01      2       6
            // (indirect),Y     ORA (oper),Y    11      2       5*
            Operation::ORA => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = self.state.accumulator.bitor(value);
                self.flags_from_result(result);
                self.state.accumulator = result;
            }

            // Exclusive-OR Memory with Accumulator
            //
            // A EOR M -> A
            // N Z C I D V
            // + + - - - -
            // addressing       assembler       opc     bytes   cycles
            // immediate        EOR #oper       49      2       2
            // zeropage         EOR oper        45      2       3
            // zeropage,X       EOR oper,X      55      2       4
            // absolute         EOR oper        4D      3       4
            // absolute,X       EOR oper,X      5D      3       4*
            // absolute,Y       EOR oper,Y      59      3       4*
            // (indirect,X)     EOR (oper,X)    41      2       6
            // (indirect),Y     EOR (oper),Y    51      2       5*
            Operation::EOR => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = self.state.accumulator.bitxor(value);
                self.flags_from_result(result);
                self.state.accumulator = result;
            }

            // Add Memory to Accumulator with Carry
            //
            // A + M + C -> A, C
            // N Z C I D V
            // + + + - - +
            // addressing       assembler       opc     bytes   cycles
            // immediate        ADC #oper       69      2       2
            // zeropage         ADC oper        65      2       3
            // zeropage,X       ADC oper,X      75      2       4
            // absolute         ADC oper        6D      3       4
            // absolute,X       ADC oper,X      7D      3       4*
            // absolute,Y       ADC oper,Y      79      3       4*
            // (indirect,X)     ADC (oper,X)    61      2       6
            // (indirect),Y     ADC (oper),Y    71      2       5*
            Operation::ADC => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let (result, carry) = {
                    let carry = match self.state.flags.contains(Flags::CARRY) {
                        true => 1,
                        false => 0,
                    };

                    let temp = (self.state.accumulator as u16)
                        .wrapping_add(value as u16)
                        .wrapping_add(carry);

                    (temp as u8, temp & 0xFF00 != 0)
                };

                self.flags_from_result(result);

                self.state.flags.set(Flags::OVERFLOW, {
                    let p1 = ((result.bitxor(self.state.accumulator)) & 0x80) != 0;
                    let p2 = ((value.bitxor(self.state.accumulator)) & 0x80) == 0;

                    p1 && p2
                });

                self.state.flags.set(Flags::CARRY, carry);

                self.state.accumulator = result;
            }

            // Subtract Memory from Accumulator with Borrow
            //
            // A - M - C -> A
            // N Z C I D V
            // + + + - - +
            // addressing       assembler       opc     bytes   cycles
            // immediate        SBC #oper       E9      2       2
            // zeropage         SBC oper        E5      2       3
            // zeropage,X       SBC oper,X      F5      2       4
            // absolute         SBC oper        ED      3       4
            // absolute,X       SBC oper,X      FD      3       4*
            // absolute,Y       SBC oper,Y      F9      3       4*
            // (indirect,X)     SBC (oper,X)    E1      2       6
            // (indirect),Y     SBC (oper),Y    F1      2       5*
            Operation::SBC => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let (result, overflow) = {
                    let carry = match self.state.flags.contains(Flags::CARRY) {
                        true => 0,
                        false => 1,
                    };

                    let result = (self.state.accumulator as u16)
                        .wrapping_sub(value as u16)
                        .wrapping_sub(carry);

                    (result as u8, result & 0xFF00 != 0)
                };

                self.state.flags.set(Flags::OVERFLOW, {
                    let p1 = ((result.bitxor(self.state.accumulator)) & 0x80) != 0;
                    let p2 = ((value.bitxor(self.state.accumulator)) & 0x80) != 0;

                    p1 && p2
                });

                self.state.flags.set(Flags::CARRY, !overflow);

                self.flags_from_result(result);

                self.state.accumulator = result;
            }

            // Increment Index Y by One
            //
            // Y + 1 -> Y
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      INY         C8      1       2
            Operation::INY => {
                let result = self.state.y_register.wrapping_add(1);
                self.flags_from_result(result);
                self.state.y_register = result;
            }

            // Increment Index X by One
            //
            // X + 1 -> X
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      INX         E8      1       2
            Operation::INX => {
                let result = self.state.x_register.wrapping_add(1);
                self.flags_from_result(result);
                self.state.x_register = result;
            }

            // Decrement Index Y by One
            //
            // Y - 1 -> Y
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      DEY         88      1       2
            Operation::DEY => {
                let result = self.state.y_register.wrapping_sub(1);
                self.flags_from_result(result);
                self.state.y_register = result;
            }

            // Decrement Index X by One
            //
            // X - 1 -> X
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      DEX         CA      1       2
            Operation::DEX => {
                let result = self.state.x_register.wrapping_sub(1);
                self.flags_from_result(result);
                self.state.x_register = result;
            }

            // Transfer Accumulator to Index Y
            //
            // A -> Y
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      TAY         A8      1       2
            Operation::TAY => {
                let result = self.state.accumulator;
                self.flags_from_result(result);
                self.state.y_register = result;
            }

            // Transfer Index Y to Accumulator
            //
            // Y -> A
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      TYA         98      1       2
            Operation::TYA => {
                let result = self.state.y_register;
                self.flags_from_result(result);
                self.state.accumulator = result;
            }

            // Transfer Accumulator to Index X
            //
            // A -> X
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      TAX         AA      1       2
            Operation::TAX => {
                let result = self.state.accumulator;
                self.flags_from_result(result);
                self.state.x_register = result;
            }

            // Transfer Index X to Accumulator
            //
            // X -> A
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      TXA         8A      1       2
            Operation::TXA => {
                let result = self.state.x_register;
                self.flags_from_result(result);
                self.state.accumulator = result;
            }

            // Transfer Stack Pointer to Index X
            //
            // SP -> X
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      TSX         BA      1       2
            Operation::TSX => {
                let result = self.state.stack_pointer;
                self.flags_from_result(result);
                self.state.x_register = result;
            }

            // Transfer Index X to Stack Register
            //
            // X -> SP
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc bytes   cycles
            // implied      TXS         9A  1       2
            Operation::TXS => {
                let result = self.state.x_register;
                self.state.stack_pointer = result;
            }

            // Jump to New Location
            //
            // (PC+1) -> PCL
            // (PC+2) -> PCH
            // N Z C I D V
            // - - - - - -
            // addressing       assembler       opc     bytes   cycles
            // absolute         JMP oper        4C      3       3
            // indirect         JMP (oper)      6C      3       5
            Operation::JMP => {
                let (addr, cycles_required) =
                    self.get_absolute_address_for_operand(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.state.program_counter = addr;
            }

            // Jump to New Location Saving Return Address
            //
            // push (PC+2),
            // (PC+1) -> PCL
            // (PC+2) -> PCH
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // absolute     JSR oper    20      3       6
            Operation::JSR => {
                self.push_16(self.state.program_counter.wrapping_sub(1));

                let (addr, cycles_required) =
                    self.get_absolute_address_for_operand(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.state.program_counter = addr;
            }

            // Load Index X with Memory
            //
            // M -> X
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // immediate    LDX #oper   A2      2       2
            // zeropage     LDX oper    A6      2       3
            // zeropage,Y   LDX oper,Y  B6      2       4
            // absolute     LDX oper    AE      3       4
            // absolute,Y   LDX oper,Y  BE      3       4*
            Operation::LDX => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;
                self.flags_from_result(value);
                self.state.x_register = value;
            }

            // Shift One Bit Right (Memory or Accumulator)
            //
            // 0 -> [76543210] -> C
            // N Z C I D V
            // 0 + + - - -
            // addressing   assembler   opc     bytes   cycles
            // accumulator  LSR A       4A      1       2
            // zeropage     LSR oper    46      2       5
            // zeropage,X   LSR oper,X  56      2       6
            // absolute     LSR oper    4E      3       6
            // absolute,X   LSR oper,X  5E      3       7
            Operation::LSR => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.state.flags.set(Flags::CARRY, value & 0x01 != 0);
                let result = value.wrapping_shr(1);
                self.flags_from_result(result);

                let cycles_required = self.set_operand_value(&instruction.operand, result);
                self.cycles_remaining += cycles_required;
            }

            // No Operation
            //
            // ---
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      NOP         EA      1       2
            Operation::NOP => {}

            // Push Accumulator on Stack
            //
            // push A
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      PHA         48      1       3
            Operation::PHA => {
                self.push(self.state.accumulator);
            }

            // Pull Accumulator from Stack
            //
            // pull A
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      PLA         68      1       4
            Operation::PLA => {
                let result = self.pull();
                self.flags_from_result(result);
                self.state.accumulator = result;
            }

            // Clear Carry Flag
            //
            // 0 -> C
            // N Z C I D V
            // - - 0 - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      CLC         18      1       2
            Operation::CLC => {
                self.state.flags.set(Flags::CARRY, false);
            }

            // Operation::CLI => {
            //     self.state.flags.set(Flags::INTERRUPTS, false);
            // }

            // Clear Decimal Mode
            //
            // 0 -> D
            // N Z C I D V
            // - - - - 0 -
            // addressing   assembler   opc     bytes   cycles
            // implied      CLD         D8      1       2
            Operation::CLD => {
                self.state.flags.set(Flags::DECIMAL, false);
            }

            // Clear Overflow Flag
            //
            // 0 -> V
            // N Z C I D V
            // - - - - - 0
            // addressing   assembler   opc     bytes   cycles
            // implied      CLV         B8      1       2
            Operation::CLV => {
                self.state.flags.set(Flags::OVERFLOW, false);
            }

            // Set Carry Flag
            //
            // 1 -> C
            // N Z C I D V
            // - - 1 - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      SEC         38      1       2
            Operation::SEC => {
                self.state.flags.set(Flags::CARRY, true);
            }

            // Set Interrupt Disable Status
            //
            // 1 -> I
            // N Z C I D V
            // - - - 1 - -
            // addressing   assembler   opc     bytes   cycles
            // implied      SEI         78      1       2
            Operation::SEI => {
                self.state.flags.set(Flags::INTERRUPTS, true);
            }

            // Set Decimal Flag
            //
            // 1 -> D
            // N Z C I D V
            // - - - - 1 -
            // addressing   assembler   opc     bytes   cycles
            // implied      SED         F8      1       2
            Operation::SED => {
                self.state.flags.set(Flags::DECIMAL, true);
            }

            // Store Index X in Memory
            //
            // X -> M
            // N Z C I D V
            // - - - - - -
            // addressing   assembler       opc     bytes   cycles
            // zeropage     STX oper        86      2       3
            // zeropage,Y   STX oper,Y      96      2       4
            // absolute     STX oper        8E      3       4
            Operation::STX => {
                let cycles_required =
                    self.set_operand_value(&instruction.operand, self.state.x_register);
                self.cycles_remaining += cycles_required;
            }

            // Branch on Carry Set
            //
            // branch on C = 1
            // N Z C I D V
            // - - - - - -
            // addressing       assembler   opc     bytes   cycles
            // relative         BCS oper    B0      2       2**
            Operation::BCS => {
                self.branch_if_flag(&instruction.operand, |f| f.contains(Flags::CARRY));
            }

            // Branch on Carry Clear
            //
            // branch on C = 0
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BCC oper    90      2       2**
            Operation::BCC => {
                self.branch_if_flag(&instruction.operand, |f| !f.contains(Flags::CARRY));
            }

            // Branch on Result Zero
            //
            // branch on Z = 1
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BEQ oper    F0      2       2**
            Operation::BEQ => {
                self.branch_if_flag(&instruction.operand, |f| f.contains(Flags::ZERO));
            }

            // Branch on Overflow Set
            //
            // branch on V = 1
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BVS oper    70      2       2**
            Operation::BVS => {
                self.branch_if_flag(&instruction.operand, |f| f.contains(Flags::OVERFLOW));
            }

            // Branch on Result Plus
            //
            // branch on N = 0
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BPL oper    10      2       2**
            Operation::BPL => {
                self.branch_if_flag(&instruction.operand, |f| !f.contains(Flags::NEGATIVE));
            }

            // Branch on Result Minus
            //
            // branch on N = 1
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BMI oper    30      2       2**
            Operation::BMI => {
                self.branch_if_flag(&instruction.operand, |f| f.contains(Flags::NEGATIVE));
            }

            // Branch on Overflow Clear
            //
            // branch on V = 0
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BVC oper    50      2       2**
            Operation::BVC => {
                self.branch_if_flag(&instruction.operand, |f| !f.contains(Flags::OVERFLOW));
            }

            // Branch on Result not Zero
            //
            // branch on Z = 0
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // relative     BNE oper    D0      2       2**
            Operation::BNE => {
                self.branch_if_flag(&instruction.operand, |f| !f.contains(Flags::ZERO));
            }

            // Load Accumulator with Memory
            //
            // M -> A
            // N Z C I D V
            // + + - - - -
            // addressing       assembler       opc     bytes   cycles
            // immediate        LDA #oper       A9      2       2
            // zeropage         LDA oper        A5      2       3
            // zeropage,X       LDA oper,X      B5      2       4
            // absolute         LDA oper        AD      3       4
            // absolute,X       LDA oper,X      BD      3       4*
            // absolute,Y       LDA oper,Y      B9      3       4*
            // (indirect,X)     LDA (oper,X)    A1      2       6
            // (indirect),Y     LDA (oper),Y    B1      2       5*
            Operation::LDA => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.flags_from_result(value);
                self.state.accumulator = value;
            }

            // Load Index Y with Memory
            //
            // M -> Y
            // N Z C I D V
            // + + - - - -
            // addressing   assembler       opc     bytes   cycles
            // immediate    LDY #oper       A0      2       2
            // zeropage     LDY oper        A4      2       3
            // zeropage,X   LDY oper,X      B4      2       4
            // absolute     LDY oper        AC      3       4
            // absolute,X   LDY oper,X      BC      3       4*
            Operation::LDY => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.flags_from_result(value);
                self.state.y_register = value;
            }

            // Store Accumulator in Memory
            //
            // A -> M
            // N Z C I D V
            // - - - - - -
            // addressing       assembler       opc     bytes   cycles
            // zeropage         STA oper        85      2       3
            // zeropage,X       STA oper,X      95      2       4
            // absolute         STA oper        8D      3       4
            // absolute,X       STA oper,X      9D      3       5
            // absolute,Y       STA oper,Y      99      3       5
            // (indirect,X)     STA (oper,X)    81      2       6
            // (indirect),Y     STA (oper),Y    91      2       6
            Operation::STA => {
                let cycles = self.set_operand_value(&instruction.operand, self.state.accumulator);
                self.cycles_remaining += cycles;
            }

            // Sore Index Y in Memory
            //
            // Y -> M
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // zeropage     STY oper    84      2       3
            // zeropage,X   STY oper,X  94      2       4
            // absolute     STY oper    8C      3       4
            Operation::STY => {
                let cycles = self.set_operand_value(&instruction.operand, self.state.y_register);
                self.cycles_remaining += cycles;
            }

            // Test Bits in Memory with Accumulator
            //
            // bits 7 and 6 of operand are transferred to bit 7 and 6 of SR (N,V);
            // the zero-flag is set to the result of operand AND accumulator.
            //
            // A AND M, M7 -> N, M6 -> V
            // N  Z C I D V
            // M7 + - - - M6
            // addressing   assembler   opc     bytes   cycles
            // zeropage     BIT oper    24      2       3
            // absolute     BIT oper    2C      3       4
            Operation::BIT => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                // fetch();
                // temp = a & fetched;
                // SetFlag(Z, (temp & 0x00FF) == 0x00);
                // SetFlag(N, fetched & (1 << 7));
                // SetFlag(V, fetched & (1 << 6));

                // self.state
                //     .flags
                //     .set(Flags::ZERO, value.bitxor(self.state.accumulator) == 0);
                self.state
                    .flags
                    .set(Flags::ZERO, self.state.accumulator.bitand(value) == 0);

                self.state
                    .flags
                    .set(Flags::NEGATIVE, value.bitand(Flags::NEGATIVE.bits()) != 0);
                self.state
                    .flags
                    .set(Flags::OVERFLOW, value.bitand(Flags::OVERFLOW.bits()) != 0);
            }

            // Rotate One Bit Left (Memory or Accumulator)
            //
            // C <- [76543210] <- C
            // N Z C I D V
            // + + + - - -
            // addressing   assembler   opc     bytes   cycles
            // accumulator  ROL A       2A      1       2
            // zeropage     ROL oper    26      2       5
            // zeropage,X   ROL oper,X  36      2       6
            // absolute     ROL oper    2E      3       6
            // absolute,X   ROL oper,X  3E      3       7
            Operation::ROL => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = value.wrapping_shl(1).wrapping_add(
                    if self.state.flags.contains(Flags::CARRY) {
                        0x01
                    } else {
                        0x00
                    },
                );

                self.flags_from_result(result);
                self.state.flags.set(Flags::CARRY, value & 0x80 != 0);

                self.cycles_remaining += self.set_operand_value(&instruction.operand, result);
            }

            // Rotate One Bit Right (Memory or Accumulator)
            //
            // C -> [76543210] -> C
            // N Z C I D V
            // + + + - - -
            // addressing   assembler   opc     bytes   cycles
            // accumulator  ROR A       6A      1       2
            // zeropage     ROR oper    66      2       5
            // zeropage,X   ROR oper,X  76      2       6
            // absolute     ROR oper    6E      3       6
            // absolute,X   ROR oper,X  7E      3       7
            Operation::ROR => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = value.wrapping_shr(1).wrapping_add(
                    if self.state.flags.contains(Flags::CARRY) {
                        0x80
                    } else {
                        0x00
                    },
                );

                self.flags_from_result(result);
                self.state.flags.set(Flags::CARRY, value & 0x01 != 0);

                self.cycles_remaining += self.set_operand_value(&instruction.operand, result);
            }

            // Return from Subroutine
            //
            // pull PC, PC+1 -> PC
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      RTS         60      1       6
            Operation::RTS => {
                let addr = self.pull_16();
                self.state.program_counter = addr.wrapping_add(1);
            }

            // Push Processor Status on Stack
            //
            // The status register will be pushed with the break flag and bit 5 set to 1.
            //
            // push SR
            // N Z C I D V
            // - - - - - -
            // addressing   assembler   opc     bytes   cycles
            // implied      PHP         08      1       3
            Operation::PHP => {
                let flags = self.state.flags | Flags::BREAK | Flags::UNUSED;
                self.push(flags.bits());
            }

            // Pull Processor Status from Stack
            //
            // The status register will be pulled with the break flag and bit 5 ignored.
            //
            // pull SR
            // N Z C I D V
            // from stack
            // addressing   assembler   opc     bytes   cycles
            // implied      PLP         28      1       4
            Operation::PLP => {
                let value = self.pull();
                let has_break = self.state.flags.contains(Flags::BREAK);
                let has_unused = self.state.flags.contains(Flags::UNUSED);
                self.state.flags = Flags::from_bits_truncate(value);
                self.state.flags.set(Flags::BREAK, has_break);
                self.state.flags.set(Flags::UNUSED, has_unused);
            }

            // Compare Memory with Accumulator
            //
            // A - M
            // N Z C I D V
            // + + + - - -
            // addressing           assembler       opc     bytes   cycles
            // immediate            CMP #oper       C9      2       2
            // zeropage             CMP oper        C5      2       3
            // zeropage,X           CMP oper,X      D5      2       4
            // absolute             CMP oper        CD      3       4
            // absolute,X           CMP oper,X      DD      3       4*
            // absolute,Y           CMP oper,Y      D9      3       4*
            // (indirect,X)         CMP (oper,X)    C1      2       6
            // (indirect),Y         CMP (oper),Y    D1      2       5*
            Operation::CMP => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.operation_compare(self.state.accumulator, value);
            }

            // Compare Memory and Index Y
            //
            // Y - M
            // N Z C I D V
            // + + + - - -
            // addressing   assembler   opc     bytes   cycles
            // immediate    CPY #oper   C0      2       2
            // zeropage     CPY oper    C4      2       3
            // absolute     CPY oper    CC      3       4
            Operation::CPY => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.operation_compare(self.state.y_register, value);
            }

            // Compare Memory and Index X
            //
            // X - M
            // N Z C I D V
            // + + + - - -
            // addressing   assembler   opc     bytes   cycles
            // immediate    CPX #oper   E0      2       2
            // zeropage     CPX oper    E4      2       3
            // absolute     CPX oper    EC      3       4
            Operation::CPX => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                self.operation_compare(self.state.x_register, value);
            }

            // Force Break
            //
            // BRK initiates a software interrupt similar to a hardware interrupt (IRQ). The return
            // address pushed to the stack is PC+2, providing an extra byte of spacing for a break
            // mark (identifying a reason for the break.)
            // The status register will be pushed to the stack with the break flag set to 1.
            // However, when retrieved during RTI or by a PLP instruction, the break flag will be
            // ignored. The interrupt disable flag is not set automatically.
            //
            // interrupt,
            // push PC+2, push SR
            // N Z C I D V
            // - - - 1 - -
            // addressing   assembler   opc     bytes   cycles
            // implied      BRK         00      1       7
            Operation::BRK => {
                self.state.program_counter = self.state.program_counter.wrapping_add(1);

                self.push_16(self.state.program_counter);

                let temp = self.state.flags | Flags::UNUSED | Flags::BREAK;
                self.push(temp.bits());

                // TODO: Request interrupt.
                // TODO: Jump to interrupt vector.
            }

            // DEC oper + CMP oper
            //
            // M - 1 -> M, A - M
            //
            // N Z C I D V
            // + + + - - -
            // addressing       assembler       opc     bytes   cycles
            // zeropage         DCP oper        C7      2       5
            // zeropage,X       DCP oper,X      D7      2       6
            // absolute         DCP oper        CF      3       6
            // absolut,X        DCP oper,X      DF      3       7
            // absolut,Y        DCP oper,Y      DB      3       7
            // (indirect,X)     DCP (oper,X)    C3      2       8
            // (indirect),Y     DCP (oper),Y    D3      2       8
            Operation::DCP => {
                self.execute(&Instruction::new(
                    Operation::DEY,
                    instruction.operand.clone(),
                ));
                self.execute(&Instruction::new(
                    Operation::CMP,
                    instruction.operand.clone(),
                ));
            }

            // Decrement Memory by One
            //
            // M - 1 -> M
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // zeropage     DEC oper    C6      2       5
            // zeropage,X   DEC oper,X  D6      2       6
            // absolute     DEC oper    CE      3       6
            // absolute,X   DEC oper,X  DE      3       7
            Operation::DEC => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = value.wrapping_sub(1);
                self.flags_from_result(result);

                self.cycles_remaining += self.set_operand_value(&instruction.operand, result);
            }

            // Shift Left One Bit (Memory or Accumulator)
            //
            // C <- [76543210] <- 0
            // N Z C I D V
            // + + + - - -
            // addressing   assembler   opc     bytes   cycles
            // accumulator  ASL A       0A      1       2
            // zeropage     ASL oper    06      2       5
            // zeropage,X   ASL oper,X  16      2       6
            // absolute     ASL oper    0E      3       6
            // absolute,X   ASL oper,X  1E      3       7
            Operation::ASL => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = value.wrapping_shl(1);

                self.flags_from_result(result as u8);
                self.state.flags.set(Flags::CARRY, value & 0x80 != 0);

                self.cycles_remaining += self.set_operand_value(&instruction.operand, result as u8);
            }

            // CMP and DEX at once, sets flags like CMP
            //
            // (A AND X) - oper -> X
            //
            // N Z C I D V
            // + + + - - -
            // addressing   assembler   opc     bytes   cycles
            // immediate    SBX #oper   CB      2       2
            Operation::SBX => {
                // TODO
            }

            // INC oper + SBC oper
            //
            // M + 1 -> M, A - M - C -> A
            //
            // N Z C I D V
            // + + + - - +
            // addressing       assembler       opc     bytes   cycles
            // zeropage         ISC oper        E7      2       5
            // zeropage,X       ISC oper,X      F7      2       6
            // absolute         ISC oper        EF      3       6
            // absolut,X        ISC oper,X      FF      3       7
            // absolut,Y        ISC oper,Y      FB      3       7
            // (indirect,X)     ISC (oper,X)    E3      2       8
            // (indirect),Y     ISC (oper),Y    F3      2       4
            Operation::ISC => {
                self.execute(&Instruction::new(
                    Operation::INC,
                    instruction.operand.clone(),
                ));

                self.execute(&Instruction::new(
                    Operation::SBC,
                    instruction.operand.clone(),
                ));
            }

            // Increment Memory by One
            //
            // M + 1 -> M
            // N Z C I D V
            // + + - - - -
            // addressing   assembler   opc     bytes   cycles
            // zeropage     INC oper    E6      2       5
            // zeropage,X   INC oper,X  F6      2       6
            // absolute     INC oper    EE      3       6
            // absolute,X   INC oper,X  FE      3       7
            Operation::INC => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;

                let result = value.wrapping_add(1);

                self.flags_from_result(result);
                self.cycles_remaining += self.set_operand_value(&instruction.operand, result);
            }

            // Return from Interrupt
            //
            // The status register is pulled with the break flag and bit 5 ignored. Then PC is
            // pulled from the stack.
            //
            // pull SR, pull PC
            // N Z C I D V
            // from stack
            // addressing   assembler   opc     bytes   cycles
            // implied      RTI         40      1       6
            Operation::RTI => {
                self.state.flags = Flags::from_bits_truncate(self.pull());
                self.state.flags.insert(Flags::UNUSED);
                self.state.flags.remove(Flags::BREAK);

                let addr = self.pull_16();
                self.state.program_counter = addr;
            }

            _ => todo!("Instruction execution not implemented: {:?}", instruction),
        }
    }

    fn push(&mut self, value: u8) {
        let stack_pointer = (self.state.stack_pointer as u16).wrapping_add(0x0100);
        log::trace!("Pushing value ${:02X} into ${:04X}", value, stack_pointer);
        self.bus.write(stack_pointer, value);
        self.state.stack_pointer = self.state.stack_pointer.wrapping_sub(1);
    }

    fn push_16(&mut self, value: u16) {
        let bytes = value.to_le_bytes();
        self.push(bytes[1]);
        self.push(bytes[0]);
    }

    fn pull(&mut self) -> u8 {
        self.state.stack_pointer = self.state.stack_pointer.wrapping_add(1);

        let stack_pointer = (self.state.stack_pointer as u16).wrapping_add(0x0100);
        let value = self.bus.read(stack_pointer);

        log::trace!("Pulled value ${:02X} from ${:04X}", value, stack_pointer);

        value
    }

    fn pull_16(&mut self) -> u16 {
        u16::from_le_bytes([self.pull(), self.pull()])
    }

    fn get_operand_value(&self, operand: &Operand) -> (u8, u8) {
        match operand {
            Operand::None => (self.state.accumulator, 0),

            Operand::Immediate(value) => (*value, 0),

            Operand::Relative(_) => {
                unreachable!("Relative operands can not be fetched with get_operand_value.")
            }

            _ => {
                let (address, cycles_required) = self.get_absolute_address_for_operand(operand);
                (self.bus.read(address), cycles_required)
            }
        }
    }

    #[must_use]
    fn set_operand_value(&mut self, operand: &Operand, value: u8) -> u8 {
        match operand {
            Operand::None => {
                self.state.accumulator = value;
                0
            }
            Operand::Immediate(_) => unreachable!("Can not set a value for an Immediate operand!"),
            Operand::Relative(_) => todo!(),
            _ => {
                let (address, cycles_required) = self.get_absolute_address_for_operand(operand);
                self.bus.write(address, value);
                cycles_required
            }
        }
    }

    #[must_use]
    fn get_absolute_address_for_operand(&self, operand: &Operand) -> (u16, u8) {
        match *operand {
            Operand::Relative(offset) => {
                let addr = self.state.program_counter.wrapping_add(offset as u16);

                let cycles_required = if !same_page(addr, self.state.program_counter) {
                    2
                } else {
                    1
                };

                (addr, cycles_required)
            }

            Operand::Absolute(address, offset) => {
                let offset_address = address.wrapping_add(match offset {
                    Offset::None => 0,
                    Offset::X => self.state.x_register as u16,
                    Offset::Y => self.state.y_register as u16,
                });

                let cycles_required = if !same_page(address, offset_address) {
                    1
                } else {
                    0
                };

                (offset_address, cycles_required)
            }

            Operand::ZeroPage(address, offset) => {
                let address = u16::from_le_bytes([
                    address.wrapping_add(match offset {
                        Offset::None => 0,
                        Offset::X => self.state.x_register,
                        Offset::Y => self.state.y_register,
                    }),
                    0x00,
                ]);

                (address, 0)
            }

            Operand::Indirect(pointer) => {
                let lo = self.bus.read(pointer);
                let hi = self.bus.read(if pointer & 0xFF == 0xFF {
                    pointer & 0xFF00
                } else {
                    pointer + 1
                });

                (u16::from_le_bytes([lo, hi]), 0)
            }

            Operand::IndirectZeroPage(pointer, offset) => match offset {
                Offset::None => unreachable!(),
                Offset::X => {
                    let pointer = pointer.wrapping_add(self.state.x_register) as u16;
                    let lo = self.bus.read(pointer);
                    let hi = self.bus.read((pointer + 1) & 0xFF);

                    (u16::from_le_bytes([lo, hi]), 0)
                }
                Offset::Y => {
                    let lo = self.bus.read(pointer as u16);
                    let hi = self.bus.read(pointer.wrapping_add(1) as u16);

                    let address =
                        u16::from_le_bytes([lo, hi]).wrapping_add(self.state.y_register as u16);

                    let extra_cycles = match same_page(address, (hi as u16) << 8) {
                        true => 0,
                        false => 1,
                    };

                    (address, extra_cycles)
                }
            },

            _ => unreachable!("Can not get absolute address for operand: {:?}", operand),
        }
    }

    fn branch_if_flag<P: Fn(&Flags) -> bool>(&mut self, operand: &Operand, predicate: P) {
        let (addr, cycles_required) = self.get_absolute_address_for_operand(operand);

        if predicate(&self.state.flags) {
            // We only increase the amount of cycles required if we do the branch.
            self.cycles_remaining += cycles_required;
            self.state.program_counter = addr;
        }
    }

    #[inline(always)]
    fn flags_from_result(&mut self, result: u8) {
        self.state.flags.set(Flags::ZERO, result == 0x00);
        self.state.flags.set(Flags::NEGATIVE, (result & 0x80) != 0);
    }

    fn operation_compare(&mut self, left: u8, right: u8) {
        let result = left.wrapping_sub(right);

        self.flags_from_result(result);
        self.state.flags.set(Flags::CARRY, left >= right);
    }
}

// impl<B: Bus> Iterator for Cpu<B> {
//     type Item = u8;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         let value = self.bus.read(self.state.program_counter);
//         self.state.program_counter = self.state.program_counter.wrapping_add(1);
//         Some(value)
//     }
// }

impl<B: Bus> ClockSignal for Cpu<B> {
    fn step(&mut self) {
        self.single_cycle();
    }
}

impl<B: Bus> Processor for Cpu<B> {
    fn reset(&mut self) {
        todo!()
    }

    fn interrupt(&mut self) {
        todo!()
    }
}
