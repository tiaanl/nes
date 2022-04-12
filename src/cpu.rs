#![allow(dead_code)]

use crate::{Bus, ClockSignal, Instruction, Offset, Operand, Operation, Processor};
use bitflags::bitflags;

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
    pub accumulator: u8,
    pub x_register: u8,
    pub y_register: u8,
    pub stack_pointer: u8,
    pub program_counter: u16,
    pub flags: Flags,
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

pub struct Internal {
    pub immediate: u8,
    pub absolute_address: u16,
    pub relative_offset: i16,
}

pub struct Cpu<B: Bus> {
    pub state: State,
    pub bus: B,

    /// Total amount of cycles that the emulator has ran.
    total_cycles: usize,

    /// If not 0, then we are still executing a previous instruction.
    pub cycles_remaining: u8,

    pub internal: Internal,
}

pub fn same_page(address1: u16, address2: u16) -> bool {
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

fn operand_to_string<B: Bus>(cpu: &Cpu<B>, op_code: u8, operand: &Operand) -> String {
    match *operand {
        Operand::None => match op_code {
            0x0A | 0x2A | 0x4A | 0x6A => format!(" A"),
            _ => format!(""),
        },

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
    }
}

impl<B: Bus> Cpu<B> {
    pub fn new(bus: B) -> Self {
        Self {
            state: State::default(),
            bus,
            total_cycles: 7,
            cycles_remaining: 0,
            internal: Internal {
                immediate: 0,
                absolute_address: 0,
                relative_offset: 0,
            },
        }
    }

    pub fn jump_to(&mut self, address: u16) {
        self.state.program_counter = address;
    }

    pub fn single_cycle(&mut self) {
        if self.cycles_remaining == 0 {
            let program_counter = self.state.program_counter;

            let op_code = self.bus.read(program_counter);
            self.state.program_counter = program_counter.wrapping_add(1);

            let (op, addressing_mode, mnemonic, cycles_required, legal) =
                Self::MAP[op_code as usize];
            self.cycles_remaining += cycles_required;

            let operand = addressing_mode(self);

            // Set internal state specified by the op code.

            match &operand {
                Operand::None => {}
                Operand::Immediate(immediate) => self.internal.immediate = *immediate,
                Operand::Relative(_) => {}
                oper @ Operand::Absolute(_, _)
                | oper @ Operand::ZeroPage(_, _)
                | oper @ Operand::Indirect(_)
                | oper @ Operand::IndirectZeroPage(_, _) => {
                    let (addr, cycles_required) = self.get_absolute_address_for_operand(oper);
                    self.cycles_remaining += cycles_required;
                    self.internal.absolute_address = addr;
                    self.internal.immediate = self.bus.read(addr);
                }
            };

            // Record the number of bytes read before executing the instruction, because we might
            // adjust the program counter with a jump.
            let bytes_read = self.state.program_counter - program_counter;

            // let program_counter = self.state.program_counter;
            //
            // let mut it = BusIterator::new(&self.bus, self.state.program_counter);
            // let decoded = crate::decoder::decode_instruction(&mut it).unwrap();
            // self.state.program_counter += it.bytes_read;

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
            match bytes_read {
                0 => print!("         "),
                1 => print!("{:02X}       ", self.bus.read(program_counter)),
                2 => print!(
                    "{:02X} {:02X}    ",
                    self.bus.read(program_counter),
                    self.bus.read(program_counter + 1)
                ),
                3 => print!(
                    "{:02X} {:02X} {:02X} ",
                    self.bus.read(program_counter),
                    self.bus.read(program_counter + 1),
                    self.bus.read(program_counter + 2)
                ),
                _ => unreachable!("Invalid number of read bytes: {}", bytes_read),
            }

            if legal {
                print!(" ");
            } else {
                print!("*");
            }

            print!(
                "{:<32}",
                format!(
                    "{}{}{}",
                    mnemonic,
                    operand_to_string(self, op_code, &operand),
                    match operand {
                        oper @ Operand::Absolute(_, offset)
                            if mnemonic != "JMP" && mnemonic != "JSR" =>
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
                            if mnemonic != "JMP" && mnemonic != "JSR" =>
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

                            if mnemonic == "JMP" || mnemonic == "JSR" {
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

            // self.cycles_remaining = decoded.cycles;
            // self.execute(&decoded.instruction);

            op(self, &operand);
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
            Operation::ROR => {}

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

            // (including DOP, TOP)
            //
            // Instructions effecting in 'no operations' in various address modes. Operands are ignored.
            // N Z C I D V
            // - - - - - -
            // opc  addressing  bytes   cycles
            // 1A   implied     1       2
            // 3A   implied     1       2
            // 5A   implied     1       2
            // 7A   implied     1       2
            // DA   implied     1       2
            // FA   implied     1       2
            // 80   immediate   2       2
            // 82   immediate   2       2
            // 89   immediate   2       2
            // C2   immediate   2       2
            // E2   immediate   2       2
            // 04   zeropage    2       3
            // 44   zeropage    2       3
            // 64   zeropage    2       3
            // 14   zeropage,X  2       4
            // 34   zeropage,X  2       4
            // 54   zeropage,X  2       4
            // 74   zeropage,X  2       4
            // D4   zeropage,X  2       4
            // F4   zeropage,X  2       4
            // 0C   absolute    3       4
            // 1C   absolut,X   3       4*
            // 3C   absolut,X   3       4*
            // 5C   absolut,X   3       4*
            // 7C   absolut,X   3       4*
            // DC   absolut,X   3       4*
            // FC   absolut,X   3       4*
            Operation::NOP if instruction.legal == false => {
                // These are illegal operations, but they use cycles according to the operand, so
                // although we're not performing any operation, we still need to use up cycles.
                let (_, cycles_required) = self.get_operand_value(&instruction.operand);
                self.cycles_remaining += cycles_required;
            }

            _ => todo!("Instruction execution not implemented: {:?}", instruction),
        }
    }

    pub fn push(&mut self, value: u8) {
        let stack_pointer = (self.state.stack_pointer as u16).wrapping_add(0x0100);
        log::trace!("Pushing value ${:02X} into ${:04X}", value, stack_pointer);
        self.bus.write(stack_pointer, value);
        self.state.stack_pointer = self.state.stack_pointer.wrapping_sub(1);
    }

    pub fn push_16(&mut self, value: u16) {
        let bytes = value.to_le_bytes();
        self.push(bytes[1]);
        self.push(bytes[0]);
    }

    pub fn pull(&mut self) -> u8 {
        self.state.stack_pointer = self.state.stack_pointer.wrapping_add(1);

        let stack_pointer = (self.state.stack_pointer as u16).wrapping_add(0x0100);
        let value = self.bus.read(stack_pointer);

        log::trace!("Pulled value ${:02X} from ${:04X}", value, stack_pointer);

        value
    }

    pub fn pull_16(&mut self) -> u16 {
        u16::from_le_bytes([self.pull(), self.pull()])
    }

    pub fn get_operand_value(&self, operand: &Operand) -> (u8, u8) {
        match operand {
            Operand::None => (self.state.accumulator, 0),

            Operand::Immediate(value) => (*value, 0),

            Operand::Relative(_) => {
                // unreachable!("Relative operands can not be fetched with get_operand_value.")
                (0, 0)
            }

            _ => {
                let (address, cycles_required) = self.get_absolute_address_for_operand(operand);
                (self.bus.read(address), cycles_required)
            }
        }
    }

    pub fn set_operand_value(&mut self, operand: &Operand, value: u8) {
        match operand {
            Operand::None => {
                self.state.accumulator = value;
            }
            Operand::Immediate(_) => unreachable!("Can not set a value for an Immediate operand!"),
            Operand::Relative(_) => todo!(),
            _ => {
                // TODO: For setting memory values, we don't add more cycles?  This seems to be the
                // case according to nestest.
                let (address, _) = self.get_absolute_address_for_operand(operand);
                self.bus.write(address, value);
            }
        }
    }

    #[must_use]
    pub fn get_absolute_address_for_operand(&self, operand: &Operand) -> (u16, u8) {
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

    pub fn branch_if_flag<P: Fn(&Flags) -> bool>(&mut self, operand: &Operand, predicate: P) {
        let (addr, cycles_required) = self.get_absolute_address_for_operand(operand);

        if predicate(&self.state.flags) {
            // We only increase the amount of cycles required if we do the branch.
            self.cycles_remaining += cycles_required;
            self.state.program_counter = addr;
        }
    }

    #[inline(always)]
    pub fn flags_from_result(&mut self, result: u8) {
        self.state.flags.set(Flags::ZERO, result == 0x00);
        self.state.flags.set(Flags::NEGATIVE, (result & 0x80) != 0);
    }

    pub fn operation_compare(&mut self, left: u8, right: u8) {
        let result = left.wrapping_sub(right);

        self.flags_from_result(result);
        self.state.flags.set(Flags::CARRY, left >= right);
    }

    // fn adc(&mut self, value: u8) {
    //     let (result, carry) = {
    //         let carry = match self.state.flags.contains(Flags::CARRY) {
    //             true => 1,
    //             false => 0,
    //         };
    //
    //         let result = (self.state.accumulator as u16)
    //             .wrapping_add(value as u16)
    //             .wrapping_add(carry);
    //
    //         (result as u8, result & 0xFF00 != 0)
    //     };
    //
    //     let overflow = {
    //         let p1 = ((result.bitxor(self.state.accumulator)) & 0x80) != 0;
    //         let p2 = ((value.bitxor(self.state.accumulator)) & 0x80) == 0;
    //
    //         p1 && p2
    //     };
    //
    //     self.flags_from_result(result);
    //     self.state.flags.set(Flags::CARRY, carry);
    //     self.state.flags.set(Flags::OVERFLOW, overflow);
    //
    //     self.state.accumulator = result;
    // }
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
