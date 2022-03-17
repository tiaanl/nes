mod cartridge;
mod decoder;
mod instruction;

use crate::cartridge::Cartridge;
use crate::instruction::{Instruction, Offset, Operand, Operation};
use bitflags::bitflags;
use std::cell::RefCell;
use std::ops::BitAnd;
use std::rc::Rc;

trait Bus {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

struct RandomAccessMemory {
    data: Vec<u8>,
}

impl RandomAccessMemory {
    pub fn with_capacity(capacity: u16) -> Self {
        Self {
            data: vec![0_u8; capacity as usize],
        }
    }
}

impl Bus for RandomAccessMemory {
    fn read(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.data[address as usize] = value;
    }
}

bitflags! {
    pub struct Flags: u8 {
        const CARRY         = 1 << 0;
        const ZERO          = 1 << 1;
        const INTERRUPTS    = 1 << 2;
        const DECIMAL       = 1 << 3;
        const BREAK         = 1 << 4;
        const UNUSED        = 1 << 5;
        const OVERFLOW      = 1 << 6;
        const NEGATIVE      = 1 << 7;
    }
}

impl Default for Flags {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Default)]
struct State {
    a: u8,
    x: u8,
    y: u8,
    _stack_pointer: u16,
    program_counter: u16,
    flags: Flags,
}

struct Cpu<B: Bus> {
    state: State,
    bus: B,

    /// If not 0, then we are still executing a previous instruction.
    cycles_remaining: u8,
}

fn same_page(address1: u16, address2: u16) -> bool {
    address1 & 0xFF00 == address2 & 0xFF00
}

fn flags_for_result(flags: &mut Flags, result: u8) {
    flags.set(Flags::ZERO, result == 0x00);
    flags.set(Flags::NEGATIVE, result & 0x80 != 0);
}

impl<B: Bus> Cpu<B> {
    pub fn new(bus: B) -> Self {
        Self {
            state: State::default(),
            bus,
            cycles_remaining: 0,
        }
    }

    pub fn single_cycle(&mut self) {
        log::info!("CPU cycle");

        if self.cycles_remaining == 0 {
            let decoded = decoder::decode_instruction(self).unwrap();
            self.cycles_remaining = decoded.cycles;
            self.execute(&decoded.instruction);
        }

        // At this point we should have set the amount of cycles we need for the instruction we just
        // started to execute.
        debug_assert!(self.cycles_remaining > 0);

        self.cycles_remaining -= 1;
    }

    pub fn _reset(&mut self) {
        let address: u16 = 0xFFFC;
        let lo = self.bus.read(address);
        let hi = self.bus.read(address + 1);
        let program_counter = u16::from_le_bytes([lo, hi]);

        self.state = State {
            a: 0,
            x: 0,
            y: 0,
            _stack_pointer: 0xFD,
            program_counter,
            flags: Flags::UNUSED,
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
        self._push(bytes[1]);
        self._push(bytes[0]);

        // Adjust the flags and then push the flags byte onto the stack.
        self.state.flags.set(Flags::BREAK, false);
        self.state.flags.set(Flags::UNUSED, true);
        self.state.flags.set(Flags::INTERRUPTS, true);
        self._push(self.state.flags.bits());

        // Get the next program counter value from the specified address location.
        let lo = self.bus.read(address);
        let hi = self.bus.read(address + 1);
        self.state.program_counter = u16::from_le_bytes([lo, hi]);
    }

    /// Execute an instruction with the given description.
    fn execute(&mut self, instruction: &Instruction) {
        log::trace!("Executing instruction: {:?}", instruction);

        match instruction.operation {
            Operation::AND => {
                let (value, cycles_required) = self.get_operand_value(&instruction.operand);
                let result = self.state.a.bitand(value);
                flags_for_result(&mut self.state.flags, result);
                self.state.a = result;
                self.cycles_remaining += cycles_required;
            }

            Operation::JMP => {
                if let Operand::Absolute(address, Offset::None) = &instruction.operand {
                    self.state.program_counter = *address;
                }
            }

            Operation::BRK => {}

            _ => todo!("Instruction execution not implemented: {:?}", instruction),
        }

        // let (address, cycles_required) = self.get_absolute_address(instruction.1);
        //
        // self.cycles_remaining += cycles_required;
        //
        // // Fetch
        // let data = match instruction.1 {
        //     AddressingMode::Implied => 0,
        //     _ => self.bus.read(address),
        // };
        //
        // // Operate
        // match instruction.0 {
        //     Operation::AND => {
        //         self.state.a = self.state.a & data;
        //         self.state.status.set(Flags::ZERO, self.state.a == 0x00);
        //         self.state
        //             .status
        //             .set(Flags::NEGATIVE, self.state.a & 0x80 != 0);
        //         self.cycles_remaining += 1;
        //     }
        //
        //     Operation::BCS => {
        //         if self.state.status.contains(Flags::CARRY) {
        //             self.cycles_remaining += 1;
        //
        //             // TODO: Should this be a wrapping_add?
        //             // TODO: This gets bit mangled in the get_absolute_address function to be signed.  Should we rather handle it here?
        //             let address = self.state.program_counter + address;
        //
        //             if address & 0xFF00 != self.state.program_counter & 0xFF {
        //                 self.cycles_remaining += 1;
        //             }
        //
        //             self.state.program_counter = address;
        //         }
        //     }
        //
        //     Operation::PHA => {
        //         self.push(self.state.a);
        //     }
        //
        //     Operation::PLA => {
        //         self.state.a = self.pop();
        //     }
        //
        //     Operation::CLC => {
        //         self.state.status.set(Flags::CARRY, false);
        //     }
        //
        //     Operation::CLI => {
        //         self.state.status.set(Flags::INTERRUPTS, false);
        //     }
        //
        //     Operation::CLD => {
        //         self.state.status.set(Flags::DECIMAL, false);
        //     }
        //
        //     Operation::CLV => {
        //         self.state.status.set(Flags::OVERFLOW, false);
        //     }
        //
        //     Operation::SEC => {
        //         self.state.status.set(Flags::CARRY, true);
        //     }
        //
        //     Operation::SEI => {
        //         self.state.status.set(Flags::INTERRUPTS, true);
        //     }
        //
        //     Operation::SED => {
        //         self.state.status.set(Flags::DECIMAL, true);
        //     }
        //
        //     // Return from interrupt.
        //     Operation::RTI => {
        //         self.state.status = Flags::from_bits_truncate(self.pop());
        //         self.state.status.set(Flags::BREAK, false);
        //         self.state.status.set(Flags::UNUSED, false);
        //
        //         let lo = self.pop();
        //         let hi = self.pop();
        //         self.state.program_counter = u16::from_le_bytes([lo, hi]);
        //     }
        //
        //     _ => {
        //         log::warn!("Invalid operation: {:?}", instruction);
        //     }
        // }
    }

    fn _push(&mut self, value: u8) {
        self.bus
            .write(self.state._stack_pointer.wrapping_add(0x0100), value);
        self.state._stack_pointer = self.state._stack_pointer.wrapping_sub(1);
    }

    fn _pop(&mut self) -> u8 {
        self.state._stack_pointer = self.state._stack_pointer.wrapping_add(1);
        let value = self
            .bus
            .read(self.state._stack_pointer.wrapping_add(0x0100));
        self.state.flags.set(Flags::ZERO, self.state.a == 0x00);
        self.state
            .flags
            .set(Flags::NEGATIVE, self.state.a & 0x80 != 0);
        value
    }

    fn get_operand_value(&self, operand: &Operand) -> (u8, u8) {
        match operand {
            Operand::None => unreachable!("No value available for Operand::None."),

            Operand::Immediate(value) => (*value, 0),

            Operand::Relative(_) => {
                unreachable!("Relative operands can not be fetched with get_operand_value.")
            }

            Operand::Absolute(address, offset) => {
                let offset_address = address.wrapping_add(match offset {
                    Offset::None => 0,
                    Offset::X => self.state.x as u16,
                    Offset::Y => self.state.y as u16,
                });

                let value = self.bus.read(offset_address);
                let cycles_required = if !same_page(*address, offset_address) {
                    1
                } else {
                    0
                };

                (value, cycles_required)
            }

            Operand::ZeroPage(address, offset) => {
                let address = (*address as u16) << 8;

                let address = address.wrapping_add(match offset {
                    Offset::None => 0,
                    Offset::X => self.state.x as u16,
                    Offset::Y => self.state.y as u16,
                });

                let value = self.bus.read(address);

                (value, 0)
            }

            Operand::Indirect(pointer, _offset) => {
                let pointer = *pointer;

                // TODO: Take offset into account.

                let address = if pointer & 0x00FF == 0xFF {
                    let lo = self.bus.read(pointer);
                    let hi = self.bus.read(pointer & 0xFF00);
                    u16::from_le_bytes([lo, hi])
                } else {
                    let lo = self.bus.read(pointer);
                    let hi = self.bus.read(pointer + 1);
                    u16::from_le_bytes([lo, hi])
                };

                let value = self.bus.read(address);

                (value, 0)
            }
        }
    }

    fn _get_relative_address(&self, address: u16, operand: &Operand) -> u16 {
        if let Operand::Relative(offset) = *operand {
            address.wrapping_add(offset as u16)
        } else {
            address
        }
    }

    // fn get_absolute_address(&mut self, addressing_mode: AddressingMode) -> (u16, u8) {
    //     match addressing_mode {
    //         AddressingMode::Implied => {
    //             unreachable!("Absolute address for implied addressing mode is invalid.")
    //         }
    //
    //         AddressingMode::Immediate => (self.state.program_counter, 0),
    //
    //         AddressingMode::Zp0 => {
    //             let offset = u16::from_le_bytes([self.consume(), 0]);
    //             (offset, 0)
    //         }
    //
    //         AddressingMode::Zpx => {
    //             let offset =
    //                 u16::from_le_bytes([self.consume(), 0]).wrapping_add(self.state.x as u16);
    //             (offset, 0)
    //         }
    //
    //         AddressingMode::Zpy => {
    //             let offset =
    //                 u16::from_le_bytes([self.consume(), 0]).wrapping_add(self.state.y as u16);
    //             (offset, 0)
    //         }
    //
    //         AddressingMode::Relative => {
    //             let mut relative = u16::from_le_bytes([self.consume(), 0]);
    //
    //             // If the number is negative, then make sure it's negative as a 16-bit value as
    //             // well.
    //             if relative & 0x0080 != 0 {
    //                 relative |= 0xFF00;
    //             }
    //
    //             (relative, 0)
    //         }
    //
    //         AddressingMode::Absolute => {
    //             let address = u16::from_le_bytes([self.consume(), self.consume()]);
    //
    //             (address, 0)
    //         }
    //
    //         AddressingMode::Abx => {
    //             let lo = self.consume();
    //             let hi = self.consume();
    //             let mut address = u16::from_le_bytes([lo, hi]);
    //             address = address.wrapping_add(self.state.x as u16);
    //
    //             let cycles = if address & 0xFF00 != (hi as u16) << 8 {
    //                 1
    //             } else {
    //                 0
    //             };
    //
    //             (address, cycles)
    //         }
    //
    //         AddressingMode::Aby => {
    //             let lo = self.consume();
    //             let hi = self.consume();
    //
    //             let mut address = u16::from_le_bytes([lo, hi]);
    //             address = address.wrapping_add(self.state.y as u16);
    //
    //             let cycles = if address & 0xFF00 != (hi as u16) << 8 {
    //                 1
    //             } else {
    //                 0
    //             };
    //
    //             (address, cycles)
    //         }
    //
    //         AddressingMode::Indirect => {
    //             let lo = self.consume();
    //             let hi = self.consume();
    //
    //             let pointer = u16::from_le_bytes([lo, hi]);
    //
    //             let address = if lo == 0xFF {
    //                 let lo = self.bus.read(pointer);
    //                 let hi = self.bus.read(pointer & 0xFF00);
    //                 u16::from_le_bytes([lo, hi])
    //             } else {
    //                 let lo = self.bus.read(pointer);
    //                 let hi = self.bus.read(pointer + 1);
    //
    //                 u16::from_le_bytes([lo, hi])
    //             };
    //
    //             (address, 0)
    //         }
    //
    //         AddressingMode::Izx => {
    //             let t = self.consume();
    //
    //             let lo = self.bus.read(t.wrapping_add(self.state.x) as u16);
    //             let hi = self
    //                 .bus
    //                 .read(t.wrapping_add(self.state.x.wrapping_add(1)) as u16);
    //
    //             let address = u16::from_le_bytes([lo, hi]);
    //
    //             (address, 0)
    //         }
    //
    //         AddressingMode::Izy => {
    //             let t = self.consume();
    //
    //             let lo = self.bus.read(t as u16);
    //             let hi = self.bus.read(t.wrapping_add(1) as u16);
    //
    //             let address = u16::from_le_bytes([lo, hi]).wrapping_add(self.state.y as u16);
    //
    //             let cycles = if address & 0xFF00 != (hi as u16) << 8 {
    //                 1
    //             } else {
    //                 0
    //             };
    //
    //             (address, cycles)
    //         }
    //     }
    // }
}

impl<B: Bus> Iterator for Cpu<B> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.bus.read(self.state.program_counter);
        self.state.program_counter += 1;
        Some(value)
    }
}

struct CpuBus {
    ram: RandomAccessMemory,
    cartridge: Cartridge,
}

impl CpuBus {
    pub fn new(cartridge: Cartridge) -> Self {
        Self {
            ram: RandomAccessMemory::with_capacity(0x0800),
            cartridge,
        }
    }
}

impl Bus for CpuBus {
    fn read(&self, address: u16) -> u8 {
        if address < 0x0800 {
            self.ram.read(address)
        } else {
            log::warn!("Reading from unmapped memory location: ${:#06X}", address);
            // 0
            0x4C
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        if address < 0x0800 {
            self.ram.write(address, value)
        } else {
            log::warn!(
                "Writing to unmapped memory location: ${:#06X} = #{:#04X}",
                address,
                value
            );
        }
    }
}

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

trait ClockSignal {
    fn step(&mut self);
}

trait Processor: ClockSignal {
    fn reset(&mut self);
    fn interrupt(&mut self);
}

struct Clock {
    frequency: f64,
    processors: Vec<Rc<RefCell<dyn Processor>>>,
}

impl Clock {
    pub fn new(frequency: f64) -> Self {
        Self {
            frequency,
            processors: vec![],
        }
    }

    pub fn connect_processor(&mut self, processor: Rc<RefCell<dyn Processor>>) {
        self.processors.push(processor);
    }

    pub fn run(&mut self) {
        let between_steps = 1.0 / self.frequency;
        dbg!(between_steps);
        loop {
            let start_time = std::time::Instant::now();
            self.step();
            let mut first = true;
            loop {
                let elapsed = start_time.elapsed().as_secs_f64();
                if elapsed >= between_steps {
                    if first {
                        panic!(
                            "Elapsed time of {} took longer than expected frequency of {}",
                            elapsed, between_steps
                        );
                    }
                    break;
                } else {
                    first = false;
                }
            }
        }
    }

    fn step(&mut self) {
        self.processors.iter().for_each(|p| p.borrow_mut().step());
    }
}

fn main() {
    pretty_env_logger::init();

    let bytes = include_bytes!("../nestest.nes");
    let cartridge = cartridge::ines::load(std::io::Cursor::new(bytes)).unwrap();

    let bus = CpuBus::new(cartridge);

    let cpu = Cpu::new(bus);

    let mut clock = Clock::new(1.0);
    clock.connect_processor(Rc::new(RefCell::new(cpu)));
    clock.run();
}
