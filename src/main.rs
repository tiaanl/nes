mod cartridge;
mod cpu;
mod decoder;
mod instruction;
mod mapper;

use crate::cartridge::Cartridge;
use crate::instruction::{Instruction, Offset, Operand, Operation};
use std::cell::RefCell;
use std::rc::Rc;

pub trait Bus {
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

struct CpuBus {
    ram: RandomAccessMemory,
    cartridge: Cartridge,

    ppu_controls_registers: [u8; 8],
}

impl CpuBus {
    pub fn new(cartridge: Cartridge) -> Self {
        Self {
            ram: RandomAccessMemory::with_capacity(0x0800),
            cartridge,
            ppu_controls_registers: [0; 8],
        }
    }
}

impl Bus for CpuBus {
    fn read(&self, address: u16) -> u8 {
        let value = match address {
            0x0000..=0x1FFF => self.ram.read(address & 0x07FF),

            0x2000..=0x3FFF => {
                let value = self.ppu_controls_registers[(address & 0x07) as usize];
                log::trace!(
                    "Read {:02X} from PPU control register at ${:04X}",
                    value,
                    address
                );
                value
            }

            0x4020..=0xFFFF => self.cartridge.cpu_read(address),

            _ => unreachable!("Invalid read from address {:#06X}", address),
        };

        // log::trace!("Read {:02X} from CPU bus at ${:04X}", value, address);

        value
    }

    fn write(&mut self, address: u16, value: u8) {
        if address == 0x0000 && value != 0x00 {
            panic!("writing to 0x0000: {:#04X}", value);
        }

        // log::trace!("Writing ${:02X} to CPU bus at ${:04X}", value, address);

        match address {
            0x0000..=0x1FFF => self.ram.write(address & 0x07FF, value),

            0x2000..=0x3FFF => {
                log::trace!(
                    "Writing ${:02X} to PPU control register at ${:04X}",
                    value,
                    address
                );
                self.ppu_controls_registers[(address & 0x07) as usize] = value
            }

            0x4020..=0xFFFF => self.cartridge.cpu_write(address, value),

            _ => unreachable!(),
        }
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
    _frequency: f64,
    processors: Vec<Rc<RefCell<dyn Processor>>>,
}

impl Clock {
    pub fn new(frequency: f64) -> Self {
        Self {
            _frequency: frequency,
            processors: vec![],
        }
    }

    pub fn connect_processor(&mut self, processor: Rc<RefCell<dyn Processor>>) {
        self.processors.push(processor);
    }

    pub fn run(&mut self) {
        // let between_steps = 1.0 / self.frequency;
        loop {
            //let start_time = std::time::Instant::now();
            self.step();
            // let mut first = true;
            // loop {
            //     let elapsed = start_time.elapsed().as_secs_f64();
            //     if elapsed >= between_steps {
            //         if first {
            //             panic!(
            //                 "Elapsed time of {} took longer than expected frequency of {}",
            //                 elapsed, between_steps
            //             );
            //         }
            //         break;
            //     } else {
            //         first = false;
            //     }
            // }
        }
    }

    fn step(&mut self) {
        self.processors.iter().for_each(|p| p.borrow_mut().step());
    }
}

fn main() {
    pretty_env_logger::init();

    let cartridge = {
        let data = std::io::Cursor::new(include_bytes!("../nestest.nes"));
        cartridge::ines::load(data).unwrap()
    };

    let cpu = {
        let bus = CpuBus::new(cartridge);
        let mut cpu = cpu::Cpu::new(bus);

        // cpu.jump_to(0xC000);
        //
        // while let Some(decoded) = decode_instruction(&mut cpu) {
        //     println!("{:04X} {}", cpu.state.program_counter, &decoded.instruction);
        //     if cpu.state.program_counter == 0xFFFF {
        //         break;
        //     }
        // }

        // cpu.jump_to(0xC8B8);
        cpu.jump_to(0xC000);

        cpu
    };

    let mut clock = Clock::new(100.0);
    clock.connect_processor(Rc::new(RefCell::new(cpu)));
    clock.run();
}
