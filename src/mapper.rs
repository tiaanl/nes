pub trait Mapper {
    fn cpu_read(&self, address: u16) -> Option<u16>;
    fn cpu_write(&self, address: u16) -> Option<u16>;

    fn ppu_read(&self, address: u16) -> Option<u16>;
    fn ppu_write(&self, address: u16) -> Option<u16>;
}

pub struct Mapper000 {
    program_memory_banks: u8,
    character_memory_banks: u8,
}

impl Mapper000 {
    pub fn new(program_memory_banks: u8, character_memory_banks: u8) -> Self {
        Self {
            program_memory_banks,
            character_memory_banks,
        }
    }
}

impl Mapper for Mapper000 {
    fn cpu_read(&self, address: u16) -> Option<u16> {
        match address {
            addr @ 0x8000..=0xFFFF => {
                if self.program_memory_banks > 1 {
                    Some(addr & 0x7FFF)
                } else {
                    Some(addr & 0x3FFF)
                }
            }

            _ => None,
        }
    }

    fn cpu_write(&self, address: u16) -> Option<u16> {
        match address {
            addr @ 0x8000..=0xFFFF => Some(if self.program_memory_banks > 1 {
                addr & 0x7FFF
            } else {
                addr & 0x3FFF
            }),

            _ => None,
        }
    }

    fn ppu_read(&self, address: u16) -> Option<u16> {
        match address {
            addr @ 0x0000..=0x1FFF => Some(addr),

            _ => None,
        }
    }

    fn ppu_write(&self, address: u16) -> Option<u16> {
        match address {
            addr @ 0x0000..=0x1FFF if self.character_memory_banks == 0 => Some(addr),

            _ => None,
        }
    }
}
