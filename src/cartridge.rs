pub mod ines {
    use super::{Cartridge, LoadError};
    use bitflags::bitflags;
    use std::io::{Read, Seek, SeekFrom};

    bitflags! {
        /// Flags for the 6th byte in the iNES header. (bits 4-7 is the lower nibble of the mapper
        /// number)
        struct MapperFlags1 : u8 {
            /// Mirroring: 0: horizontal (vertical arrangement) (CIRAM A10 = PPU A11)
            ///            1: vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
            const IS_VERTICAL = 1 << 0;

            /// 1: Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
            const BATTERY = 1 << 1;

            /// 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
            const TRAINER = 1 << 2;

            /// 1: Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
            const IGNORE_MIRRORING = 1 << 3;
        }
    }

    #[derive(Debug)]
    struct Header {
        id: [u8; 4],
        program_memory_chunks: u8,
        character_memory_chunks: u8,
        mapper_flags_1: MapperFlags1,
        _mapper_2: u8,
        _program_memory_size: u8,
        _tv_system_1: u8,
        _tv_system_2: u8,
        _unused: [u8; 5],
    }

    impl Header {
        fn _mapper_id(&self) -> u8 {
            self.mapper_flags_1.bits() >> 4
        }
    }

    pub fn load(mut data: impl Read + Seek) -> Result<Cartridge, LoadError> {
        let header = {
            let mut header: Header = unsafe { std::mem::zeroed() };
            let header_size = std::mem::size_of::<Header>();
            let header_slice = unsafe {
                std::slice::from_raw_parts_mut(&mut header as *mut _ as *mut u8, header_size)
            };
            data.read_exact(header_slice)?;
            header
        };

        if header.id != [b'N', b'E', b'S', 0x1A] {
            return Err(LoadError::InvalidBinaryFormat);
        }

        // If the image contains a trainer, then we just skip it.
        if header.mapper_flags_1.contains(MapperFlags1::TRAINER) {
            data.seek(SeekFrom::Current(512))?;
        }

        // let mapper_id = header.mapper_id();

        // let hardware_mirror = header.mapper_flags_1.contains(MapperFlags1::IS_VERTICAL);

        let program_memory = {
            let mut memory = vec![0_u8; 0x4000 * header.program_memory_chunks as usize];
            data.read_exact(memory.as_mut_slice())?;
            memory
        };

        let character_memory = {
            let mut memory = vec![0_u8; 0x2000 * header.character_memory_chunks as usize];
            data.read_exact(memory.as_mut_slice())?;
            memory
        };

        Ok(Cartridge {
            program_memory,
            character_memory,
        })
    }
}

pub struct Cartridge {
    program_memory: Vec<u8>,
    character_memory: Vec<u8>,
}

#[derive(Debug)]
pub enum LoadError {
    IoError(std::io::Error),
    InvalidBinaryFormat,
}

impl From<std::io::Error> for LoadError {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}
