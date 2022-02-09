use emulator_6502::{Interface6502, MOS6502};
use std::io::prelude::*;

const RAM_SIZE: usize = 1 << 14; // 16 KiB
const ROM_SIZE: usize = 1 << 15; // 32 KiB

const PORTB_ADDR: u16 = 0x6000;
const PORTA_ADDR: u16 = 0x6001;
const DDRB_ADDR: u16 = 0x6002;
const DDRA_ADDR: u16 = 0x6003;

const LCD_ENABLE: u8 = 0b10000000;
const LCD_READ_WRITE: u8 = 0b01000000;
const LCD_REGISTER_SELECT: u8 = 0b00100000;

// The memory layout of the Ben Eater 6502 breadboard computer looks like this:
//   0000..=3fff  RAM (16 KiB)
//   4000..=5fff  [unmapped]
//   6000         IO PORT B
//   6001         IO PORT A
//   6002         IO DDR B
//   6003         IO DDR A
//   6004..=7fff  [unmapped]
//   8000..=ffff  ROM (32 KiB)
struct Memory {
    ram: Box<[u8; 1 << 14]>,       // 0x0000 - 0x3fff
    io_port_b: u8,                 // 0x6000
    io_port_a: u8,                 // 0x6001
    data_direction_register_b: u8, // 0x6002
    data_direction_register_a: u8, // 0x6003
    rom: Box<[u8; 1 << 15]>,       // 0x8000 - 0xffff
}

impl Memory {
    fn new(rom: Box<[u8; ROM_SIZE]>) -> Self {
        Self {
            ram: vec![0; RAM_SIZE].into_boxed_slice().try_into().unwrap(),
            io_port_b: 0,
            io_port_a: 0,
            data_direction_register_a: 0,
            data_direction_register_b: 0,
            rom,
        }
    }

    fn lcd_enable(&self) -> bool {
        self.io_port_a & LCD_ENABLE != 0
    }

    fn lcd_read(&self) -> bool {
        self.io_port_a & LCD_READ_WRITE != 0
    }

    fn lcd_select(&self) -> bool {
        self.io_port_a & LCD_REGISTER_SELECT != 0
    }
}

impl Interface6502 for Memory {
    fn read(&mut self, address: u16) -> u8 {
        let data = match address {
            0x0000..=0x3fff => self.ram[address as usize],
            PORTB_ADDR => {
                assert_eq!(
                    0b00000000, self.data_direction_register_b,
                    "read from PORTB when data direction is set to write",
                );
                self.io_port_b
            }
            PORTA_ADDR => unimplemented!("read from PORTA"),
            DDRB_ADDR => unimplemented!("read from DDRB"),
            DDRA_ADDR => unimplemented!("read from DDRA"),
            0x8000..=0xffff => self.rom[address as usize - 0x8000],
            _ => panic!("address {address:04x} is unmapped"),
        };
        // println!("{address:04x}  r {data:02x}");
        data
    }

    fn write(&mut self, address: u16, data: u8) {
        match address {
            0x0000..=0x3fff => self.ram[address as usize] = data,
            PORTB_ADDR => {
                assert_eq!(
                    0b11111111, self.data_direction_register_b,
                    "write to PORTB when data direction is set to read",
                );
                self.io_port_b = data;
            }
            PORTA_ADDR => {
                assert_eq!(
                    0b11100000, self.data_direction_register_a,
                    "write to PORTA when data direction is set to read",
                );
                assert_eq!(0, data & 0b00011111, "unexpected bits set in PORTA");
                let previously_enabled = self.lcd_enable();
                self.io_port_a = data;
                // Only execute LCD instructions when the enable flag transitions from off to on.
                if !previously_enabled && self.lcd_enable() {
                    if self.lcd_read() {
                        assert!(
                            !self.lcd_select(),
                            "only reading the busy flag is implemented",
                        );
                        // Reading with register select bit set to 0 means reading the LCD busy
                        // flag. In this emulator the LCD is never busy, so the result of this read
                        // is always 0.
                        self.io_port_b = 0;
                    } else {
                        // This is a write.
                        if self.lcd_select() {
                            // This is a data write. Print it.
                            print!("{}", self.io_port_b as char);
                            std::io::stdout().flush().unwrap();
                        } else {
                            // This is an instruction write.
                            match self.io_port_b {
                                0b00000001 => {
                                    // LCD clear. Print a dividing line.
                                    println!();
                                    println!("----------------");
                                }
                                0b10101000 => {
                                    // Seek to display position 40. Start a new line.
                                    println!();
                                }
                                // Other instructions are ignored.
                                _ => {}
                            }
                        }
                    }
                }
            }
            DDRB_ADDR => {
                assert!(
                    data == 0b00000000 || data == 0b11111111,
                    "unexpected data direction for PORTB",
                );
                self.data_direction_register_b = data;
            }
            DDRA_ADDR => {
                assert_eq!(0b11100000, data, "unexpected data direction for PORTA");
                self.data_direction_register_a = data;
            }
            0x8000..=0xffff => panic!("address {address:04x} isn't writeable"),
            _ => panic!("address {address:04x} is unmapped"),
        }
        // println!("{address:04x}  W {data:02x}");
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage:  emulate <A.OUT>");
        std::process::exit(1);
    }
    let rom_path = &args[1];
    let rom_bytes = std::fs::read(rom_path).unwrap();
    assert_eq!(ROM_SIZE, rom_bytes.len(), "ROM is the wrong size");
    let mut memory = Memory::new(rom_bytes.into_boxed_slice().try_into().unwrap());
    let mut computer = MOS6502::new_reset_position(&mut memory);
    loop {
        computer.execute_instruction(&mut memory);
    }
}
