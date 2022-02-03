#! /usr/bin/env python3

from os import path
import sys

HERE = path.dirname(__file__)
SIZE = 32768

# fmt: off
code = bytearray([
    0xa9, 0xff,        # lda #$ff
    0x8d, 0x02, 0x60,  # sta $6002

    0xa9, 0x00,        # lda #$00

    0x1a,              # inc A
    0x8d, 0x00, 0x60,  # sta $6000

    0x4c, 0x07, 0x80,  # jmp $8007
])
# fmt: on

rom = bytearray([0xEA] * SIZE)

rom[: len(code)] = code

# reset vector
rom[0x7FFC] = 0x00
rom[0x7FFD] = 0x80

with open(path.join(HERE, "rom.bin"), "wb") as f:
    f.write(rom)
