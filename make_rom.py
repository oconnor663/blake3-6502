#! /usr/bin/env python3

from os import path
import sys

HERE = path.dirname(__file__)
SIZE = 32768

# fmt: off
code = bytearray([
    # Set the IO controller's B register to output.
    0xa9, 0xff,        # lda #$ff
    0x8d, 0x02, 0x60,  # sta $6002

    # Initialize A to 0.
    0xa9, 0x00,        # lda #$00

    # Clear carry and rotate A. This is the loop start.
    0x18,              # clc
    0x2a,              # rol A

    # If A is 0, set it to 1.
    0xc9, 0x00,        # cmp #0
    0xd0, 0x02,        # bne #2
    0xa9, 0x01,        # lda #$01

    # Write A to the IO controller's B register.
    0x8d, 0x00, 0x60,  # sta $6000

    # Jump to loop start.
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
