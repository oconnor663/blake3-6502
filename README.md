# BLAKE3 in 6502 assembly

[BLAKE3](https://blake3.io) is a cryptographic hash function. The [MOS
6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) is the classic 8-bit
microprocessor that powered the Apple II, the Commodore 64, and the NES
console. This implementation of BLAKE3 runs on [Ben Eater's 6502 breadboard
computer](https://eater.net/6502). Source code is in [`rom.s`](rom.s).

![photo](photo.jpg)

The program runs over the [BLAKE3 test
vectors](https://github.com/BLAKE3-team/BLAKE3/blob/master/test_vectors/test_vectors.json)
and hashes each one. The output shown in the photo is for the [test input of
length 0x1c01
(7169)](https://github.com/BLAKE3-team/BLAKE3/blob/4e84c8c7ae3da71d3aff5ba54d8ffa39a9b90fa0/test_vectors/test_vectors.json#L181-L182).
