#! /usr/bin/env python3

from os import path
import sys

HERE = path.dirname(__file__)

with open(path.join(HERE, "rom.bin"), "wb") as f:
    f.write(b"\xea" * 32768)
