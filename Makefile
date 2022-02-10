flash: rom.bin
	minipro --device AT28C256 --write rom.bin

dump: rom.bin
	hexdump -C rom.bin

# Assembling the binary requires the vasm assembler. `vasm6502_oldstyle` is how
# the relevant configuration gets installed on Arch Linux, but on other OSs (or
# if you compile Vasm yourself) it probably has different names.
# HACK: Git will usually checkout `rom.s` and `rom.bin` with the same
# timestamp, so Make shouldn't try to rebuild `rom.bin` if you don't touch
# `rom.s`. However, this might not be entirely reliable...
rom.bin: rom.s
	vasm6502_oldstyle -Fbin -dotdir -esc rom.s -o rom.bin

.PHONY: emulate
emulate: rom.bin
	cd emulate && cargo run --release ../rom.bin
