flash: rom.bin
	minipro --device AT28C256 --write rom.bin

dump: rom.bin
	hexdump -C rom.bin

rom.bin: rom.s
	vasm6502_oldstyle -Fbin -dotdir -esc rom.s -o rom.bin

.PHONY: emulate
emulate: rom.bin
	cd emulate && cargo run --release ../rom.bin
