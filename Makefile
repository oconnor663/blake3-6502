flash: a.out
	minipro --device AT28C256 --write a.out

dump: a.out
	hexdump -C a.out

a.out: rom.s
	vasm6502_oldstyle -Fbin -dotdir -esc rom.s
