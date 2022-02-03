vasm:
	vasm6502_oldstyle -Fbin -dotdir rom.s && \
		hexdump -C a.out && \
		minipro --device AT28C256 --write a.out

raw:
	./make_rom.py && \
		hexdump -C rom.bin && \
		minipro --device AT28C256 --write rom.bin
