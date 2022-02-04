PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000

  .org $8000

main:
  ldx #$ff        ; initialize the stack pointer
  txs

  lda #%11111111  ; all pins port B to output
  sta DDRB

  lda #%11100000  ; top 3 pins of port A to output
  sta DDRA

  lda #%00111000  ; set 8-bit mode, 2-line display, 5x8 font
  jsr lcd_instruction

  lda #%00001100  ; display on, cursor off, blink off
  jsr lcd_instruction

  lda #%00000110  ; increment cursor, do not shift display
  jsr lcd_instruction

  jsr lcd_clear

  ; initialize the zero page with incrementing bytes
  lda $00
  ldx $00
clear_loop:
  txa
  sta $00, x
  inx
  bne clear_loop

  lda #$00
  sta 42
  lda #$22
  sta 43
  lda #$44
  sta 44
  lda #$66
  sta 45

  lda #$11
  sta 52
  lda #$22
  sta 53
  lda #$33
  sta 54
  lda #$44
  sta 55

  ldx #42
  ldy #52
  jsr add_u32
  jsr print_hex_u32

end_loop:
  jmp end_loop

; *X += *Y, preserves registers
add_u32:
  clc
  lda $00, x
  adc $00, y
  sta $00, x
  lda $01, x
  adc $01, y
  sta $01, x
  lda $02, x
  adc $02, y
  sta $02, x
  lda $03, x
  adc $03, y
  sta $03, x
  rts

; preserves registers
lcd_wait:
  pha
  lda #%00000000  ; all pins port B to input
  sta DDRB
lcdbusy:
  lda #RW
  sta PORTA
  lda #(RW | E)
  sta PORTA
  lda PORTB
  and #%10000000
  bne lcdbusy

  lda #RW
  sta PORTA
  lda #%11111111  ; all pins port B to output
  sta DDRB
  pla
  rts

; reads A
lcd_instruction:
  jsr lcd_wait
  sta PORTB
  lda #0          ; clear RS/RW/E
  sta PORTA
  lda #E          ; set E
  sta PORTA
  lda #0          ; clear RS/RW/E
  sta PORTA
  rts

lcd_clear:
  lda #%00000001  ; clear the display
  jsr lcd_instruction
  rts

lcd_line_two:
  lda #%10101000  ; DDRAM address to byte 40
  jsr lcd_instruction
  rts

; reads A
print_char:
  jsr lcd_wait
  sta PORTB
  lda #RS         ; set RS, clear RW/E
  sta PORTA
  lda #(RS | E)   ; set RS and E
  sta PORTA
  lda #RS         ; set RS, clear RW/E
  sta PORTA
  rts

; str pointer at $00
print_str:
  ldy #0
print_str_loop:
  lda ($0), y
  beq print_str_end
  jsr print_char
  iny
  jmp print_str_loop
print_str_end:
  rts

; reads A
print_hex_nibble:
  cmp #10
  bmi print_hex_nibble_0_10
  sec
  sbc #10
  clc
  adc #"a"
  jmp print_hex_nibble_end
print_hex_nibble_0_10:
  clc
  adc #"0"
print_hex_nibble_end:
  jsr print_char
  rts

; reads A
print_hex_byte:
  pha
  lsr
  lsr
  lsr
  lsr
  jsr print_hex_nibble
  pla
  and #%00001111
  jsr print_hex_nibble
  rts

; prints *X
print_hex_u32:
  lda #"0"
  jsr print_char
  lda #"x"
  jsr print_char
  lda $03, x
  jsr print_hex_byte
  lda $02, x
  jsr print_hex_byte
  lda $01, x
  jsr print_hex_byte
  lda $00, x
  jsr print_hex_byte
  rts

; preserves registers
pause:
  pha
  txa
  pha
  tya
  pha

  ; tweak A here to adjust the pause
  lda #1
  ldx #0
  ldy #0
pause_loop:
  ; decrement X
  dex
  bne pause_loop
  ; if X reached 0, decrement Y
  dey
  bne pause_loop
  ; if Y reached 0, decrement A
  sec
  sbc #1
  bne pause_loop
  ; if A reached 0, we're done

  pla
  tay
  pla
  tax
  pla
  rts

  .org $fffc
  .word main
  .word 0
