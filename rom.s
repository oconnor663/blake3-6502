; IO controller constants
PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003
E  = %10000000
RW = %01000000
RS = %00100000

; G function constants
A  = $02
B  = $03
C  = $04
D  = $05
MX = $06
MY = $07

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
  lda #0
  ldx #0
init_zp_loop:
  txa
  sta $00, x
  inx
  bne init_zp_loop

  ; use the 24 bytes starting at address 100 for a, b, c, d, mx, my
  ; these have incrementing values starting at 100
  ; store their addresses in $02..08
  lda #100
  sta A
  lda #104
  sta B
  lda #108
  sta C
  lda #112
  sta D
  lda #116
  sta MX
  lda #120
  sta MY

  ; mix these 6 ints with g()
  jsr g

  ; print a and b
  ldx A
  jsr print_hex_u32
  jsr lcd_line_two
  ldx B
  jsr print_hex_u32

end_loop:
  jmp end_loop

; $02..08 are 1-byte word pointers: (a, b, c, d, mx, my)
; note that $00..02 are needed by ror12_u32
g:
  ; a = a + b + mx
  ldx A
  ldy B
  jsr add_u32
  ldy MX
  jsr add_u32

  ; d = (d ^ a) >>> 16
  ldx D
  ldy A
  jsr xor_u32
  jsr ror16_u32

  ; c = c + d
  ldx C
  ldy D
  jsr add_u32

  ; b = (b ^ c) >>> 12
  ldx B
  ldy C
  jsr xor_u32
  jsr ror12_u32

  ; a = a + b + my
  ldx A
  ldy B
  jsr add_u32
  ldy MY
  jsr add_u32

  ; d = (d ^ a) >>> 8
  ldx D
  ldy A
  jsr xor_u32
  jsr ror8_u32

  ; c = c + d
  ldx C
  ldy D
  jsr add_u32

  ; b = (b ^ c) >>> 7
  ldx B
  ldy C
  jsr xor_u32
  jsr ror7_u32

  rts

; *X >>>= 16, preserves X
ror16_u32:
  ; swap *(X + 0) and *(X + 2)
  lda $00, x
  tay
  lda $02, x
  sta $00, x
  tya
  sta $02, x
  ; swap *(X + 1) and *(X + 3)
  lda $01, x
  tay
  lda $03, x
  sta $01, x
  tya
  sta $03, x
  rts

; *X >>>= 12, preserves registers, bytes $00..02 are scratch
ror12_u32:
  ; copy byte0 and byte1
  lda $00, x
  sta $00
  lda $01, x
  sta $01

  ; write byte0 lower nibble
  lda $01, x
  lsr
  lsr
  lsr
  lsr
  sta $00, x
  ; write byte0 upper nibble
  lda $02, x
  asl
  asl
  asl
  asl
  ora $00, x
  sta $00, x

  ; write byte1 lower nibble
  lda $02, x
  lsr
  lsr
  lsr
  lsr
  sta $01, x
  ; write byte1 upper nibble
  lda $03, x
  asl
  asl
  asl
  asl
  ora $01, x
  sta $01, x

  ; write byte2 lower nibble
  lda $03, x
  lsr
  lsr
  lsr
  lsr
  sta $02, x
  ; write byte2 upper nibble, using scratch
  lda $00
  asl
  asl
  asl
  asl
  ora $02, x
  sta $02, x

  ; write byte2 lower nibble, using scratch
  lda $00
  lsr
  lsr
  lsr
  lsr
  sta $03, x
  ; write byte2 upper nibble, using scratch
  lda $01
  asl
  asl
  asl
  asl
  ora $03, x
  sta $03, x

  rts

; *X >>>= 8, preserves X
ror8_u32:
  ; stash byte0 in Y
  lda $00, x
  tay

  ; move the top three bytes down
  lda $01, x
  sta $00, x
  lda $02, x
  sta $01, x
  lda $03, x
  sta $02, x

  ; retrieve byte 0 from Y and write it
  tya
  sta $03, x

  rts

; *X >>>= 7, preserves X
ror7_u32:
  ; stash byte0 in Y
  lda $00, x
  tay

  ; load byte1 and distribute its bits
  lda $01, x
  asl         ; the high bit is now in C
  sta $00, x
  lda #0
  adc #0      ; retrieve the high bit from C
  sta $01, x

  ; load byte2 and distribute its bits
  lda $02, x
  asl         ; the high bit is now in C
  ora $01, x
  sta $01, x
  lda #0
  adc #0      ; retrieve the high bit
  sta $02, x

  ; load byte3 and distribute its bits
  lda $03, x
  asl         ; the high bit is now in C
  ora $02, x
  sta $02, x
  lda #0
  adc #0      ; retrieve the high bit
  sta $03, x

  ; retrieve byte0 from Y and distribute its bits
  tya
  asl         ; the high bit is now in C
  ora $03, x
  sta $03, x
  lda #0
  adc #0      ; retrieve the high bit
  ora $00, x
  sta $00, x

  rts

; *X += *Y, preserves X and Y
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

; *X ^= *Y, preserves X and Y
xor_u32:
  lda $00, x
  eor $00, y
  sta $00, x
  lda $01, x
  eor $01, y
  sta $01, x
  lda $02, x
  eor $02, y
  sta $02, x
  lda $03, x
  eor $03, y
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
  lda #4
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
