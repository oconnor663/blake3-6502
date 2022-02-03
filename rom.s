PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000

  .org $8000

reset:
  ldx #$ff        ; initialize the stack pointer
  txs

  lda #%11111111  ; all pins port B to output
  sta DDRB

  lda #%11100000  ; top 3 pins of port A to output
  sta DDRA

  lda #%00000001  ; clear the display
  jsr lcd_instruction

  lda #%00111000  ; set 8-bit mode, 2-line display, 5x8 font
  jsr lcd_instruction

  lda #%00001110  ; display on, cursor on, blink off
  jsr lcd_instruction

  lda #%00000110  ; increment cursor, do not shift display
  jsr lcd_instruction

  lda #"H"
  jsr print_char

  lda #"e"
  jsr print_char

  lda #"l"
  jsr print_char

  lda #"l"
  jsr print_char

  lda #"o"
  jsr print_char

  lda #","
  jsr print_char

  lda #" "
  jsr print_char

  lda #"w"
  jsr print_char

  lda #"o"
  jsr print_char

  lda #"r"
  jsr print_char

  lda #"l"
  jsr print_char

  lda #"d"
  jsr print_char

  lda #"!"
  jsr print_char

loop:
  jmp loop

lcd_instruction:
  sta PORTB
  lda #0          ; clear RS/RW/E
  sta PORTA
  lda #E          ; set E
  sta PORTA
  lda #0          ; clear RS/RW/E
  sta PORTA
  rts

print_char:
  sta PORTB
  lda #RS         ; set RS, clear RW/E
  sta PORTA
  lda #(RS | E)   ; set RS and E
  sta PORTA
  lda #RS         ; set RS, clear RW/E
  sta PORTA
  rts

  .org $fffc
  .word reset
  .word 0
