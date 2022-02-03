PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000

  .org $8000

reset:
  lda #%11111111  ; port B to output
  sta DDRB

  lda #%11100000  ; top 3 pins of port A to output
  sta DDRA

  lda #%00000001  ; clear the display
  sta PORTB

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #E          ; set E
  sta PORTA

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #%00111000  ; set 8-bit mode, 2-line display, 5x8 font
  sta PORTB

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #E          ; set E
  sta PORTA

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #%00001110  ; display on, cursor on, blink off
  sta PORTB

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #E          ; set E
  sta PORTA

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #%00000110  ; increment cursor, do not shift display
  sta PORTB

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #E          ; set E
  sta PORTA

  lda #0          ; clear RS/RW/E
  sta PORTA

  lda #"H"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"e"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"l"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"l"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"o"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #","
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #" "
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"w"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"o"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"r"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"l"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"d"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #"!"
  sta PORTB

  lda #RS         ; set RS, clear RW/E
  sta PORTA

  lda #(RS | E)   ; set RS and E
  sta PORTA

  lda #RS         ; set RS, clear RW/E
  sta PORTA

loop:
  jmp loop

  .org $fffc
  .word reset
  .word 0
