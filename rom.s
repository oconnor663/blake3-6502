PORTB = $6000
DDRB = $6002

  .org $8000

reset:
  lda #%11111111
  sta DDRB

  lda #1
  ldx #1

loop:
  sta PORTB

; If X is 1, rotate A left. Otherwise, rotate A right.
  tay
  txa
  cmp #1
  bne rotate_right
  tya
  clc
  rol
  jmp rotate_end
rotate_right:
  tya
  clc
  ror
rotate_end:

; If A reaches 128, set X to 0.
  cmp #%00000001
  bne not_1
  ldx #1
not_1:
; If A reaches 1, set X to 1.
  cmp #%10000000
  bne not_128
  ldx #0
not_128:

  jmp loop

  .org $fffc
  .word reset
  .word 0
