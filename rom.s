; ======================= Memory Map =======================
; 00-02   scratch space or pointer arguments
; 02-08   G function arguments
; 08-18   permuted pointers into the message block
; 18-80   [free]
; 80-c0   state matrix
; c0-100  message block
; ==========================================================

  ; Our ROM is mapped at address $8000.
  .org $8000

IV0_BYTES: .byte $67, $e6, $09, $6a
IV1_BYTES: .byte $85, $ae, $67, $bb
IV2_BYTES: .byte $72, $f3, $6e, $3c
IV3_BYTES: .byte $3a, $f5, $4f, $a5
IV4_BYTES: .byte $7f, $52, $0e, $51
IV5_BYTES: .byte $8c, $68, $05, $9b
IV6_BYTES: .byte $ab, $d9, $83, $1f
IV7_BYTES: .byte $19, $cd, $e0, $5b

; G function constants
; These are scratch space slots for zp pointers.
G_A  = $02
G_B  = $03
G_C  = $04
G_D  = $05
G_MX = $06
G_MY = $07
; An array of 16 bytes, $08..18, each of which points into
; COMPRESS_MSG
MPTRS = $08

; compression function constants
; The whole second half of the zero page is reserved for the
; compression function state and message block.
; $80..c0 (64 bytes) is the internal state.
H0  = $80
H1  = $84
H2  = $88
H3  = $8c
H4  = $90
H5  = $94
H6  = $98
H7  = $9c
H8  = $a0
H9  = $a4
H10 = $a8
H11 = $ac
H12 = $b0
H13 = $b4
H14 = $b8
H15 = $bc
; These constants refer to 4-byte words within the state.
COMPRESS_IV0 = H8
COMPRESS_IV1 = H9
COMPRESS_IV2 = H10
COMPRESS_IV3 = H11
COMPRESS_T0  = H12
COMPRESS_T1  = H13
COMPRESS_B   = H14
COMPRESS_D   = H15

; $c0..100 (64 bytes) is the message block.
COMPRESS_MSG = $c0

; compression function bitflags
; Note that keying and key derivation aren't supported here.
CHUNK_START = (1 << 0)
CHUNK_END   = (1 << 1)
PARENT      = (1 << 2)
ROOT        = (1 << 3)

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

  ; initialize the zero page with all null bytes
  lda #0
  ldx #0
init_zp_loop:
  dex
  sta $00, x
  bne init_zp_loop

  ; set things up to hash the empty message

  ; copy the IV bytes to H
  ldx #0
init_iv_loop:
  lda IV0_BYTES, x
  sta H0, x
  inx
  cpx #32
  bne init_iv_loop

  ; t0, t1, and b are already zeroed. set d here.
  lda #(CHUNK_START | CHUNK_END | ROOT)
  sta COMPRESS_D

  ; compress the empty block
  jsr compress

  jsr print_hash

end_loop:
  jmp end_loop

; This function (re)initializes the MPTRS array, and it sets
; the IV constants into the third row of the state. The caller
; needs to set H0..=H15, T0, T1, B, and D in the state, in
; addition to the COMPRESS_MSG block.
compress:
  ; reinitialize MPTRS with COMPRESS_MSG, +4, +8, ...
  lda #COMPRESS_MSG
  ldx #0
compress_init_mptrs_loop:
  sta MPTRS, x
  ; increment X by 1 and A by 4
  inx
  clc
  adc #4
  ; break when X reaches 16
  cpx #16
  bne compress_init_mptrs_loop

  ; set IV constants in the third row of the state
  ldx #0
compress_iv_consts_loop:
  lda IV0_BYTES, x
  sta COMPRESS_IV0, x
  inx
  cpx #16
  bne compress_iv_consts_loop

  ; compression rounds
  jsr round    ; round 1
  jsr permute
  jsr round    ; round 2
  jsr permute
  jsr round    ; round 3
  jsr permute
  jsr round    ; round 4
  jsr permute
  jsr round    ; round 5
  jsr permute
  jsr round    ; round 6
  jsr permute
  jsr round    ; round 7

  ; XOR the second half into the first half. Note that this
  ; implementation does *not* feed-forward into the second
  ; half, so extended outputs aren't supported.
  ldx #H0
  ldy #H8
  jsr xor_u32
  ldx #H1
  ldy #H9
  jsr xor_u32
  ldx #H2
  ldy #H10
  jsr xor_u32
  ldx #H3
  ldy #H11
  jsr xor_u32
  ldx #H4
  ldy #H12
  jsr xor_u32
  ldx #H5
  ldy #H13
  jsr xor_u32
  ldx #H6
  ldy #H14
  jsr xor_u32
  ldx #H7
  ldy #H15
  jsr xor_u32

  rts

permute:
  ; Rather than permuting the 32-bit words stored in the
  ; COMPRESS_MSG array, we permute the 8-bit pointers stored
  ; in the MPTRS array.
  ;
  ; The permutation is defined to be:
  ; 2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8
  ;
  ; If we walk those assignments, we get two loops:
  ; 0<-2<-3<-10<-12<-9<-11<-5<-0
  ; 1<-6<-4<-7<-13<-14<-15<-8<-1

  ; stash the m0 ptr in Y
  ldy MPTRS + 0

  ; walk the first assignment loop
  lda MPTRS + 2
  sta MPTRS + 0
  lda MPTRS + 3
  sta MPTRS + 2
  lda MPTRS + 10
  sta MPTRS + 3
  lda MPTRS + 12
  sta MPTRS + 10
  lda MPTRS + 9
  sta MPTRS + 12
  lda MPTRS + 11
  sta MPTRS + 9
  lda MPTRS + 5
  sta MPTRS + 11

  ; finish the first loop with Y
  sty MPTRS + 5

  ; stash the m1 ptr in Y
  ldy MPTRS + 1

  ; walk the second assignment loop
  lda MPTRS + 6
  sta MPTRS + 1
  lda MPTRS + 4
  sta MPTRS + 6
  lda MPTRS + 7
  sta MPTRS + 4
  lda MPTRS + 13
  sta MPTRS + 7
  lda MPTRS + 14
  sta MPTRS + 13
  lda MPTRS + 15
  sta MPTRS + 14
  lda MPTRS + 8
  sta MPTRS + 15

  ; finish the second loop with Y
  sty MPTRS + 8

  rts

; The top 128 bytes of the zero page are reserved for the
; compression function state and message block. The MPTRS
; array is also set up with the message permutation. We don't
; need any other arguments.
round:
  ; Mix the columns.

  ; g(state, 0, 4, 8, 12, m[0], m[1]);
  lda #H0
  sta G_A
  lda #H4
  sta G_B
  lda #H8
  sta G_C
  lda #H12
  sta G_D
  lda MPTRS + 0
  sta G_MX
  lda MPTRS + 1
  sta G_MY
  jsr g

  ; g(state, 1, 5, 9, 13, m[2], m[3]);
  lda #H1
  sta G_A
  lda #H5
  sta G_B
  lda #H9
  sta G_C
  lda #H13
  sta G_D
  lda MPTRS + 2
  sta G_MX
  lda MPTRS + 3
  sta G_MY
  jsr g

  ; g(state, 2, 6, 10, 14, m[4], m[5]);
  lda #H2
  sta G_A
  lda #H6
  sta G_B
  lda #H10
  sta G_C
  lda #H14
  sta G_D
  lda MPTRS + 4
  sta G_MX
  lda MPTRS + 5
  sta G_MY
  jsr g

  ; g(state, 3, 7, 11, 15, m[6], m[7]);
  lda #H3
  sta G_A
  lda #H7
  sta G_B
  lda #H11
  sta G_C
  lda #H15
  sta G_D
  lda MPTRS + 6
  sta G_MX
  lda MPTRS + 7
  sta G_MY
  jsr g

  ; Mix the diagonals.

  ; g(state, 0, 5, 10, 15, m[8], m[9]);
  lda #H0
  sta G_A
  lda #H5
  sta G_B
  lda #H10
  sta G_C
  lda #H15
  sta G_D
  lda MPTRS + 8
  sta G_MX
  lda MPTRS + 9
  sta G_MY
  jsr g

  ; g(state, 1, 6, 11, 12, m[10], m[11]);
  lda #H1
  sta G_A
  lda #H6
  sta G_B
  lda #H11
  sta G_C
  lda #H12
  sta G_D
  lda MPTRS + 10
  sta G_MX
  lda MPTRS + 11
  sta G_MY
  jsr g

  ; g(state, 2, 7, 8, 13, m[12], m[13]);
  lda #H2
  sta G_A
  lda #H7
  sta G_B
  lda #H8
  sta G_C
  lda #H13
  sta G_D
  lda MPTRS + 12
  sta G_MX
  lda MPTRS + 13
  sta G_MY
  jsr g

  ; g(state, 3, 4, 9, 14, m[14], m[15]);
  lda #H3
  sta G_A
  lda #H4
  sta G_B
  lda #H9
  sta G_C
  lda #H14
  sta G_D
  lda MPTRS + 14
  sta G_MX
  lda MPTRS + 15
  sta G_MY
  jsr g

  rts


; $02..08 are 1-byte word pointers: (a, b, c, d, mx, my)
; Note that $00..02 are needed by ror12_u32.
g:
  ; a = a + b + mx
  ldx G_A
  ldy G_B
  jsr add_u32
  ldy G_MX
  jsr add_u32

  ; d = (d ^ a) >>> 16
  ldx G_D
  ldy G_A
  jsr xor_u32
  jsr ror16_u32

  ; c = c + d
  ldx G_C
  ldy G_D
  jsr add_u32

  ; b = (b ^ c) >>> 12
  ldx G_B
  ldy G_C
  jsr xor_u32
  jsr ror12_u32

  ; a = a + b + my
  ldx G_A
  ldy G_B
  jsr add_u32
  ldy G_MY
  jsr add_u32

  ; d = (d ^ a) >>> 8
  ldx G_D
  ldy G_A
  jsr xor_u32
  jsr ror8_u32

  ; c = c + d
  ldx G_C
  ldy G_D
  jsr add_u32

  ; b = (b ^ c) >>> 7
  ldx G_B
  ldy G_C
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

; ---------------------------
; everything below is IO code
; ---------------------------

; IO controller constants
PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003
E  = %10000000
RW = %01000000
RS = %00100000

; preserves A, X, and Y
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

; reads A, preserves X and Y
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

; preserves X and Y
lcd_clear:
  lda #%00000001  ; clear the display
  jsr lcd_instruction
  rts

; preserves X and Y
lcd_line_two:
  lda #%10101000  ; DDRAM address to byte 40
  jsr lcd_instruction
  rts

; reads A, preserves X and Y
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

; str pointer at $00, preserves X
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

; prints *X, preserves X and Y
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

; prints the state bytes H0..=H15
; The display only has 16 chars per row, so we only print the
; first 8 out of 32 bytes here.
print_hash:
  ldx #0
print_hash_loop1:
  lda H0, x
  jsr print_hex_byte
  inx
  cpx #8
  bne print_hash_loop1
  jsr lcd_line_two
  ldx #0
print_hash_loop2:
  lda H2, x
  jsr print_hex_byte
  inx
  cpx #8
  bne print_hash_loop2
  rts

; preserves A, X, and Y
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
