; ======================= Memory Map =======================
; 00..02    scratch space or pointer arguments
; 02..08    G function arguments
; 08..18    permuted pointers into the message block
; 18        chunk counter
; 19        chunk counter for chunks, 0 for parent nodes
; 1a        block length
; 1b        block compressed in current chunk (TODO: unused)
; 1c        bitflags for the next compression
; 1d..1f    input ptr
; 1f..21    hasher input len
; 21..23    chunk input len
; 23..26    pause scratch
; 26        break flag
; 27        remaining block bytes
; 29..2b    print string arg
; 2b..2d    ror12 scratch
; 2d..2f    chunk length
; 2f..80    [free]
; 80..c0    state matrix
; c0..100   message block
; 100..200  [stack page]
; 200...    test vector input pattern
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

INPUT_DONE_STRING: .asciiz "test input done"

; two bytes of scratch space, or sometimes a pointer arg
SCRATCH = $00

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

; chunk state metadata
CHUNK_COUNTER      = $18
CHUNK_COUNTER_OR_0 = $19  ; must be 0 for parent nodes
BLOCK_LENGTH       = $1a
BLOCKS_COMPRESSED  = $1b  ; TODO: unused
DOMAIN_BITFLAGS    = $1c

; inputs for hasher_update
INPUT_PTR = $1d
HASHER_INPUT_LEN = $1f
CHUNK_INPUT_LEN = $21

; 3 bytes of scratch space for the pause function
PAUSE_SCRATCH = $23

; set by BRK/IRQ and cleared by NMI
BREAK_FLAG = $26

REMAINING_BLOCK_BYTES = $27

; two-byte pointer
PRINT_STR_ARG = $29

; two bytes
ROR12_SCRATCH = $2b

; two bytes
CHUNK_LENGTH = $2d

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

TEST_VECTOR_INPUT_START = $200

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

  ; populate the test vector input with 10 KiB of
  ; 0,1,2,...,250,0,1... bytes
  ; Reuse INPUT_PTR for the walking pointer.
  lda #<TEST_VECTOR_INPUT_START
  sta INPUT_PTR
  lda #>TEST_VECTOR_INPUT_START
  sta INPUT_PTR + 1
  ldx #0
populate_input_loop:
  txa
  ldy #0
  sta (INPUT_PTR), y
  ; increment the pointer in INPUT_PTR
  lda INPUT_PTR
  clc
  adc #1
  sta INPUT_PTR
  lda INPUT_PTR + 1
  adc #0  ; add the carry bit
  sta INPUT_PTR + 1
  ; We want to write 40 pages. 42 accounts for the zero page
  ; and the stack page.
  cmp #(40 + 2)
  beq populate_input_done
  inx
  cpx #251
  bne after_reset_paint
  ldx #0
after_reset_paint:
  jmp populate_input_loop
populate_input_done:

  lda #<INPUT_DONE_STRING
  sta PRINT_STR_ARG
  lda #>INPUT_DONE_STRING
  sta PRINT_STR_ARG + 1
  jsr print_str

  ; init the hash state
  jsr hasher_init

  ; set INPUT_PTR to point to TEST_VECTOR_INPUT_START
  lda #<TEST_VECTOR_INPUT_START
  sta INPUT_PTR
  lda #>TEST_VECTOR_INPUT_START
  sta INPUT_PTR + 1

  ; set length 193
  lda #<1023
  sta HASHER_INPUT_LEN
  lda #>1023
  sta HASHER_INPUT_LEN + 1

  ; add some input
  jsr hasher_update

  ; finalize
  jsr hasher_finalize

  ; Did we get it right?!
  jsr print_hash

  ; Now do it again for 1024.
  lda #<TEST_VECTOR_INPUT_START
  sta INPUT_PTR
  lda #>TEST_VECTOR_INPUT_START
  sta INPUT_PTR + 1
  lda #<1024
  sta HASHER_INPUT_LEN
  lda #>1024
  sta HASHER_INPUT_LEN + 1
  jsr hasher_init
  jsr hasher_update
  jsr hasher_finalize
  jsr print_hash

end_loop:
  jmp end_loop

; TODO: currently this only handles one chunk
hasher_init:
  ; pre-initialize the chunk counter to $ff so that it rolls
  ; over to $00 in chunk_state_init_next
  lda #$ff
  sta CHUNK_COUNTER
  jsr chunk_state_init_next
  rts

; reads INPUT_PTR and HASHER_INPUT_LEN
; overwrites CHUNK_INPUT_LEN internally
hasher_update:
  ; The logic here is
  ;
  ;   while input length > 0:
  ;     if the current chunk is full, finalize it and reset it;
  ;     add as much input as possible into the current chunk;

  ; if input is empty, return
  lda HASHER_INPUT_LEN
  ora HASHER_INPUT_LEN + 1
  bne hasher_update_nonempty_input
  ; input is empty
  rts
hasher_update_nonempty_input:

  ; The chunk state is full when CHUNK_LENGTH == $0400 (1024).
  lda CHUNK_LENGTH
  bne hasher_update_chunk_not_full
  lda CHUNK_LENGTH + 1
  cmp #$04
  bne hasher_update_chunk_not_full

  ; If we get here, the chunk state is full. Finalize it, push
  ; the new CV onto the CV stack, and reset the chunk state.
  ; We know there's more input coming, so this is finalization
  ; is non-root.
  lda #0  ; non-root
  jsr chunk_state_finalize
  ; TODO jsr hasher_add_chunk_cv
  jsr chunk_state_init_next

hasher_update_chunk_not_full:
  ; Either we just finalized the last chunk and initialized a
  ; new empty one, or the chunk state was not full. Either
  ; way, add as much input as possible to the current chunk.

  ; initialize CHUNK_INPUT_LEN to $0400 (1024) minus
  ; CHUNK_LENGTH.
  lda #$00
  sec
  sbc CHUNK_LENGTH
  sta CHUNK_INPUT_LEN
  lda #$04
  sbc CHUNK_LENGTH + 1  ; includes the carry/borrow flag
  sta CHUNK_INPUT_LEN + 1

  ; CHUNK_INPUT_LEN now contains the number of bytes needed to
  ; fill the current chunk. However, if HASHER_INPUT_LEN is
  ; less than CHUNK_INPUT_LEN, set CHUNK_INPUT_LEN to
  ; HASHER_INPUT_LEN.
  lda CHUNK_INPUT_LEN
  sec
  sbc HASHER_INPUT_LEN
  lda CHUNK_INPUT_LEN + 1
  sbc HASHER_INPUT_LEN + 1  ; includes the carry/borrow flag
  bcc hasher_update_chunk_input_length_ready
  ; If we get here, then HASHER_INPUT_LEN is <= CHUNK_LENGTH.
  ; Set CHUNK_INPUT_LEN to HASHER_INPUT_LEN.
  lda HASHER_INPUT_LEN
  sta CHUNK_INPUT_LEN
  lda HASHER_INPUT_LEN + 1
  sta CHUNK_INPUT_LEN + 1

hasher_update_chunk_input_length_ready:
  jsr chunk_state_update
  ; chunk_state_update updated INPUT_PTR, HASHER_INPUT_LEN,
  ; and CHUNK_INPUT_LEN (which should now be zero).
  jmp hasher_update  ; back to the top

hasher_finalize:
  ; TODO: roll up subtree CVs
  lda #ROOT
  jsr chunk_state_finalize
  rts

; Chunk state initialization will:
;   - increment the CHUNK_COUNTER
;   - zero the BLOCK_LENGTH
;   - zero the CHUNK_LENGTH
;   - copy the 32 IV bytes to H
;   - initialize DOMAIN_BITFLAGS to CHUNK_START
; The chunk counter is only 8 bits in this implementation,
; because our addressable memory can only fit 32 chunks, but
; in theory we could increase it if we wanted to hash some
; larger input in pieces. For the first chunk, we'll
; initialize the chunk counter to $ff before calling this, and
; it'll roll over.
chunk_state_init_next:
  inc CHUNK_COUNTER
  lda #0
  sta BLOCK_LENGTH
  sta CHUNK_LENGTH
  sta CHUNK_LENGTH + 1

  ; set H to IV
  ldx #0
new_chunk_state_iv_bytes_loop:
  lda IV0_BYTES, x
  sta H0, x
  inx
  cpx #32
  bne new_chunk_state_iv_bytes_loop

  ; initialize DOMAIN_BITFLAGS
  lda #CHUNK_START
  sta DOMAIN_BITFLAGS

  rts

; Reads INPUT_PTR and CHUNK_INPUT_LEN.
; Modifies both of those and also HASHER_INPUT_LEN.
chunk_state_update:
  ; The logic here is
  ;
  ;   while input length > 0:
  ;     if the block is full, compress it and clear it;
  ;     copy as much input as possible into the block buffer;

  ; if input is empty, return
  lda CHUNK_INPUT_LEN
  ora CHUNK_INPUT_LEN + 1
  bne chunk_state_update_nonempty_input
  ; input is empty
  rts
chunk_state_update_nonempty_input:

  ; if the block buffer isn't full, go to copy
  lda BLOCK_LENGTH
  cmp #64
  bne chunk_state_update_copy

  ; the block buffer is full, compress it
  lda CHUNK_COUNTER
  sta CHUNK_COUNTER_OR_0
  jsr compress

  ; update CHUNK_LENGTH, and clear BLOCK_LENGTH and
  ; DOMAIN_BITFLAGS (to remove CHUNK_START)
  lda CHUNK_LENGTH
  clc
  adc #64
  sta CHUNK_LENGTH
  lda CHUNK_LENGTH + 1
  adc #0  ; add the carry bit
  sta CHUNK_LENGTH + 1
  lda #0
  sta BLOCK_LENGTH
  sta DOMAIN_BITFLAGS

chunk_state_update_copy:
  ; we need to copy the minimum of (64-BLOCK_LENGTH), which is
  ; 8-bits, and CHUNK_INPUT_LEN, which is 16-bits.

  ; The remaining buffer space is 64 - BLOCK_LENGTH. Write
  ; this value to REMAINING_BLOCK_BYTES.
  lda #64
  sec
  sbc BLOCK_LENGTH
  sta REMAINING_BLOCK_BYTES

  ; If the high byte of CHUNK_INPUT_LEN is non-zero, keep the
  ; remaining buffer space in REMAINING_BLOCK_BYTES and jump
  ; to the loop start.
  lda CHUNK_INPUT_LEN + 1
  bne chunk_state_update_copy_loop_start

  ; Do the same if the remaining buffer space is less than the
  ; low byte of CHUNK_INPUT_LEN.
  ; NOTE: A big mistake I made here was trying to use BMI to
  ; branch if the result of the subtraction is negative. That
  ; appears to work until CHUNK_INPUT_LEN is large enough that
  ; the result wraps back around to <=127. (So in this case it
  ; goes wrong here when CHUNK_INPUT_LEN is 193.) BCC is the
  ; reliable way to implement a less-than check.
  lda REMAINING_BLOCK_BYTES
  cmp CHUNK_INPUT_LEN
  bcc chunk_state_update_copy_loop_start

  ; Otherwise, this is the "input is shorter than remaining
  ; buffer space" case. Overwrite REMAINING_BLOCK_BYTES with
  ; the low byte of CHUNK_INPUT_LEN (the high byte must be
  ; zero here).
  lda CHUNK_INPUT_LEN
  sta REMAINING_BLOCK_BYTES

  ; REMAINING_BLOCK_BYTES holds the number of bytes to copy
  ; from INPUT_PTR. Use Y to do indirect indexing through
  ; INPUT_PTR (X does not support this), and use X as a cursor
  ; in COMPRESS_MSG.
chunk_state_update_copy_loop_start:
  ldy #0
  ldx BLOCK_LENGTH
chunk_state_update_copy_loop_continue:
  cpy REMAINING_BLOCK_BYTES
  beq chunk_state_update_copy_loop_end
  lda (INPUT_PTR), y
  sta COMPRESS_MSG, x
  inx
  iny
  jmp chunk_state_update_copy_loop_continue
chunk_state_update_copy_loop_end:

  ; X contains the new BLOCK_LENGTH. Store it.
  stx BLOCK_LENGTH

  ; REMAINING_BLOCK_BYTES contains the number of bytes just
  ; copied. Add REMAINING_BLOCK_BYTES to INPUT_PTR and
  ; subtract it from both CHUNK_INPUT_LEN and
  ; HASHER_INPUT_LEN.
  lda INPUT_PTR
  clc
  adc REMAINING_BLOCK_BYTES
  sta INPUT_PTR
  lda INPUT_PTR + 1
  adc #0  ; adds the carry bit
  sta INPUT_PTR + 1

  lda CHUNK_INPUT_LEN
  sec
  sbc REMAINING_BLOCK_BYTES
  sta CHUNK_INPUT_LEN
  lda CHUNK_INPUT_LEN + 1
  sbc #0  ; subtracts the carry/borrow bit
  sta CHUNK_INPUT_LEN + 1

  lda HASHER_INPUT_LEN
  sec
  sbc REMAINING_BLOCK_BYTES
  sta HASHER_INPUT_LEN
  lda HASHER_INPUT_LEN + 1
  sbc #0  ; subtracts the carry/borrow bit
  sta HASHER_INPUT_LEN + 1

  ; go back to the top
  jmp chunk_state_update

chunk_state_update_end:
  rts

; The caller must first set the A register to either 0
; (non-root) or ROOT. This function writes zero padding to the
; block buffer, sets the CHUNK_END flag, and calls compress.
chunk_state_finalize:
  ; The caller has set the A register to either 0 or ROOT.
  ; DOMAIN_BITFLAGS contains either 0 or CHUNK_START.
  ; Bitwise-or both of those values together, bitwise-or the
  ; CHUNK_END flag in addition, and then write the reuslt to
  ; DOMAIN_BITFLAGS.
  ora DOMAIN_BITFLAGS
  ora #CHUNK_END
  sta DOMAIN_BITFLAGS

  ; write zero padding to the block buffer
  ldx BLOCK_LENGTH
chunk_state_finalize_padding_loop:
  cpx #64
  beq chunk_state_finalize_padding_loop_end
  lda #0
  sta COMPRESS_MSG, x
  inx
  jmp chunk_state_finalize_padding_loop
chunk_state_finalize_padding_loop_end:

  ; Compress the final block.
  lda CHUNK_COUNTER
  sta CHUNK_COUNTER_OR_0
  jsr compress

  rts

; Compression will first:
;   - initialize the MPTRS array
;   - set the IV constants in the third row of the state
;   - set T0, T1, B, and D in the fourth row of the state
; The caller needs to arrange:
;   - the 32-byte CV in H0..=H7
;   - the 64-byte block buffer at COMPRESS_MSG, including its
;     zero padding
;   - the value of BLOCK_LENGTH
;   - the value of CHUNK_COUNTER_OR_0
;   - the bitflags in DOMAIN_BITFLAGS
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

  ; set T0, T1, B, and D
  lda #0
  sta COMPRESS_T0 + 1
  sta COMPRESS_T0 + 2
  sta COMPRESS_T0 + 3
  sta COMPRESS_T1 + 0
  sta COMPRESS_T1 + 1
  sta COMPRESS_T1 + 2
  sta COMPRESS_T1 + 3
  sta COMPRESS_B + 1
  sta COMPRESS_B + 2
  sta COMPRESS_B + 3
  sta COMPRESS_D + 1
  sta COMPRESS_D + 2
  sta COMPRESS_D + 3
  lda CHUNK_COUNTER_OR_0
  sta COMPRESS_T0
  lda BLOCK_LENGTH
  sta COMPRESS_B
  lda DOMAIN_BITFLAGS
  sta COMPRESS_D

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

; *X >>>= 12, preserves X and Y
ror12_u32:
  ; copy byte0 and byte1
  lda $00, x
  sta ROR12_SCRATCH
  lda $01, x
  sta ROR12_SCRATCH + 1

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
  ; write byte2 upper nibble, using ROR12_SCRATCH
  lda ROR12_SCRATCH
  asl
  asl
  asl
  asl
  ora $02, x
  sta $02, x

  ; write byte2 lower nibble, using ROR12_SCRATCH
  lda ROR12_SCRATCH
  lsr
  lsr
  lsr
  lsr
  sta $03, x
  ; write byte2 upper nibble, using ROR12_SCRATCH
  lda ROR12_SCRATCH + 1
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

; str pointer in PRINT_STR_ARG, preserves X
print_str:
  ldy #0
print_str_loop:
  lda (PRINT_STR_ARG), y
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
  lda $00, x
  jsr print_hex_byte
  lda $01, x
  jsr print_hex_byte
  lda $02, x
  jsr print_hex_byte
  lda $03, x
  jsr print_hex_byte
  rts

; prints the state bytes H0..=H3
print_hash:
  ldx #H0
  jsr print_state_row
  rts

; X contains the starting address of the row (H0, H4, H8, H12)
print_state_row:
  jsr lcd_clear
  jsr print_hex_u32
  inx
  inx
  inx
  inx
  jsr print_hex_u32
  inx
  inx
  inx
  inx
  jsr lcd_line_two
  jsr print_hex_u32
  inx
  inx
  inx
  inx
  jsr print_hex_u32
  inx
  inx
  inx
  inx
  rts

print_full_state:
  ldx #H0
print_full_state_loop:
  jsr print_state_row
  lda #4
  jsr pause
  jsr lcd_clear
  lda #1
  jsr pause
  cpx #(H0 + 64)
  bne print_full_state_loop

  rts

print_debug_info:
  pha
  txa
  pha
  tya
  pha

  jsr lcd_clear

  lda #"d"
  jsr print_char
  lda #" "
  jsr print_char

  lda INPUT_PTR+1
  jsr print_hex_byte
  lda INPUT_PTR
  jsr print_hex_byte
  lda #" "
  jsr print_char

  lda HASHER_INPUT_LEN+1
  jsr print_hex_byte
  lda HASHER_INPUT_LEN
  jsr print_hex_byte
  lda #" "
  jsr print_char

  lda BLOCK_LENGTH
  jsr print_hex_byte
  lda #" "
  jsr print_char

  lda #4
  jsr pause

  pla
  tay
  pla
  tax
  pla
  rts

; A controls the pause duration, preserves A, X, and Y
pause:
  ; write A, X, and Y to PAUSE_SCRATCH
  sta PAUSE_SCRATCH
  stx PAUSE_SCRATCH + 1
  sty PAUSE_SCRATCH + 2

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

  ; restore X and Y
  lda PAUSE_SCRATCH
  ldx PAUSE_SCRATCH + 1
  ldy PAUSE_SCRATCH + 2
  rts

break:
  pha
  lda #1
  sta BREAK_FLAG
break_loop:
  lda BREAK_FLAG
  bne break_loop
  ; we get here when NMI clears the BREAK_FLAG
  pla
  rts

irq:
  rti

nmi:
  pha
  lda #0
  sta BREAK_FLAG
  pla
  rti

  .org $fffa
  .word nmi
  .word main
  .word irq
