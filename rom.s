; ======================= Memory Map =======================
; 00..02      scratch space or pointer arguments
; 02..08      G function arguments
; 08..18      permuted pointers into the message block
; 18          chunk counter
; 19          chunk counter for chunks, 0 for parent nodes
; 1a          block length
; 1b          block compressed in current chunk (TODO: unused)
; 1c          bitflags for the next compression
; 1d..1f      input ptr
; 1f..21      hasher input len
; 21..23      chunk input len
; 23..26      pause scratch
; 26          break flag
; 27          block bytes to copy
; 29..2b      print string arg
; 2b..2d      ror12 scratch
; 2d..2f      chunk length
; 2f..31      cv stack pointer
; 31..33      test input lengths pointer
; 33..80      [free]
; 78..80      chunk counter (64 bits)
; 80..c0      state matrix
; c0..100     message block
; 100..200    [stack page]
; 200..8c0    cv stack (32 * 54 = 1728 bytes)
; 8c0...30c0  test vector input with repeating pattern (10 KiB)
; 30c0..4000  [free]
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

INPUT_LENGTH_STRING: .asciiz "input len: "
INPUT_251_STRING: .asciiz "251*251 = 63001:"
INPUT_1MB_STRING: .asciiz "1 MB all zeros: "

TEST_INPUT_LENGTHS_START:
  .word 0
  .word 1
  .word 2
  .word 3
  .word 4
  .word 5
  .word 6
  .word 7
  .word 8
  .word 63
  .word 64
  .word 65
  .word 127
  .word 128
  .word 129
  .word 1023
  .word 1024
  .word 1025
  .word 2048
  .word 2049
  .word 3072
  .word 3073
  .word 4096
  .word 4097
  .word 5120
  .word 5121
  .word 6144
  .word 6145
  .word 7168
  .word 7169
  .word 8192
  .word 8193
TEST_INPUT_LENGTHS_END:
  .word 0

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

; CHUNK_COUNTER      = $18  ; TODO: unused
; CHUNK_COUNTER_OR_0 = $19  ; TODO: unused
BLOCK_LENGTH       = $1a
; BLOCKS_COMPRESSED  = $1b  ; TODO: unused
; DOMAIN_BITFLAGS    = $1c  ; TODO: unused

; inputs for hasher_update
INPUT_PTR = $1d
HASHER_INPUT_LEN = $1f
CHUNK_INPUT_LEN = $21

; 3 bytes of scratch space for the pause function
PAUSE_SCRATCH = $23

; set by BRK/IRQ and cleared by NMI
BREAK_FLAG = $26

BLOCK_BYTES_TO_COPY = $27

; two-byte pointer
PRINT_STR_ARG = $29

; two bytes
ROR12_SCRATCH = $2b

; two bytes
CHUNK_LENGTH = $2d

; two bytes, pointing to just after the last CV in the stack
CV_STACK_PTR = $2f

; two bytes, pointing to just after the last CV in the stack
TEST_INPUT_LEN_PTR = $31

; 8 bytes
CHUNK_COUNTER = $78

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

; The chaining value stack ("CV stack") has space for 54 CVs, 32 bytes
; each: 54 * 32 = 1728 = $06c0
; $0200 + $06c0 = $08c0
;
; Allocating a full CV stack here is excessive, since we only have 64
; KiB of addressable memory, and the tree depth of a 64 KiB input is six
; (much less than 54). However, we do have a test case that hashes 1 MB
; of zeros, much larger than addressable memory. Also this demonstrates
; that BLAKE3's space overhead isn't too bad, even for the 6502.
CV_STACK_START = $200
CV_STACK_CAPACITY_END = CV_STACK_START + (54 * 32)

TEST_INPUT_START = CV_STACK_CAPACITY_END
TEST_INPUT_END   = TEST_INPUT_START + (10 * 1024)

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

  ; Populate the test input with 10 KiB of a repeating 251-byte cycle:
  ; 0, 1, 2, ..., 250, 0, 1, and so on. This is the input pattern used
  ; by the BLAKE3 test vectors. Reuse INPUT_PTR for writing here. X will
  ; track the 251-byte cycle, and the Y will track page offsets.
  lda #<TEST_INPUT_START
  sta INPUT_PTR
  lda #>TEST_INPUT_START
  sta INPUT_PTR + 1
  ldx #0
populate_test_input_loop:
  ldy #0
populate_test_input_page_loop:
  ; Write X to the Yth byte of the current page.
  txa
  sta (INPUT_PTR), y
  ; X = (X + 1) % 251
  inx
  cpx #251
  bne populate_test_input_x_lt_251
  ldx #0
populate_test_input_x_lt_251:
  ; Y += 1
  iny
  ; If Y hasn't wrapped to 0, continue writing this page.
  bne populate_test_input_page_loop
  ; Y has wrapped to 0. Bump INPUT_PTR by one page.
  inc INPUT_PTR + 1
  ; If INPUT_PTR - TEST_INPUT_START is 40 * 256 = 10240 = $2800, this
  ; loop is done. We only increment the high byte, which is still in A,
  ; so that's all we need to check.
  lda INPUT_PTR + 1
  sec
  sbc #>TEST_INPUT_START
  cmp #$28
  bne populate_test_input_loop
  ; Populate test input loop done.

  ; Hash each of the test inputs. These are the official BLAKE3 test
  ; vectors (leaving out the ones that don't fit in RAM here).
  lda #<TEST_INPUT_LENGTHS_START
  sta TEST_INPUT_LEN_PTR
  lda #>TEST_INPUT_LENGTHS_START
  sta TEST_INPUT_LEN_PTR + 1
hash_test_inputs_loop:
  ; Check whether we've reached TEST_INPUT_LENGTHS_END.
  lda TEST_INPUT_LEN_PTR
  cmp #<TEST_INPUT_LENGTHS_END
  bne hash_test_inputs_loop_keep_going
  lda TEST_INPUT_LEN_PTR + 1
  cmp #>TEST_INPUT_LENGTHS_END
  bne hash_test_inputs_loop_keep_going
  ; If we get here, we're done with this loop.
  jmp hash_test_inputs_end
hash_test_inputs_loop_keep_going:
  ; Set up the input pointer and length.
  lda #<TEST_INPUT_START
  sta INPUT_PTR
  lda #>TEST_INPUT_START
  sta INPUT_PTR + 1
  ldy #0
  lda (TEST_INPUT_LEN_PTR), y
  sta HASHER_INPUT_LEN
  iny
  lda (TEST_INPUT_LEN_PTR), y
  sta HASHER_INPUT_LEN + 1

  ; Print the input length to the first row of the LCD.
  jsr lcd_clear
  lda #<INPUT_LENGTH_STRING
  sta PRINT_STR_ARG
  lda #>INPUT_LENGTH_STRING
  sta PRINT_STR_ARG + 1
  jsr print_str
  lda HASHER_INPUT_LEN + 1
  jsr print_hex_byte
  lda HASHER_INPUT_LEN
  jsr print_hex_byte
  jsr lcd_line_two

  ; Hash this input.
  jsr hasher_init
  jsr hasher_update
  jsr hasher_finalize

  ; Print the first 16 hex chars of the hash to the second row.
  jsr print_hash
  lda #4
  jsr pause

  ; Increment TEST_INPUT_LEN_PTR.
  lda TEST_INPUT_LEN_PTR
  clc
  adc #2
  sta TEST_INPUT_LEN_PTR
  lda TEST_INPUT_LEN_PTR + 1
  adc #0  ; includes the carry bit
  sta TEST_INPUT_LEN_PTR + 1
  ; Continue the test inputs loop.
  jmp hash_test_inputs_loop
hash_test_inputs_end:

  ; We've finished the main test vectors loop. Now there are two more
  ; custom inputs to hash. First, hash the 251-byte test cycle 251
  ; times. These bytes are already populated.
  jsr lcd_clear
  lda #<INPUT_251_STRING
  sta PRINT_STR_ARG
  lda #>INPUT_251_STRING
  sta PRINT_STR_ARG + 1
  jsr print_str
  jsr lcd_line_two
  ; Initialize the hasher and use X as the loop variable.
  jsr hasher_init
  ldx #251
loop_251:
  lda #<TEST_INPUT_START
  sta INPUT_PTR
  lda #>TEST_INPUT_START
  sta INPUT_PTR + 1
  lda #251
  sta HASHER_INPUT_LEN
  lda #0
  sta HASHER_INPUT_LEN + 1
  ; Save X.
  txa
  pha
  ; Add input.
  jsr hasher_update
  ; Restore X.
  pla
  tax
  ; Decrement X and continue the loop if we're not done.
  dex
  bne loop_251
  jsr hasher_finalize
  jsr print_hash
  lda #8
  jsr pause


  ; The second custom input is a megabyte of all zeros. Overwrite the
  ; test input with all zeros, then hash a 10 KB (not KiB) chunk of it
  ; 100 times.
  lda #<TEST_INPUT_START
  sta INPUT_PTR
  lda #>TEST_INPUT_START
  sta INPUT_PTR + 1
zero_test_input_loop:
  ldy #0
  lda #0
zero_test_input_page_loop:
  ; Write 0 to the Yth byte of the current page.
  sta (INPUT_PTR), y
  ; Y += 1
  iny
  ; If Y hasn't wrapped to 0, continue writing this page.
  bne zero_test_input_page_loop
  ; Y has wrapped to 0. Bump INPUT_PTR by one page.
  inc INPUT_PTR + 1
  ; If INPUT_PTR - TEST_INPUT_START is 40 * 256 = 10240 = $2800, this
  ; loop is done. We only increment the high byte, which is still in A,
  ; so that's all we need to check.
  lda INPUT_PTR + 1
  sec
  sbc #>TEST_INPUT_START
  cmp #$28
  bne zero_test_input_loop
  ; Zero test input loop done.

  ; Now hash 10 KB one hundred times.
  jsr lcd_clear
  lda #<INPUT_1MB_STRING
  sta PRINT_STR_ARG
  lda #>INPUT_1MB_STRING
  sta PRINT_STR_ARG + 1
  jsr print_str
  jsr lcd_line_two
  jsr hasher_init
  ldx #100
hash_megabyte_loop:
  lda #<TEST_INPUT_START
  sta INPUT_PTR
  lda #>TEST_INPUT_START
  sta INPUT_PTR + 1
  lda #<10000
  sta HASHER_INPUT_LEN
  lda #>10000
  sta HASHER_INPUT_LEN + 1
  txa
  pha
  jsr hasher_update
  pla
  tax
  dex
  bne hash_megabyte_loop
  jsr hasher_finalize
  jsr print_hash

end_loop:
  jmp end_loop


hasher_init:
  ; Initialize the 8-byte chunk counter to zero.
  lda #0
  sta CHUNK_COUNTER + 0
  sta CHUNK_COUNTER + 1
  sta CHUNK_COUNTER + 2
  sta CHUNK_COUNTER + 3
  sta CHUNK_COUNTER + 4
  sta CHUNK_COUNTER + 5
  sta CHUNK_COUNTER + 6
  sta CHUNK_COUNTER + 7
  ; Initialize the CV_STACK_PTR to CV_STACK_START.
  lda #<CV_STACK_START
  sta CV_STACK_PTR
  lda #>CV_STACK_START
  sta CV_STACK_PTR + 1
  ; Initialize the chunk state.
  jsr chunk_state_init
  rts


increment_chunk_counter:
  inc CHUNK_COUNTER + 0
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 1
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 2
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 3
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 4
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 5
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 6
  bne increment_chunk_counter_end
  inc CHUNK_COUNTER + 7
increment_chunk_counter_end:
  rts


; pushes the CV stored at H0..=H7 and moves up CV_STACK_PTR
hasher_push_stack:
  ; Copy the new CV.
  ldy #31
hasher_push_stack_loop:
  lda H0, y
  sta (CV_STACK_PTR), y
  dey
  ; Continue until Y is negative.
  bpl hasher_push_stack_loop

  ; The copy is done. Increment CV_STACK_PTR and return.
  lda CV_STACK_PTR
  clc
  adc #32
  sta CV_STACK_PTR
  lda CV_STACK_PTR + 1
  adc #0  ; includes the carry bit
  sta CV_STACK_PTR + 1
  rts


; decrements CV_STACK_PTR and copies the popped CV into the left half of
; the message block
hasher_pop_stack:
  ; Decrement CV_STACK_PTR.
  lda CV_STACK_PTR
  sec
  sbc #32
  sta CV_STACK_PTR
  lda CV_STACK_PTR + 1
  sbc #0  ; includes the carry/borrow bit
  sta CV_STACK_PTR + 1

  ; Copy the popped CV into the left half of the message block.
  ldy #31
hasher_pop_stack_loop:
  lda (CV_STACK_PTR), y
  sta COMPRESS_MSG, y
  dey
  ; Continue until Y is negative.
  bpl hasher_pop_stack_loop
  rts


; Copy the current CV stored at H0..=H7 into the right half of the
; message block. This is used in both hasher_add_chunk_cv and
; hasher_finalize.
copy_current_cv_to_message_block:
  ldy #31
copy_current_cv_to_message_block_loop:
  lda H0, y
  sta COMPRESS_MSG + 32, y
  dey
  ; Continue until Y is negative.
  bpl copy_current_cv_to_message_block_loop
  rts


hasher_merge_parent_node:
  ; First, copy the current CV into the right half of the message
  ; buffer.
  jsr copy_current_cv_to_message_block
  ; Next, pop the top CV off the stack. It gets copied into the left
  ; half of the message buffer.
  jsr hasher_pop_stack
  ; Do a parent block compression to merge them. A=0 here indicates this
  ; is a non-root compression.
  lda #0  ; non-root
  jsr compress_parent_block
  rts


; adds the chunk CV stored at H0..=H7 into the hash tree
;
; This chunk might complete some subtrees. For each completed subtree,
; its left child will be the current top CV in the CV stack, and its
; right child will be the CV currently in H0..=H7. Thus for each
; completed subtree, pop its left child off the stack and into the
; message buffer, copy the current CV into the other half of the message
; buffer, and compress to merge them, overwriting the current CV in the
; process. After all these merges, push the final value of H0..=H7 onto
; the stack. The number of completed subtrees is given by the number of
; trailing 0-bits in the new total number of chunks.
;
; Section 5.1.2 of the BLAKE3 spec explains this algorithm in more
; detail.
hasher_add_chunk_cv:
  ; In pseudocode this is:
  ;
  ;   for each trailing 0 bit in CHUNK_COUNTER:
  ;     pop a CV to the left half of message block
  ;     copy H0..=H7 to the right half message block
  ;     compress a parent node, leaving its CV in H0..=H7
  ;   push the CV in H0..=H7

  ; X is an index into the bytes of CHUNK_COUNTER.
  ldx #0
hasher_add_chunk_cv_loop_over_chunk_counter_bytes:
  ; Load the current counter byte into A.
  lda CHUNK_COUNTER, x
  ; Y counts the bits shifted in each byte. For each zero bit, we'll
  ; merge a parent. If all 8 bits are zero, we'll continue the outer
  ; loop to the next byte. If we encounter a one-bit, return.
  ldy #8
hasher_add_chunk_cv_loop_within_chunk_counter_byte:
  ; Shift A one bit to the right, so that the lowest bit goes to the
  ; carry flag.
  lsr
  ; If the carry bit is clear, keep going. Otherwise push the current CV
  ; and return.
  bcc hasher_add_chunk_cv_keep_going
  jsr hasher_push_stack
  rts
hasher_add_chunk_cv_keep_going:
  ; Save A, X, and Y to the stack and do one parent node merge.
  pha
  txa
  pha
  tya
  pha
  jsr hasher_merge_parent_node
  pla
  tay
  pla
  tax
  pla
  ; Decrement Y. If it has not reached zero, continue the inner loop.
  dey
  bne hasher_add_chunk_cv_loop_within_chunk_counter_byte
  ; Otherwise increment X and continue the outer loop.
  inx
  jmp hasher_add_chunk_cv_loop_over_chunk_counter_bytes


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
  jsr increment_chunk_counter
  jsr hasher_add_chunk_cv
  jsr chunk_state_init

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


; If the current chunk is the only chunk (CHUNK_COUNTER == 0), then
; root-finalize it and return. Otherwise, non-root-finalize it, and then
; merge each of the CVs in the CV stack. The final merge is the root.
; This ends up computing all the parent nodes on the right edge of the
; hash tree.
hasher_finalize:
  ; If this is the only chunk, root-finalize it and return. We need to
  ; check all the bytes of CHUNK_COUNTER.
  lda CHUNK_COUNTER + 0
  ora CHUNK_COUNTER + 1
  ora CHUNK_COUNTER + 2
  ora CHUNK_COUNTER + 3
  ora CHUNK_COUNTER + 4
  ora CHUNK_COUNTER + 5
  ora CHUNK_COUNTER + 6
  ora CHUNK_COUNTER + 7
  bne hasher_finalize_stack_nonempty
  ; CHUNK_COUNTER is zero and this is the only chunk. Root finalize it
  ; and return.
  lda #ROOT
  jsr chunk_state_finalize
  rts
hasher_finalize_stack_nonempty:
  ; We need to do merges along the right edge of the tree. First
  ; finalize the current chunk as non-root.
  lda #0  ; non-root
  jsr chunk_state_finalize
  ; Pop and merge all the entries in the CV stack. The final one is the
  ; root.
hasher_finalize_merge_loop:
  jsr copy_current_cv_to_message_block
  jsr hasher_pop_stack
  ; We've just popped a CV from the stack into the message buffer. If
  ; the stack is now empty (CV_STACK_PTR == CV_STACK_START), break out
  ; of this loop to merge as root. Otherwise merge as non-root and
  ; continue the loop.
  lda CV_STACK_PTR
  cmp #<CV_STACK_START
  bne hasher_finalize_merge_loop_nonroot
  lda CV_STACK_PTR + 1
  cmp #>CV_STACK_START
  bne hasher_finalize_merge_loop_nonroot
  ; Finalize as root and return.
  lda #ROOT
  jsr compress_parent_block
  rts
hasher_finalize_merge_loop_nonroot:
  ; Finalize as non-root and continue the loop.
  lda #0  ; non-root
  jsr compress_parent_block
  jmp hasher_finalize_merge_loop


; Chunk state initialization will:
;   - zero the BLOCK_LENGTH
;   - zero the CHUNK_LENGTH
;   - copy the 32 IV bytes to H
; Note that zeroing and incrementing the CHUNK_COUNTER is done in
; hasher_init and hasher_update, not here.
chunk_state_init:
  lda #0
  sta BLOCK_LENGTH
  sta CHUNK_LENGTH
  sta CHUNK_LENGTH + 1

  ; set H to IV
  jsr set_h_to_iv

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

  ; The block buffer is full, so compress it. This is not CHUNK_END or
  ; ROOT, so set A to 0. Note that compress_chunk_block will take care
  ; of CHUNK_START.
  lda #0  ; non-root, non-end
  jsr compress_chunk_block

  ; reset BLOCK_LENGTH to 0
  lda #0
  sta BLOCK_LENGTH

chunk_state_update_copy:
  ; we need to copy the minimum of (64-BLOCK_LENGTH), which is
  ; 8-bits, and CHUNK_INPUT_LEN, which is 16-bits.

  ; The remaining buffer space is 64 - BLOCK_LENGTH. Write
  ; this value to BLOCK_BYTES_TO_COPY.
  lda #64
  sec
  sbc BLOCK_LENGTH
  sta BLOCK_BYTES_TO_COPY

  ; If the high byte of CHUNK_INPUT_LEN is non-zero, keep the
  ; remaining buffer space in BLOCK_BYTES_TO_COPY and jump
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
  lda BLOCK_BYTES_TO_COPY
  cmp CHUNK_INPUT_LEN
  bcc chunk_state_update_copy_loop_start

  ; Otherwise, this is the "input is shorter than remaining
  ; buffer space" case. Overwrite BLOCK_BYTES_TO_COPY with
  ; the low byte of CHUNK_INPUT_LEN (the high byte must be
  ; zero here).
  lda CHUNK_INPUT_LEN
  sta BLOCK_BYTES_TO_COPY

  ; BLOCK_BYTES_TO_COPY holds the number of bytes to copy
  ; from INPUT_PTR. Use Y to do indirect indexing through
  ; INPUT_PTR (X does not support this), and use X as a cursor
  ; in COMPRESS_MSG.
chunk_state_update_copy_loop_start:
  ldy #0
  ldx BLOCK_LENGTH
chunk_state_update_copy_loop_continue:
  cpy BLOCK_BYTES_TO_COPY
  beq chunk_state_update_copy_loop_end
  lda (INPUT_PTR), y
  sta COMPRESS_MSG, x
  inx
  iny
  jmp chunk_state_update_copy_loop_continue
chunk_state_update_copy_loop_end:

  ; X contains the new BLOCK_LENGTH. Store it.
  stx BLOCK_LENGTH

  ; BLOCK_BYTES_TO_COPY contains the number of bytes just copied. Add
  ; BLOCK_BYTES_TO_COPY to both INPUT_PTR and CHUNK_LENGTH and subtract
  ; it from both CHUNK_INPUT_LEN and HASHER_INPUT_LEN.
  lda INPUT_PTR
  clc
  adc BLOCK_BYTES_TO_COPY
  sta INPUT_PTR
  lda INPUT_PTR + 1
  adc #0  ; adds the carry bit
  sta INPUT_PTR + 1

  lda CHUNK_LENGTH
  clc
  adc BLOCK_BYTES_TO_COPY
  sta CHUNK_LENGTH
  lda CHUNK_LENGTH + 1
  adc #0  ; adds the carry bit
  sta CHUNK_LENGTH + 1

  lda CHUNK_INPUT_LEN
  sec
  sbc BLOCK_BYTES_TO_COPY
  sta CHUNK_INPUT_LEN
  lda CHUNK_INPUT_LEN + 1
  sbc #0  ; subtracts the carry/borrow bit
  sta CHUNK_INPUT_LEN + 1

  lda HASHER_INPUT_LEN
  sec
  sbc BLOCK_BYTES_TO_COPY
  sta HASHER_INPUT_LEN
  lda HASHER_INPUT_LEN + 1
  sbc #0  ; subtracts the carry/borrow bit
  sta HASHER_INPUT_LEN + 1

  ; go back to the top
  jmp chunk_state_update


; The caller must first set the A register to either 0
; (non-root) or ROOT. This function writes zero padding to the
; block buffer, sets the CHUNK_END flag, and calls compress.
chunk_state_finalize:
  ; The caller has set A to either 0 or ROOT. Save it to Y.
  tay

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

  ; Retrieve 0 or ROOT from Y, set CHUNK_END in addition, and compress.
  ; Note that compress_chunk_block will take care of CHUNK_START.
  tya
  ora #CHUNK_END
  jsr compress_chunk_block

  rts


; The caller should set A to 0, CHUNK_END, or CHUNK_END|ROOT. This
; function:
;   - adds the CHUNK_START flag to A if CHUNK_LENGTH <= 64
;   - copies CHUNK_COUNTER into T0 and T1
;   - calls compress
compress_chunk_block:
  ; Copy the domain flags from A to X.
  tax
  ; Subtract 64 - CHUNK_LENGTH. If 64 < CHUNK_LENGTH, this will clear
  ; the carry flag. Thus we should set CHUNK_START if the carry flag
  ; remains set.
  lda #64
  sec
  sbc CHUNK_LENGTH
  lda #0
  sbc CHUNK_LENGTH + 1  ; includes the carry/borrow flag
  ; Now the carry flag is set if we should set CHUNK_START. The
  ; CHUNK_START flag happens to have the value 1, so just add the carry
  ; bit to the flags previously stashed in X.
  txa
  adc #0

  ; Copy CHUNK_COUNTER into T0 and T1. Preserve A while we do this.
  ldx CHUNK_COUNTER + 0
  stx COMPRESS_T0 + 0
  ldx CHUNK_COUNTER + 1
  stx COMPRESS_T0 + 1
  ldx CHUNK_COUNTER + 2
  stx COMPRESS_T0 + 2
  ldx CHUNK_COUNTER + 3
  stx COMPRESS_T0 + 3
  ldx CHUNK_COUNTER + 4
  stx COMPRESS_T1 + 0
  ldx CHUNK_COUNTER + 5
  stx COMPRESS_T1 + 1
  ldx CHUNK_COUNTER + 6
  stx COMPRESS_T1 + 2
  ldx CHUNK_COUNTER + 7
  stx COMPRESS_T1 + 3

  ; Note that we don't mess with H or the IV bytes here. H is either set
  ; up in chunk_state_init, or left over from the last compression.
  ; Similarly, BLOCK_LENGTH is set in chunk_state_update.

  ; A still contains the full set of flags.
  jsr compress
  rts


; In addition to putting child CVs in the message block, the
; caller must first set the A register to either 0 (non-root)
; or ROOT. This function:
;   - adds the PARENT flag to A
;   - writes zero bytes to T0 and T1
;   - sets BLOCK_LENGTH to 64
;   - sets H to IV
;   - calls compress
compress_parent_block:
  ; The caller has set A to either 0 (non-root) or ROOT. Set the PARENT
  ; flag also and then push A.
  ora #PARENT;
  pha

  ; Set BLOCK_LENGTH to 64.
  lda #64
  sta BLOCK_LENGTH

  ; Load IV bytes into H.
  jsr set_h_to_iv

  ; Load zeros into T0 and T1.
  lda #0
  sta COMPRESS_T0 + 0
  sta COMPRESS_T0 + 1
  sta COMPRESS_T0 + 2
  sta COMPRESS_T0 + 3
  sta COMPRESS_T1 + 0
  sta COMPRESS_T1 + 1
  sta COMPRESS_T1 + 2
  sta COMPRESS_T1 + 3

  ; Pull the domain flags back into A and compress.
  pla
  jsr compress
  rts


set_h_to_iv:
  ldx #31
set_h_to_iv_loop:
  lda IV0_BYTES, x
  sta H0, x
  dex
  ; continue until x is negative
  bpl set_h_to_iv_loop
  rts


; Compression will first:
;   - set D based on domain bitflags in A
;   - load B from BLOCK_LENGTH
;   - initialize the MPTRS array
;   - set the IV constants in the third row of the state
; The caller needs to set or keep track of:
;   - domain bitflags in the A register
;   - the value of BLOCK_LENGTH
;   - the 8 bytes in T0 and T1
;   - the 32-byte CV in H0..=H7
;   - the 64-byte block buffer at COMPRESS_MSG, including zero padding
compress:
  ; Use the domain bitflags in A to set the low byte of D
  sta COMPRESS_D
  ; Load BLOCK_LENGTH into the low byte of B.
  lda BLOCK_LENGTH
  sta COMPRESS_B
  ; Zero out the other bytes of D and B.
  lda #0
  sta COMPRESS_D + 1
  sta COMPRESS_D + 2
  sta COMPRESS_D + 3
  sta COMPRESS_B + 1
  sta COMPRESS_B + 2
  sta COMPRESS_B + 3

  ; If the PARENT flag is set in D, load zeros into T0 and T1. Otherwise
  ; load CHUNK_COUNTER.
  lda COMPRESS_D
  and #PARENT
  beq compress_load_chunk_counter
  lda #0
  sta COMPRESS_T0 + 0
  sta COMPRESS_T0 + 1
  sta COMPRESS_T0 + 2
  sta COMPRESS_T0 + 3
  sta COMPRESS_T1 + 0
  sta COMPRESS_T1 + 1
  sta COMPRESS_T1 + 2
  sta COMPRESS_T1 + 3
  jmp compress_t_loaded
compress_load_chunk_counter:
  lda CHUNK_COUNTER + 0
  sta COMPRESS_T0 + 0
  lda CHUNK_COUNTER + 1
  sta COMPRESS_T0 + 1
  lda CHUNK_COUNTER + 2
  sta COMPRESS_T0 + 2
  lda CHUNK_COUNTER + 3
  sta COMPRESS_T0 + 3
  lda CHUNK_COUNTER + 4
  sta COMPRESS_T1 + 0
  lda CHUNK_COUNTER + 5
  sta COMPRESS_T1 + 1
  lda CHUNK_COUNTER + 6
  sta COMPRESS_T1 + 2
  lda CHUNK_COUNTER + 7
  sta COMPRESS_T1 + 3
compress_t_loaded:

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

  ; Setup is finished. Now do the compression rounds.
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
  jsr print_hex_u32
  inx
  inx
  inx
  inx
  jsr print_hex_u32
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
