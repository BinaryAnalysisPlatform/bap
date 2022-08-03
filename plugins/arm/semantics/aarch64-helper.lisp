;; Helper functions specific to aarch64.
(declare (context (target arm-family)
                  (bits 64)))

(in-package aarch64)

(require bits)
(require arm-bits)

(defun word () (word-width))

(defun shift-encoded (rm off)
  "(shift-encoded rm off) decodes the 8-bit shift value
   into its type and offset, and shifts rm accordingly."
  (let ((typ (extract 7 6 off))
        (off (extract 5 0 off)))
    (case typ
      0b00 (lshift rm off)
      0b01 (rshift rm off)
      0b10 (arshift rm off)
      0b11 (rotate-right rm off)
      )))

(defun unsigned-extend (n rm)
  "(unsigned-extend n rm) returns the unsigned extension (prepend with zeros)
   of the lowest n bits of rm."
  (cast-unsigned (word-width) (cast-low n rm)))

(defun signed-extend (n rm)
  "(signed-extend n rm) returns the signed extension (prepend with rm[n-1])
   of the lowest n bits of rm."
  (cast-signed (word-width) (cast-low n rm)))

(defun extended (rm bits)
  "(extended rm bits) decodes the extension type and amount from bits,
   and returns the value of the extension on rm."
  (let ((typ (extract 5 3 bits))
        (off (extract 2 0 bits)))
    (lshift (case typ
              0b000 (unsigned-extend 8 rm)
              0b001 (unsigned-extend 16 rm)
              0b010 (unsigned-extend 32 rm)
              0b011 rm
              0b100 (signed-extend 8 rm)
              0b101 (signed-extend 16 rm)
              0b110 (signed-extend 32 rm)
              0b111 rm)
            off)))

(defun decode-bit-masks (immN imms immr immediate register-width)
  "(decode-bit-masks immN imms immr immediate register-width) returns the immediate value
   corresponding to the immN:immr:imms bit pattern within opcodes of
   ARMv8 logical operation instructions like AND, ORR etc.
   register-width denotes the width of the registers to be acted on (32 or 64).
   I'm not sure what the immediate parameter does, but it's nearly always
   called with true.
   Modified from ARMv8 ISA pseudocode."
  (let ((len (- 64 (clz (cast-unsigned 64 (concat immN (lnot imms)))) 1))
        (levels (cast-unsigned 6 (ones len)))
        (S (logand imms levels))
        (R (logand immr levels))
        (diff (- S R))) ; assuming "6-bit subtract with borrow" is regular 2'c subtraction
    (assert-msg (>= len 1) "decode-bit-masks len < 1")
    (assert-msg (not (and immediate (= levels (logand imms levels)))) "decode-bit-masks long condition")
    (let ((esize (lshift 1 len))
          (d (extract (- len 1) 0 diff))
          (welem (cast-unsigned esize (ones (+ S 1))))
          (telem (cast-unsigned esize (ones (+ d 1))))
          (wmask (replicate-to-fill (rotate-right welem R) register-width))
          (tmask (replicate-to-fill telem register-width)))
      ; it seems like wmask is for logical immediates, and tmask is not used
      ; anywhere in the ISA except for the BFM instruction and its aliases.
      ; we're just returning wmask here.
      wmask)))

(defun immediate-from-bitmask (mask register-width)
  "(immediate-from-bitmask mask register-width) returns the immediate value corresponding to
   the given 13-bit mask in the form of N:immr:imms.
   register-width denotes the width of the registers to be acted on (32 or 64)."
  (let ((N (select 12 mask))
        (immr (extract 11 6 mask))
        (imms (extract 5 0 mask)))
    (decode-bit-masks N imms immr true register-width)))

(defun barrier-option-to-symbol (option)
  "(barrier-option-to-symbol option) converts the
   4-bit value to a symbol.
   This is to be used with the (intrinsic) primitive."
  (case option
    0b1111 'sy
    0b1110 'st
    0b1101 'ld
    0b1011 'ish
    0b1010 'ishst
    0b1001 'ishld
    0b0111 'nsh
    0b0110 'nshst
    0b0101 'nshld
    0b0011 'osh
    0b0010 'oshst
    0b0001 'oshld
    'unknown))

(defun bitvec-to-symbol (x sym)
  "(bitvec-to-symbol x sym) returns the symbol concatenation of
   sym and the hexadecimal representation of x."
  (if (> (word-width x) 0)
    (bitvec-to-symbol
      (cast-low (- (word-width x) 4) x) 
      (symbol-concat 
        sym
        (case (cast-high 4 x)
            0x0 '0
            0x1 '1
            0x2 '2
            0x3 '3
            0x4 '4
            0x5 '5
            0x6 '6
            0x7 '7
            0x8 '8
            0x9 '9
            0xa 'a
            0xb 'b
            0xc 'c
            0xd 'd
            0xe 'e
            0xf 'f)))
    sym))

(defun replace-bit-range (reg hi lo val size)
  "(replace-bit-range reg hi lo val) returns reg with bits
   hi to lo inclusive set to the value stored in val."
  (let ((mask (lshift (cast-unsigned size (ones (+ (- hi lo) 1))) lo))
        (cleared (logand reg (lnot mask)))
        (result (logor cleared (logand mask (lshift (cast-unsigned size val) lo)))))
    result))

(defun insert-element-into-vector (vd index element size)
  "(insert-element-into-vector vd index element size) inserts element into vd[index], 
   where size is in {8,16,32,64}"
  (let ((highIndex (-1 (* size (+ index 1))))
        (lowIndex (* size index)))
    (set$ vd (replace-bit-range vd highIndex lowIndex element 128))))

(defun replicate-and-insert (vd element esize dsize)
  "(replicate-and-insert vd element esize dsize) replicates and concatenates 
   an element of esize to dsize and sets the vector register vd"
  (set$ vd (replicate-and-insert-helper element esize dsize 1)))

(defun replicate-and-insert-helper (element esize dsize index)
  (if (< (* index esize) dsize)
    (concat element (replicate-and-insert-helper element esize dsize (+ index 1)))
    element))

(defun get-vector-S-element (index vn)
  "(get-vector-S-element index vn) returns the 32 bit element from vn[index]"
  (case index
    0x0 (extract 31 0 vn)
    0x1 (extract 63 32 vn)
    0x2 (extract 95 64 vn)
    0x3 (extract 127 96 vn)
    0x0))

(defun load-dbyte (address) 
  "(load-dbyte address) loads two bytes from memory."
  (load-bits 16 address))

(defun mem-read (address size)
  "(mem-read address size) loads size bytes from memory at address."
  (case size
    1   (load-byte  address)
    2   (load-dbyte address)
    4   (load-hword address)
    8   (load-word  address)
    16  (concat (load-word address) (load-word (+ address 8)))))

(defun register-pair-concat (r-pair)
  "(register-pair-concat r-pair) returns the concatenated values of
   the register pair returned by LLVM, taking into account
   the endianness."
  (case (symbol r-pair)
    'X0_X1   (endian concat X0 X1)
    'X2_X3   (endian concat X2 X3)
    'X4_X5   (endian concat X4 X5)
    'X6_X7   (endian concat X6 X7)
    'X8_X9   (endian concat X8 X9)
    'X10_X11 (endian concat X10 X11)
    'X12_X13 (endian concat X12 X13)
    'X14_X15 (endian concat X14 X15)
    'X16_X17 (endian concat X16 X17)
    'X18_X19 (endian concat X18 X19)
    'X20_X21 (endian concat X20 X21)
    'X22_X23 (endian concat X22 X23)
    'X24_X25 (endian concat X24 X25)
    'X26_X27 (endian concat X26 X27)
    'X28_X29 (endian concat X28 X29)
    'W0_W1   (endian concat W0 W1)
    'W2_W3   (endian concat W2 W3)
    'W4_W5   (endian concat W4 W5)
    'W6_W7   (endian concat W6 W7)
    'W8_W9   (endian concat W8 W9)
    'W10_W11 (endian concat W10 W11)
    'W12_W13 (endian concat W12 W13)
    'W14_W15 (endian concat W14 W15)
    'W16_W17 (endian concat W16 W17)
    'W18_W19 (endian concat W18 W19)
    'W20_W21 (endian concat W20 W21)
    'W22_W23 (endian concat W22 W23)
    'W24_W25 (endian concat W24 W25)
    'W26_W27 (endian concat W26 W27)
    'W28_W29 (endian concat W28 W29)))

(defun store-pair (scale indexing t1 t2 dst imm) 
  "(store-pair scale indexing t1 t2 dst imm)
   stores the pair t1,t2 of size (8 << scale) at the register dst plus an offset, 
   using the specified indexing (either 'post, 'pre or 'offset)."
  (assert-msg (= (word-width t1) (word-width t2) (lshift 8 scale))
    "store-pair: scale must match size of register ") 
  (let ((off (lshift (cast-signed 64 imm) scale))
        (datasize (lshift 8 scale))
        (addr
          (case indexing
            'post dst
            'pre (+ dst off)
            'offset (+ dst off)
            (assert-msg false "store-pair invalid indexing scheme"))))
    (store-word addr t1)
    (store-word (+ addr (/ datasize 8)) t2)
    (case indexing
       'post (set$ dst (+ addr off))
       'pre  (set$ dst addr)
       'offset )
    ))
