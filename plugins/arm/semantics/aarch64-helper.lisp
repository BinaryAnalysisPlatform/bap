;; Helper functions specific to aarch64.

(declare (context (target arm armv8-a+le)))

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

(defun decode-bit-masks (immN imms immr immediate)
  "(decode-bit-masks immN imms immr immediate) returns the immediate value
   corresponding to the immN:immr:imms bit pattern within opcodes of
   ARMv8 logical operation instructions like AND, ORR etc.
   I'm not sure what the immediate parameter does, but it's nearly always
   called with true.
   Modified from ARMv8 ISA pseudocode."
  (let ((memory-width 64) ; change to 32 if 32-bit system
        (len (highest-set-bit (concat immN (lnot imms))))
        (levels (zero-extend (ones len) 6))
        (S (logand imms levels))
        (R (logand immr levels))
        (diff (- S R))) ; assuming "6-bit subtract with borrow" is regular 2'c subtraction
    (assert-msg (>= len 1) "decode-bit-masks len < 1")
    (assert-msg (not (and immediate (= levels (logand imms levels)))) "decode-bit-masks long condition")
    (let ((esize (lshift 1 len))
          (d (extract (- len 1) 0 diff))
          (welem (zero-extend (ones (+ S 1)) esize))
          (telem (zero-extend (ones (+ d 1)) esize))
          (wmask (replicate-to-fill (rotate-right welem R) memory-width))
          (tmask (replicate-to-fill telem memory-width)))
      ; it seems like wmask is for logical immediates, and tmask is not used
      ; anywhere in the ISA except for the BFM instruction and its aliases.
      ; we're just returning wmask here.
      ; TODO: can we return tuples in Primus Lisp?
      wmask)))

(defun immediate-from-bitmask (mask)
  "(immediate-from-bitmask mask) returns the immediate value corresponding to
   the given 13-bit mask in the form of N:immr:imms."
  (let ((N (select 12 mask))
        (immr (extract 11 6 mask))
        (imms (extract 5 0 mask)))
    (decode-bit-masks N imms immr true)))

(defun barrier-option-to-symbol (barrier-type option)
  "(barrier-option-to-symbol barrier-type option) converts the
   barrier type (:dmb, :dsb, :isb) and 4-bit optional value
   to a symbol.
   This is to be used with the (special) primitive."
  (case barrier-type
    :dmb
      (case option
        0b1111 :barrier-dmb-sy
        0b1110 :barrier-dmb-st
        0b1101 :barrier-dmb-ld
        0b1011 :barrier-dmb-ish
        0b1010 :barrier-dmb-ishst
        0b1001 :barrier-dmb-ishld
        0b0111 :barrier-dmb-nsh
        0b0110 :barrier-dmb-nshst
        0b0101 :barrier-dmb-nshld
        0b0011 :barrier-dmb-osh
        0b0010 :barrier-dmb-oshst
        0b0001 :barrier-dmb-oshld
        :barrier-dmb-unknown)
    :dsb
      (case option
        0b1111 :barrier-dsb-sy
        0b1110 :barrier-dsb-st
        0b1101 :barrier-dsb-ld
        0b1011 :barrier-dsb-ish
        0b1010 :barrier-dsb-ishst
        0b1001 :barrier-dsb-ishld
        0b0111 :barrier-dsb-nsh
        0b0110 :barrier-dsb-nshst
        0b0101 :barrier-dsb-nshld
        0b0011 :barrier-dsb-osh
        0b0010 :barrier-dsb-oshst
        0b0001 :barrier-dsb-oshld
        :barrier-dsb-unknown)
    :isb
      :barrier-isb-sy))
