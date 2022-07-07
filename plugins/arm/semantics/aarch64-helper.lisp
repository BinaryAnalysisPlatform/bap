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

(defun replace-bit-range (reg hi lo val)
  "(replace-bit-range reg hi lo val) returns reg with bits
   hi to lo inclusive set to the value stored in val."
  (let ((mask (lshift (cast-unsigned (word-width reg) (ones (+ (- hi lo) 1))) lo))
        (cleared (logand reg (lnot mask)))
        (result (logor cleared (logand mask (lshift (cast-unsigned (word-width reg) val) lo)))))
    result))

(defun replace-bit-range-v (reg hi lo val size)
  "(replace-bit-range reg hi lo val) returns reg with bits
   hi to lo inclusive set to the value stored in val."
  (let ((mask (lshift (cast-unsigned size (ones (+ (- hi lo) 1))) lo))
        (cleared (logand reg (lnot mask)))
        (result (logor cleared (logand mask (lshift (cast-unsigned (word-width reg) val) lo)))))
    result))


(defun reverse-elems-in-one-container (elem-size c)
  "(reverse-elems-in-one-container elem-size c) reverses the order
   of each group of elem-size bits in c.
   For non-vector instructions, elem-size = 8.
   If c's width is not a multiple of elem-size, the remaining bits
   get appended at the end."
  (if (<= (word-width c) elem-size) c
    (concat
      (cast-low elem-size c)
      (reverse-elems-in-one-container elem-size
        (cast-high (- (word-width c) elem-size) c)))))

(defun reverse-elems-in-all-containers (container-size elem-size x)
  "(reverse-elems-in-all-containers container-size elem-size x) applies
   reverse-elems-in-one-container to each group of container-size bits in x.
   In other words, it reverses the order of groups of elem-size bits within
   each group of container-size bits.
   If x's width is not a multiple of container-size, the remaining bits
   get appended at the end."
  (if (< (word-width x) container-size) x
    (concat
      (reverse-elems-in-one-container elem-size (cast-high container-size x))
      (reverse-elems-in-all-containers container-size elem-size
        (cast-low (- (word-width x) container-size) x)))))

(defun insert-element-into-vector (vd index element size)
	"(insert-element-into-vector vd index element size) inserts element into vd[index], where size is in {8,16,32,64}"
	(let ((highIndex (- (* size (+ index 1)) 1))
				(lowIndex (* size index))
				(newVal (replace-bit-range-v v highIndex lowIndex element 128)))
		(set$ vd newVal)))

(defun get-vector-S-element (index vn)
	"(get-vector-S-element) returns the 32 bit element from vn[index]"
  (case index
    0x0 (extract 31 0 vn)
    0x1 (extract 63 32 vn)
    0x2 (extract 95 64 vn)
    0x3 (extract 127 96 vn)
    0x0))

(defun load-dbyte (address) 
	"(load-dbyte address) loads two bytes from memory."
	(load-bits 16 address))

(defun get-first-128b-reg (qa_qb)
	"(get-first-128b-reg qa_qb) returns the first register of a pair of vector registers."
	(case (symbol qa_qb)
		'Q0_Q1	'Q0
		'Q1_Q2	'Q1
		'Q2_Q3	'Q2
		'Q3_Q4	'Q3
		'Q4_Q5	'Q4
		'Q5_Q6	'Q5
		'Q6_Q7	'Q6
		'Q7_Q8	'Q7
		'Q8_Q9	'Q8
		'Q9_Q10	'Q9
		'Q10_Q11	'Q10
		'Q11_Q12	'Q11
		'Q12_Q13	'Q12
		'Q13_Q14	'Q13
		'Q14_Q15	'Q14
		'Q15_Q16	'Q15
		'Q16_Q17	'Q16
		'Q17_Q18	'Q17
		'Q18_Q19	'Q18
		'Q19_Q20	'Q19
		'Q20_Q21	'Q20
		'Q21_Q22	'Q21
		'Q22_Q23	'Q22
		'Q23_Q24	'Q23
		'Q24_Q25	'Q24
		'Q25_Q26	'Q25
		'Q26_Q27	'Q26
		'Q27_Q28	'Q27
		'Q28_Q29	'Q28
		'Q29_Q30	'Q29
		'Q30_Q31	'Q30
		'Q0))

(defun get-second-128b-reg (qa_qb)
	"(get-second-128b-reg qa_qb) returns the first register of a pair of vector registers."
	(case (symbol qa_qb)
		'Q0_Q1	'Q1
		'Q1_Q2	'Q2
		'Q2_Q3	'Q3
		'Q3_Q4	'Q4
		'Q4_Q5	'Q5
		'Q5_Q6	'Q6
		'Q6_Q7	'Q7
		'Q7_Q8	'Q8
		'Q8_Q9	'Q9
		'Q9_Q10	'Q10
		'Q10_Q11	'Q11
		'Q11_Q12	'Q12
		'Q12_Q13	'Q13
		'Q13_Q14	'Q14
		'Q14_Q15	'Q15
		'Q15_Q16	'Q16
		'Q16_Q17	'Q17
		'Q17_Q18	'Q18
		'Q18_Q19	'Q19
		'Q19_Q20	'Q20
		'Q20_Q21	'Q21
		'Q21_Q22	'Q22
		'Q22_Q23	'Q23
		'Q23_Q24	'Q24
		'Q24_Q25	'Q25
		'Q25_Q26	'Q26
		'Q26_Q27	'Q27
		'Q27_Q28	'Q28
		'Q28_Q29	'Q29
		'Q29_Q30	'Q30
		'Q30_Q31	'Q31
		'Q0))

(defun mem-read (address size)
	"(mem-read address size) loads size bytes from memory at address."
	(case size
		1		(load-byte address)
		2		(load-dbyte address)
		4		(load-hword	address)
		8		(load-word	address)
		16	(concat (load-word address) (load-word (+ address 8)))))
