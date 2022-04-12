(in-package core)

(defun logandnot (rd rn)
  "(logandnot X Y) is X & ~Y, i.e., X and complement of Y"
  (logand rd (lnot rn)))

(defun msb (x)
  "(msb X) is the most significant bit of X."
  (select (- (word-width x) 1) x))

(defun lsb (x)
  "(msb X) is the least significant bit of X."
  (select 0 x))

(defun carry (rd rn rm)
  "(carry RD RN RM) is true if the sum RD = RN + RM causes
   the carry out of the msb bit."
  (logor (logand (msb rn) (msb rm))
         (logand (msb rm) (lnot (msb rd)))
         (logand (msb rn) (lnot (msb rd)))))

(defun overflow (rd rn rm)
  "(overflow RD RN RM) is true if the sum RD = RN + RM results
   in two's complement overflow."
  (logor (logand (msb rn) (msb rm) (lnot (msb rd)))
         (logand (lnot (msb rn)) (lnot (msb rm)) (msb rd))))

(defun replicate (bitv n)
  "(replicate bitv n) returns a bitvector with bitv repeated n times."
  (let ((output bitv))
    (while (> n 1)
      (decr n)
      (set output (concat output bitv)))
    output))

(defun zeros (n)
  "(zeros n) returns an empty bitvector of length n."
  (cast-unsigned n 0:1))

(defun ones (n)
  "(ones n) returns a bitvector of length n with all bits set."
  (lnot (zeros n)))
  
(defun rotate-right (bitv n)
  "(rotate-right bitv n) rotates bitv to the right by n positions.
    Carry-out is ignored."
  (if (= n 0)
    bitv
    (let ((bitv-length (word-width bitv))
          (m (mod n bitv-length)))
      ; need to trim the result of logor.
      (extract (- bitv-length 1) 0
        (logor 
          (rshift bitv m)
          (lshift bitv (- bitv-length m)))))))

(defun rotate-left (bitv n)
  "(rotate-right bitv n) rotates bitv to the right by n positions.
    Carry-out is ignored."
  (if (= n 0)
    bitv
    (let ((bitv-length (word-width bitv))
          (m (mod n bitv-length)))
      (extract (- bitv-length 1) 0
        (logor 
          (lshift bitv m)
          (rshift bitv (- bitv-length m)))))))

(defmacro popcount/helper (x sh m1 m2 m4 h01)
  (prog
     (set x (- x (logand (rshift x 1) m1)))
     (set x (+ (logand x m2) (logand (rshift x 2) m2)))
     (set x (logand (+ x (rshift x 4)) m4))
     (rshift (* x h01) sh)))

(defmacro popcount16 (x)
  (popcount/helper x 8
                   0x5555
                   0x3333
                   0x0f0f
                   0x0101))

(defmacro popcount32 (x)
  (popcount/helper x 24
                   0x55555555
                   0x33333333
                   0x0f0f0f0f
                   0x01010101))

(defmacro popcount64 (x)
  (popcount/helper x 56
                   0x5555555555555555
                   0x3333333333333333
                   0x0f0f0f0f0f0f0f0f
                   0x0101010101010101))

(defun clz16 (r)
  (let ((x r))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (logor x (rshift x 8)))
    (set x (lnot x))
    (popcount16 x)))

(defun clz32 (x)
  (let ((x x))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (logor x (rshift x 8)))
    (set x (logor x (rshift x 16)))
    (set x (lnot x))
    (popcount32 x)))

(defun clz64 (x)
  (let ((x x))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (logor x (rshift x 8)))
    (set x (logor x (rshift x 16)))
    (set x (logor x (rshift x 32)))
    (set x (lnot x))
    (popcount64 x)))
