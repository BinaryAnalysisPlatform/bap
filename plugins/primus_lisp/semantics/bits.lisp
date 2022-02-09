(in-package core)

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
