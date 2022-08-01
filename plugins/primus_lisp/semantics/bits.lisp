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
  (cast-signed n 1:1))

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

(defun extract-elem (x e esize off)
  "(extract-elem x e esize off) extracts the e-th bit range
   of size esize of x, after adding the bit offset off."
  (extract
    (+ off (-1 (* esize (+1 e))))
    (+ off (* esize e))
    x))

(defun extract-elem (x e esize)
  "(extract-elem x e esize) extracts the e-th bit range
   of size esize of x."
  (extract-elem x e esize 0))

(defun reverse-in-containers (csize esize x)
  "(reverse-in-containers csize esize x) returns the result
   of reversing the order of elements of elem-size bits
   within each container of container-size bits.
   It returns this as a concatenation of extracts of x."
  (assert (= 0 (mod csize esize)))
  (assert (= 0 (mod (word-width x) csize)))
  (reverse-in-containers/helper csize esize x 0))

(defun reverse-in-containers/helper-elem (csize esize x off e)
  "Returns the result of reversing the elements in one container."
  (declare (visibility :private))
  (if (= e (-1 (/ csize esize)))
    (extract-elem x e esize off)
    (concat
      (extract-elem x e esize off)
      (reverse-in-containers/helper-elem csize esize x off (+1 e)))))

(defun reverse-in-containers/helper (csize esize x c)
  "Maps reverse-in-containers/helper-elem over all containers
   and concatenates the results."
  (declare (visibility :private))  
  (if (= c (-1 (/ (word-width x) csize)))
    (reverse-in-containers/helper-elem csize esize x (* csize c) 0)
    (concat
      (reverse-in-containers/helper csize esize x (+1 c))
      (reverse-in-containers/helper-elem csize esize x (* csize c) 0))))

(defun reverse-bits (x) 
  "(reverse-bits x) returns a bitvector with the bit order of x reversed."
  (reverse-bits/helper x (-1 (word-width x))))

(defun reverse-bits/helper (x i) 
  (if (> i 0)
    (concat (reverse-bits/helper x (-1 i)) (select i x))
    (select i x)))

(defun clz (x)
  "(clz X) counts leading zeros in X.
   The returned value is the number of consecutive zeros starting
   from the most significant bit. Returns 0 for 0 and works for
   inputs of any size, including inputs that are not statically
   known. In the latter case, the computation is unfolded into
   the loopless code with the size proportional to the size of word
   divided by 64."
  (case (word-width x)
    8  (clz8  x)
    16 (clz16 x)
    32 (clz32 x)
    64 (clz64 x)
    (if (> (word-width x) 64)
        (clz/rec x)
      (clz/small x))))

(defun popcount (x)
  "(popcount X) computes the total number of 1 bits in X."
  (if (> (word-width x) 64)
      (+ (popcount64 (cast-high 64 x))
         (popcount (cast-low (- (word-width x) 64) x)))
    (if (= (word-width x) 64)
        (popcount64 x)
      (popcount64 (cast-unsigned 64 x)))))

;; private helpers

(defun popcount/helper (x sh m1 m2 m4 h01)
  (declare (visibility :private))
  (let ((x x))
    (set x (- x (logand (rshift x 1) m1)))
    (set x (+ (logand x m2) (logand (rshift x 2) m2)))
    (set x (logand (+ x (rshift x 4)) m4))
    (rshift (* x h01) sh)))

(defun popcount8 (x)
  (declare (visibility :private))
  (popcount/helper x 0
                   0x55:8
                   0x33:8
                   0x0f:8
                   0x01:8))

(defun popcount16 (x)
  (declare (visibility :private))
  (popcount/helper x 8
                   0x5555:16
                   0x3333:16
                   0x0f0f:16
                   0x0101:16))

(defun popcount32 (x)
  (declare (visibility :private))
  (popcount/helper x 24
                   0x55555555:32
                   0x33333333:32
                   0x0f0f0f0f:32
                   0x01010101:32))

(defun popcount64 (x)
  (declare (visibility :private))
  (popcount/helper x 56
                   0x5555555555555555:64
                   0x3333333333333333:64
                   0x0f0f0f0f0f0f0f0f:64
                   0x0101010101010101:64))

(defun clz8 (r)
  (declare (visibility :private))
  (let ((x r))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (lnot x))
    (popcount8 x)))

(defun clz16 (r)
  (declare (visibility :private))
  (let ((x r))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (logor x (rshift x 8)))
    (set x (lnot x))
    (popcount16 x)))

(defun clz32 (x)
  (declare (visibility :private))
  (let ((x x))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (logor x (rshift x 8)))
    (set x (logor x (rshift x 16)))
    (set x (lnot x))
    (popcount32 x)))

(defun clz64 (x)
  (declare (visibility :private))
  (let ((x x))
    (set x (logor x (rshift x 1)))
    (set x (logor x (rshift x 2)))
    (set x (logor x (rshift x 4)))
    (set x (logor x (rshift x 8)))
    (set x (logor x (rshift x 16)))
    (set x (logor x (rshift x 32)))
    (set x (lnot x))
    (popcount64 x)))

(defun clz/rec (x)
  (declare (visibility :private))
  (if (> (word-width x) 64)
        (if (is-zero (cast-high 64 x))
            (+ 64 (clz (cast-low (- (word-width x) 64) x)))
          (clz64 (cast-high 64 x)))
    (clz x)))

(defun clz/small (x)
  (declare (visibility :private))
  (let ((w (word-width x)))
    (if       (< w 8)  (- (clz8  (cast-unsigned 8 x))  (- 8 w))
      (if     (< w 16) (- (clz16 (cast-unsigned 16 x)) (- 16 w))
        (if   (< w 32) (- (clz32 (cast-unsigned 32 x)) (- 32 w))
          (if (< w 64) (- (clz64 (cast-unsigned 64 x)) (- 64 w))
            (clz x)))))))
