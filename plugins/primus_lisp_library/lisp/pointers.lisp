;; pointer arithmetics

(require types)


(defmacro ptr+ (t p n)
  "increments a pointer N times"
  (+ p (* (sizeof t) n)))

(defmacro ptr+1 (type p)
  "increments a pointer"
  (ptr+ type p 1))

(defmacro endian (f x y)
  "word endianness"
  (declare (context (endian little)))
  (f y x))

(defmacro endian (f x y)
  "word endianness"
  (declare (context (endian big)))
  (f x y))

(defmacro nth-byte-of-word (t i x)
  "extracts nth byte of a word, based on endianness."
  (let ((n (sizeof t))
        (j (endian - n i))
        (k (if (< j 0) (+ j n) j))
        (hi (- (* 8 (+ k 1)) 1))
        (lo (* k 8)))
    (coerce hi lo x)))

(defmacro read-word (t p)
  "reads a word of type T at address P"
  (let ((p p)
        (x (memory-read p))
        (n (-1 (sizeof t))))
    (while n
      (incr p)
      (decr n)
      (endian cat x (memory-read p)))))

(defmacro write-word (t p x)
  "writes a word of type T to address P"
  (let ((p p)
        (n (sizeof t))
        (i 0))
    (while (< i n)
      (memory-write
       p (nth-byte-of-word t i x))
      (incr p i))
    p))

(defmacro points-to (t p v)
  "reads a word from address P and compares it with V"
  (= (read-word t p) v))

(defmacro array-get (t p n)
  "gets N-th element of an array P"
  (read-word t (ptr+ t p n)))

(defmacro array-set (t p n w)
  "sets N-th element of an array P"
  (write-word t (ptr+ p n) w))
