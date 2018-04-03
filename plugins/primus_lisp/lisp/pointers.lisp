;; pointer arithmetics

(require types)

(defmacro ptr+ (t p n)
  "(ptr+ T P N) increments N times the
    pointer P to a value of type T."
  (+ p (* (sizeof t) n)))

(defmacro ptr+1 (type p)
  "(ptr+1 T P) increments the pointer P to
   a value of to type T."
  (ptr+ type p 1))

(defmacro endian (f x y)
  "(endian F X Y) expands to (F Y X) if applied
   in the little endian context"
  (declare (context (endian little)))
  (f y x))

(defmacro endian (f x y)
  "(endian F X Y) expands to (F X Y) if applied
   in the big endian context"
  (declare (context (endian big)))
  (f x y))

(defmacro nth-byte-of-word (t i x)
  "(nth-byte-of-word T N X) returns N-th byte
   of the word X that has type T"
  (let ((n (sizeof t))
        (j (endian - n i))
        (k (if (< j n) (- j 1) (+ j n)))
        (hi (- (* 8 (+ k 1)) 1))
        (lo (* k 8)))
    (extract hi lo x)))

(defmacro read-word (t a)
  "(read-word T A) reads a word of type T at address A"
  (let ((p a)
        (x (memory-read p))
        (n (-1 (sizeof t))))
    (while n
      (incr p)
      (decr n)
      (set x (endian concat x (memory-read p))))
    x))

(defmacro write-word (t a x)
  "(write-word T A X) writes the word X of type T to address A"
  (let ((p a)
        (n (sizeof t))
        (i 0))
    (while (< i n)
      (memory-write p (nth-byte-of-word t i x))
      (incr p i))
    p))

(defmacro points-to (t p v)
  "(points-to T P V) return true if t P points
  to a value of type T that is equal to V."
  (= (read-word t p) (cast t v)))

(defmacro array-get (t p n)
  "(array-get T P N) gets the N-th element of the
   array of elements of type T, pointed by P"
  (read-word t (ptr+ t p n)))

(defmacro array-set (t p n w)
  "(array-set T P N W) sets to W the N-th element of the array of
   elements of type T, pointed by P"
  (write-word t (ptr+ p n) w))
