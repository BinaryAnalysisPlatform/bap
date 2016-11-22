(require types)


(defun endian (f x y)
  (declare (context (endian little)))
  (f y x))

(defun endian (f x y)
  (declare (context (endian big)))
  (f x y))

(defmacro nth-byte-of-word (t x i)
  (let ((n (sizeof t))
        (j (endian - n i)))
    (if (< j 0) (+ j n) j)))

(defmacro read-word (t p)
  (let ((p p)
        (x (memory-read p))
        (n (-1 (sizeof t))))
    (while n
      (incr p)
      (decr n)
      (endian cat x (memory-read p)))))

(defmacro write-word (t p)
  (let ((p p)
        (n (sizeof t))
        (i 0))
    (while (< i n)
      (memory-write p (nth-byte-of-word endian t p i))
      (incr p) (incr i))
    p))

(defmacro array-get (t p n)
  (cast t (read-word t (ptr+ t p n))))

(defmacro array-set (t p n w)
  (write-word t (ptr+ p n) w))
