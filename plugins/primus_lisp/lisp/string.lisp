(require types)
(require pointers)
(require memory)

(defun strlen (p)
  (declare (external "strlen"))
  (let ((len 0))
    (while (not (points-to-null p))
      (incr len p))
    len))


(defun strcpy (dst src)
  (declare (external "strcpy"))
  (let ((dst dst))
    (while (not (points-to-null src))
      (copy-byte-shift dst src))
    (memory-write dst 0:8))
  dst)

(defun strncpy (dst src len)
  (declare (external "strncpy"))
  (let ((dst dst))
    (while (and len (not (points-to-null src)))
      (decr len)
      (copy-byte-shift dst src))
    (memory-write dst 0:8))
  dst)

(defun strdup (src)
  (declare (external "strdup"))
  (let ((dst (malloc (+1 (strlen src)))))
    (and dst (strcpy dst src))))


(defun memmove (dst src len)
  (declare (external "memmove"))
  (let ((a dst) (b src))
    (when (/= src dst)
      (if (> src dst) (copy-right a b len)
        (+= a (-1 len))
        (+= b (-1 len))
        (copy-left a b len))))
  dst)

(defun memcpy (dst src len)
  (declare (external "memcpy"))
  (copy-right dst src len))

(defun memccpy (dst src c len)
  (declare (external "memccpy"))
  (let ((found 0))
    (while (and len (not found))
      (copy-byte dst src)
      (incr src)
      (decr len)
      (when (points-to char dst c)
        (set found (+ 1 dst)))
      (incr dst))
    found))


(defmacro find-character (dir p c n)
  (prog
   (while (and n (not (points-to char p c)))
    (decr n)
    (dir p))
   (if (points-to char p c) p 0)))

(defun memchr (p c n)
  (declare (external "memchr"))
  (find-character incr p c n))


(defun memrchr (p c n)
  (declare (external "memrchr"))
  (find-character decr p c n))


(defun strchr (p c)
  (declare (external "strchr"))
  (memchr p c (+ (strlen p) 1)))

(defun strrchr (p c)
  (declare (external "strrchr"))
  (memrchr p c (+ (strlen p) 1)))


(defun strpbrk (str set)
  (declare (external "strpbrk"))
  (let ((p set) (found 0))
    (while (and
            (not (points-to-null p))
            (not found))
      (set found (strchr str (cast int (memory-read p))))
      (incr p))
    found))


(defun memset (p c n)
  (declare (external "memset"))
  (let ((p p))
    (while n
      (memory-write p c)
      (incr p)
      (decr n)))
  p)

(defun memcmp (p1 p2 n)
  (declare (external "memcmp"))
  (let ((res 0) (i 0))
    (while (and (< i n) (not res))
      (set res (compare (memory-read p1) (memory-read p2)))
      (incr p1 p2 i))
    res))

(defun strlen/with-null (s)
  "returns a length of the string S
   (including the terminating null character)"
  (+1 (strlen s)))

(defun strncmp (s1 s2 n)
  (declare (external "strncmp"))
  (memcmp s1 s2 (min n
                     (strlen/with-null s1)
                     (strlen/with-null s2))))

(defun strcmp (s1 s2)
  (declare (external "strcmp"))
  (memcmp s1 s2 (min (strlen/with-null s1)
                     (strlen/with-null s2))))
