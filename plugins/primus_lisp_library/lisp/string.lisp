(require types)
(require pointers)
(require memory)

(defun strcpy (dst src)
  (let ((dst dst))
    (while (/= (points-to-null p))
      (copy-byte-shift dst src))
    (memory-write dst 0:8))
  dst)

(defun strncpy (dst src len)
  (let ((dst dst))
    (while (and len (/= (points-to-null p)))
      (decr len)
      (copy-byte-shift dst src))
    (memory-write dst 0:8))
  dst)

(defun memmove (dst src len)
  (declare (external "memmove"))
  (let ((a dst) (b src))
    (when (/= src dst)
      (if (> src dst) (copy-right a b len)
        (prog
         (+= a (-1 len))
         (+= b (-1 len))
         (copy-left a b len)))))
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
      (when (points-to char_t dst)
        (set found (+ 1 dst)))
      (incr dst))
    found))

(defun strchr (p c n)
  (while (and n (not (points-to char_t p c)))
    (decr n)
    (incr p))
  (if (points-to char_t p c) p 0))

(defun memset (p c n)
  (let ((p p))
    (while n
      (memory-write p c)
      (incr p)))
  p)

(defun compare (x y)
  (if (< x y) -1 (if (> x y) 1) 0))

(defun memcmp (p1 p2 n)
  (let ((res 0) (i 0))
    (while (and (< i n) (not res))
      (set res (compare (memory-read p1) (memory-read p2)))
      (incr p1 p2 i))))


;; (defun memmem (p m c n)
;;   (if (> m n) 0
;;     (prog )))
