;; functions to access memory

(defun points-to-null (p)
  (is-zero (memory-read p)))

(defun copy-byte (dst src)
  (memory-write dst (memory read src)))

(defmacro copy-byte-shift (dst src)
  (prog (copy-byte dst src)
        (incr dst)
        (incr src)))

(defmacro copy-byte-shift-left (dst src)
  (prog (copy-byte dst src)
        (decr dst)
        (decr src)))

(defmacro make-copy (copy-byte dst src len)
  (let ((ret dst))
    (while len
      (decr len)
      (copy-byte dst src))
    ret))

(defmacro copy-right (dst src len)
  (make-copy copy-byte-shift dst src len))

(defmacro copy-left (dst src len)
  (make-copy copy-byte-shift-left dst src len))
