;; functions to access memory

(defun points-to-null (p)
  "true if P points to a zero byte"
  (is-zero (memory-read p)))

(defun copy-byte (dst src)
  "copies byte from address SRC to DST"
  (memory-write dst (memory-read src)))

(defmacro copy-byte-shift (dst src)
  "copies byte from DST to SRC and increments SRC and DST"
  (prog (copy-byte dst src)
        (incr dst src)))

(defmacro copy-byte-shift-left (dst src)
  "copies byte from DST to SRC and decrements SRC and DST"
  (prog (copy-byte dst src)
        (decr dst src)))

(defmacro make-copy (copy-byte dst src len)
  (let ((ret dst))
    (while len
      (decr len)
      (copy-byte dst src))
    ret))

(defmacro copy-right (dst src len)
  "copies LEN bytes from SRC to DST (left to right)"
  (make-copy copy-byte-shift dst src len))

(defmacro copy-left (dst src len)
  "copies LEN bytes from SRC to DST (right to left)"
  (make-copy copy-byte-shift-left dst src len))
