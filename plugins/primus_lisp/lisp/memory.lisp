;; functions to access memory

(defun points-to-null (p)
  "(points-to-null P) true if P points to a zero byte"
  (is-zero (memory-read p)))

(defun copy-byte (dst src)
  "(copy-byte DST SRC) copies byte from
   address SRC to DST."
  (memory-write dst (memory-read src)))

(defmacro copy-byte-shift (dst src)
  "(copy-byte-shift DST SRC) copies byte from
   DST to SRC and increments SRC and DST."
  (prog (copy-byte dst src)
        (incr dst src)))

(defmacro copy-byte-shift-left (dst src)
  "(copy-byte-shift-left DST SRC) copies
   byte from DST to SRC and decrements SRC and DST."
  (prog (copy-byte dst src)
        (decr dst src)))

(defmacro make-copy (copy-byte dst src len)
  "<internal>"
  (let ((ret dst))
    (while len
      (decr len)
      (copy-byte dst src))
    ret))

(defmacro copy-right (dst src len)
  "(copy-right DST SRC LEN) copies LEN bytes
    from SRC to DST (left to right)"
  (make-copy copy-byte-shift dst src len))

(defmacro copy-left (dst src len)
  "(copy-left DST SRC LEN) copies LEN bytes
    from SRC to DST (right to left)"
  (make-copy copy-byte-shift-left dst src len))
