(require memory)

(defun putchar (char)
  (declare (external "putchar"))
  (if (= (output-char 0 char) 1) char -1))

(defun puts (p)
  (declare (external "puts"))
  (while (not (points-to-null p))
    (putchar (memory-read p))
    (incr p)))
