(defun putchar (char)
  (declare (external "putchar"))
  (if (= (output-char 0 char) 1) char -1))
