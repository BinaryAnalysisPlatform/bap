(in-package posix)

(defun setjmp (_)
  (declare (external "setjmp" "_setjmp"))
  0)

(defun sigsetjmp (_ _)
  (declare (external "sigsetjmp" "_sigsetjmp"))
  0)

(defun longjmp (_ _)
  (declare (external "longjmp" "_longjmp" "siglongjmp" "_siglongjmp"))
  (exit 1))
