(in-package posix)

(require types)

(defparameter *lconv-size* (* 20 (sizeof ptr_t)))

(declare (static LCONV))

(defmethod init ()
  (set LCONV brk)
  (+= brk *lconv-size*))

(defun setlocale (_ locale)
  (declare (external "setlocale"))
  locale)

(defun lconv ()
  (declare (external "lconv"))
  LCONV)
