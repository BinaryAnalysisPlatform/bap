;;; Taint Analysis Framework
;;;

(defparameter *indirect-taint-size* nil)

(defun taint-introduce (r k v)
  "(taint-introduce REL KIND VAL) associates a fresh new taint with
   the value VAL using the specified relation REL, that must be either
   'directly or 'indirectly. If taint is introduced indirectly, then
   *indirect-taint-size* number of bytes are tainted."
  (if (= r 'directly)
      (taint-introduce-directly k v)
    (let ((n (or *indirect-taint-size* (/ (word-width) 8))))
      (taint-introduce-indirectly k v n))))
