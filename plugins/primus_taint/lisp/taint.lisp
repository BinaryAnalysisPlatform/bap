;;; Taint Analysis Framework
;;;


(defun taint-introduce (r k v)
  "(taint-introduce REL KIND VAL) associates a fresh new taint with
   the value VAL using the specified relation REL, that must be either
   'directly or 'indirectly"
  (if (= r 'directly)
      (taint-introduce-directly k v)
    (taint-introduce-indirectly k v)))
