(defun check-null-pointer (ptr)
  (when (and (not ptr) (all-static-constant ptr))
    (incident-report 'null-pointer-dereference (incident-location))))

(defmethod loading (ptr)
  (check-null-pointer ptr))

(defmethod storing (ptr)
  (check-null-pointer ptr))
