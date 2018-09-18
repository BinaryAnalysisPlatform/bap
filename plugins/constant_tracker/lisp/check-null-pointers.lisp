(defun check-null-pointer (ptr)
  (when (and (not ptr) (all-static-constants ptr))
    (incident-report 'null-pointer-dereference (incident-location))))

(defmethod loading (ptr)
  (check-null-pointer ptr))

(defmethod storing (ptr)
  (check-null-pointer ptr))
