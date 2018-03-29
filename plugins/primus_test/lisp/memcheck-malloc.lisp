(require memcheck)

(defmethod call (name ptr)
  (when (and ptr (= name 'free))
    (memcheck-release 'malloc ptr)))

(defmethod loaded (ptr)
  (memcheck-access 'malloc ptr))

(defmethod stored (ptr)
  (memcheck-access 'malloc ptr))

(defmethod call-return (name len ptr)
  (when (and len ptr (= name 'malloc))
    (memcheck-acquire 'malloc ptr len)))
