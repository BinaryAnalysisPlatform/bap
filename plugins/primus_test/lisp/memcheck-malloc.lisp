(require memcheck)

(defmethod call (name ptr)
  (when (and ptr (= name 'free))
    (memcheck-release 'malloc ptr)))

(defmethod loaded (ptr)
  (memcheck-access 'malloc ptr))

(defmethod call-return (name len ptr)
  (when (= name 'malloc)
    (memcheck-acquire 'malloc ptr len)))
