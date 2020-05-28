(require memcheck)

(defmethod call (name ptr)
  (when (and ptr (= name 'free)
             (not (= ptr *malloc-zero-sentinel*)))
    (memcheck-release 'malloc ptr)))

(defmethod loaded (ptr)
  (memcheck-access 'malloc ptr))

(defmethod stored (ptr)
  (memcheck-access 'malloc ptr))

(defmethod call-return (name len ptr)
  (when (and len ptr (= name 'malloc))
    (memcheck-acquire 'malloc ptr len)))


(defmethod call (name dst src len)
  (when (is-in name 'memmove 'memcpy 'memcmp)
    (check/both dst src len)))

(defmethod call (name dst src len)
  (when (is-in name 'strncpy 'strncmp)
    (check/strn* dst len)
    (check/strn* src len)))

(defmethod call (name dst src c len)
  (when (= name 'memccpy)
    (check/both dst src len)))

(defmethod call-return (name dst len)
  (when (= name 'strlen)
    (memcheck-bounds 'malloc dst len)))

(defmethod call (name ptr c len)
  (when (is-in name 'memchr 'memrchr)
    (memcheck-bounds 'malloc ptr len)))

(defun check/both (dst src len)
  (memcheck-bounds 'malloc src len)
  (memcheck-bounds 'malloc dst len))

(defun check/strn* (str len)
  (memcheck-bounds 'malloc str (min len (strlen str))))
