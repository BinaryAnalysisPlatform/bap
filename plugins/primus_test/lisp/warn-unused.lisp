(require check-value)

(defmethod call-return (name _ ret)
  (when (= name
           'malloc
           'getenv
           'mkdtemp
           'mkstemp
           'posix_openpt)
    (check-value ret)))


(defmethod call-return (name _ _ ret)
  (when (= name 'realpath)
    (check-value ret)))
