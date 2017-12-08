(require posix)

(defun memcheck-malloc (s p)
  (declare (advice :after "malloc"))
  (memcheck-acquire p s))

(defun memcheck-calloc (n s p)
  (declare (advice :after "calloc"))
  (memcheck-acquire p (* n s)))

(defun memcheck-strdup (src dst)
  (declare (advice :after "strdup"))
  (memcheck-acquire dst (strlen src)))

(defun memcheck-strndup (src n dst)
  (declare (advice :after "strndup"))
  (memcheck-acquire dst n))

(defun memcheck-free (p)
  (declare (advice :before "free"))
  (memcheck-release p))

(defun memcheck-aligned-alloc (a s p)
  (declare (advice :after "aligned-malloc"))
  (memcheck-acquire p s))

(defun memcheck-memalign (b s p)
  (declare (advice :after "memalign"))
  (memcheck-acquire p s))

(defun memcheck-posix-memalign (pp a s r)
  (declare (advice :after "posix-memalign"))
  (when (= 0 r)
    (memcheck-acquire (read-word ptr_t pp) s)))

(defun memcheck-valloc (s p)
  (declare (advice :after "valloc"))
  (memcheck-acquire p s))

(defun memcheck-asprintf (pp fmt r)
  (declare (advice :after "asprintf"))
  (when (>= r 0)
    (let ((p (read-word ptr_t pp)))
      (memcheck-acquire p (strlen p)))))

(defun memcheck-vasprintf (pp fmt valist p)
  (declare (advice :after "vasprintf"))
  (memcheck-asprintf pp fmt p))
