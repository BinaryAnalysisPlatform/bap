;; advises free and functions that are known to return values
;; allocated with malloc. For each allocation function we tell
;; Primus the location and the size of the allocated chunk.

(require posix)

(defun track-malloc (s p)
  (memcheck-acquire p s))

(defun track-calloc (n s p)
  (memcheck-acquire p (* n s)))

(defun track-strdup (src dst)
  (memcheck-acquire dst (strlen src)))

(defun track-strndup (src n dst)
  (memcheck-acquire dst n))

(defun track-free (p)
  (memcheck-release p))

(defun track-aligned-alloc (a s p)
  (memcheck-acquire p s))

(defun track-memalign (b s p)
  (memcheck-acquire p s))

(defun track-posix-memalign (pp a s r)
  (when (= 0 r)
    (memcheck-acquire (read-word ptr_t pp) s)))

(defun track-valloc (s p)
  (memcheck-acquire p s))

(defun track-asprintf (pp fmt r)
  (when (>= r 0)
    (let ((p (read-word ptr_t pp)))
      (memcheck-acquire p (strlen p)))))

(defun track-vasprintf (pp fmt valist p)
  (track-asprintf pp fmt p))

(advice-add track-malloc :after malloc)
(advice-add track-calloc :after calloc)
(advice-add track-strdup :after strdup)
(advice-add track-strncpy :after strncpy)
(advice-add track-aligned-alloc :after aligned-alloc)
(advice-add track-memalign :after memalign)
(advice-add track-posix-memalign :after posix-memalign)
(advice-add track-asprintf :after asprintf)
(advice-add track-vasprintf :after vasprintf)
(advice-add track-valloc :after valloc)
(advice-add track-free :before free)
