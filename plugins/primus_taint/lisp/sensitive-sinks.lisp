;; defines arguments to certain functions as sensitive and establishes
;; a policy that they should either be checked (must/check) if they
;; are tainted with the untrusted input, or never be tainted at all
;; (must/trust)


;; mark all taints that affected branch control, this gives at least
;; a hope that the untrusted input was checked and validated.

(require types)
(require pointers)

(defmethod jumping (c _)
  (let ((t (taint-get-direct 'untrusted c)))
    (when t
      (dict-add 'untrusted/checked t (incident-location)))))

(defun must/check (v)
  (let ((t (taint-get-direct 'untrusted v))
        (c (dict-get 'untrusted/checked t)))
    (when (and t (not c))
      (incident-report 'unchecked-untrusted-argument
                       (incident-location)
                       (dict-get 'taint-sources/untrusted t)))))

(defun must/trust (v)
  (let ((t (taint-get-direct 'untrusted v)))
    (when t
      (incident-report 'untrusted-argument
                       (incident-location)
                       (dict-get 'taint-sources/untrusted t)))))

(defun must/trust-string (s)
  (while (not (points-to-null s))
    (must/trust (memory-read s))
    (incr s)))

(defmethod call (name m n)
  (when (= name 'calloc)
    (must/check m)
    (must/check n)))

(defmethod call (name n)
  (when (is-in name 'malloc)
    (must/check n)))

(defmethod call (name _ n)
  (when (is-in name 'div 'ldiv 'lldiv)
    (must/check n)))

(defmethod call (name ptr aln len)
  (when (= name 'posix_memalign)
    (must/check len)))

(defmethod call (name ptr len)
  (when (= name 'realloc)
    (must/check len)))

(defmethod call (name x y len)
  (when (is-in name
               'memchr 'memcmp 'memcpy 'memmove 'memset
               'strncasecmp 'strpcpy)
    (must/check len)))

(defmethod call (name cmd)
  (when (is-in name 'system 'popen)
    (must/trust-string cmd)))


(defmethod call (name path arg)
  (when (is-in name 'execl 'execle 'execlp 'execv 'execve 'execvp)
    (must/trust-string path)
    (must/trust-string arg)))

(defun must/trust-args (args)
  (while args
    (must/trust-string (read-word ptr_t args))
    (ptr+1 ptr_t args)))

(defmethod call (name pid path fa attr args envp)
  (must/trust-string path)
  (must/trust-args args)
  (must/trust-args envp))
