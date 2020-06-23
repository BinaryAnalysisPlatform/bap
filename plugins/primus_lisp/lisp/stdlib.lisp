(require string)
(require atoi)
(require stdio)
(require simple-memory-allocator)
(require types)

(defun getenv (name)
  "finds a value of an environment variable with the given name"
  (declare (external "getenv"))
  (let ((p environ)
        (n (strlen name)))
    (while (and (not (points-to-null p))
                (/= (memcmp (read-word ptr_t p) name n) 0))
      (set p (ptr+1 ptr_t p)))
    (if (and (not (points-to-null p)) (/= p environ))
        (let ((p (read-word ptr_t p)))
          (if (= (memory-read (+ p n)) ?=) (+ p n 1) 0))
      0)))


(defun abort ()
  "terminates program with exit code 1"
  (declare (external "abort"))
  (exit-with 1))


(defun exit (code)
  (declare (external "exit" "_exit"))
  (exit-with code))


(defun atexit (cb)
  (declare (external "atexit"))
  0)

(defun abs (x)
  (declare (external "abs"))
  (if (is-negative x) (neg x) x))
