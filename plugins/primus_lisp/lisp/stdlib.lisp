(require string)
(require atoi)
(require stdio)

(defun malloc (n)
  "allocates a memory region of size N"
  (declare (external "malloc"))
  (if (= n 0) brk
    (let ((ptr brk)
          (failed (memory-allocate ptr n)))
      (if failed 0
        (set brk (+ brk n))
        ptr))))

;; in our simplistic malloc implementation, free is just a nop
(defun free (p)
  "frees the memory region pointed by P"
  (declare (external "free")))

(defun calloc (n s)
  "allocates memory and initializes it with zero"
  (declare (external "calloc"))
  (let ((size (* n s))
        (ptr (malloc size)))
    (memset ptr 0 size)))

(defun getenv (name)
  "finds a value of an environemnt variable with the given name"
  (declare (external "getenv"))
  (let ((p environ))
    (while (and (not (points-to-null p))
                (/= (strcmp p name) 0))
      (ptr+1 ptr_t p))
    (if p (strchr p ?=) p)))


(defun abort ()
  "terminates program with exit code 1"
  (declare (external "abort"))
  (msg "abort!")
  (exit-with 1))


(defun atexit (cb)
  (declare (external "atexit")))

(defun stub ()
  "stubs that does nothing"
  (declare (external
            "setlocale"
            "bindtextdomain"
            "textdomain"
            "__cxa_atexit"
            "__ctype_get_mb_cur_max"
            "__ctype_b_loc"
            "__do_global_dtors_aux")))
