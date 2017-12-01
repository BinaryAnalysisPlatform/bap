(require string)

(defun malloc (n)
  "allocates a memory region of size N"
  (declare (external "malloc"))
  (msg "allocating $0 bytes" n)
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
