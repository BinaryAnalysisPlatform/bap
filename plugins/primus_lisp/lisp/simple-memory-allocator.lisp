(require string)

(defparameter *malloc-max-chunk-size* nil
  "the maximum size of a single memory chunk,
   if nil then there is no limit. ")

(defparameter *malloc-max-arena-size* nil
  "the maximum number of bytes totally allocated by malloc,
   if not set, then there is no limit")

(defparameter *malloc-arena-start* brk
  "the starting address of the malloc arena")

(defparameter *malloc-guard-edges* 0
  "if not nil, then add padding of the specified size
   around allocated chunks")

(defparameter *malloc-guard-pattern* 0xA5
  "a byte that will be used to fill guard edges")

(defparameter *malloc-zero-sentinel* 0
  "a pointer that is returned by (malloc 0)")

(defparameter *malloc-initialize-memory* false
  "if true then initialize allocated memory with *malloc-initial-value*")

(defparameter *malloc-initial-value* 0
  "initialize allocated memory with the said value")

(defun memory/allocate (ptr len)
  (if *malloc-initialize-memory*
      (memory-allocate ptr n *malloc-initial-value*)
    (memory-allocate ptr n)))

(defun malloc (n)
  "allocates a memory region of size N"
  (declare (external "malloc"))
  (if (= n 0) *malloc-zero-sentinel*
    (if (malloc-will-reach-limit n) 0
      (let ((n (+ n (* 2 *malloc-guard-edges*)))
            (ptr brk)
            (failed (memory/allocate ptr n)))
        (if failed 0
          (set brk (+ brk n))
          (malloc/fill-edges ptr n)
          (let ((ptr (+ ptr *malloc-guard-edges*)))
            (dict-add 'malloc/regions ptr n)
            ptr))))))

(defun realloc (ptr len)
  (declare (external "realloc"))
  (let ((len' (dict-get 'malloc/regions ptr)))
    (if (not len') (malloc len)
      (if (>= len' len) ptr
        (let ((new_ptr (malloc len)))
          (if (not new_ptr) new_ptr
            (copy-right new_ptr ptr len')))))))

;; in our simplistic malloc implementation, free is just a nop
(defun free (p)
  "frees the memory region pointed by P"
  (declare (external "free")))

(defun calloc (n s)
  "allocates memory and initializes it with zero"
  (declare (external "calloc"))
  (malloc (* n s))) ; in our implementation malloc zeros memory


(defun malloc-heap-size ()
  (- brk *malloc-arena-start*))


(defun malloc-will-reach-limit (n)
  (or (and *malloc-max-chunk-size*
           (> n *malloc-max-chunk-size*))
      (and *malloc-max-arena-size*
           (> (malloc-heap-size) *malloc-max-arena-size*))))

(defun malloc/fill-edges (ptr n)
  (when *malloc-guard-edges*
    (memset ptr
            *malloc-guard-pattern*
            *malloc-guard-edges*)
    (memset (- (+ ptr n) *malloc-guard-edges*)
            *malloc-guard-pattern*
            *malloc-guard-edges*)))
