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


(defun malloc (n)
  "allocates a memory region of size N"
  (declare (external "malloc"))
  (if (= n 0) brk
    (if (malloc-will-reach-limit n) 0
      (let ((n (+ n (* 2 *malloc-guard-edges*)))
            (ptr brk)
            (failed (memory-allocate ptr n 0)))
        (if failed 0
          (set brk (+ brk n))
          (malloc/fill-edges ptr n)
          (+ ptr *malloc-guard-edges*))))))

;; in our simplistic malloc implementation, free is just a nop
(defun free (p)
  "frees the memory region pointed by P"
  (declare (external "free")))

(defun calloc (n s)
  "allocates memory and initializes it with zero"
  (declare (external "calloc"))
  (malloc (* n s)))


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
