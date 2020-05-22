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

(defparameter *malloc-uniform-min-value* nil
  "if set then defines the lower bound of the uniformely distributed
   random value that is used to represent an unitialized memory ")

(defparameter *malloc-uniform-max-value* nil
  "if set then defines the lower bound of the uniformely distributed
   random value that is used to represent an unitialized memory ")

(defparameter *malloc-initial-value* 0
  "initialize allocated memory with the said value")

(defun memory/allocate (ptr len)
  (if *malloc-initialize-memory*
      (memory-allocate ptr len *malloc-initial-value*)
    (memory-allocate ptr len
                     *malloc-uniform-min-value*
                     *malloc-uniform-max-value*)))

(defun malloc/put-chunk-size (ptr len)
  (write-word ptr_t ptr len))

(defun malloc/get-chunk-size (ptr)
  (let ((header-size (/ (word-width) 8)))
    (read-word ptr_t (- ptr header-size))))

(defun malloc (n)
  "allocates a memory region of size N"
  (declare (external "malloc"))
  (if (= n 0) *malloc-zero-sentinel*
    (if (malloc-will-reach-limit n) 0
      (let ((header-size (/ (word-width) 8))
            (chunk-size (+ n (* 2 *malloc-guard-edges*) header-size))
            (ptr brk)
            (failed (memory/allocate ptr chunk-size)))
        (if failed 0
          (set brk (+ brk chunk-size))
          (malloc/fill-edges ptr chunk-size)
          (set ptr (+ ptr *malloc-guard-edges*))
          (malloc/put-chunk-size ptr n)
          (+ ptr header-size))))))

(defun realloc (ptr len)
  (declare (external "realloc"))
  (if (not ptr) (malloc len)
    (if (not len) (realloc/as-free ptr)
      (realloc/update-chunk ptr len))))

(defun realloc/shrink-chunk (ptr len)
  (malloc/put-chunk-size ptr len)
  ptr)

;; pre: both old-ptr and new-len are not null
(defun realloc/update-chunk (old-ptr new-len)
  (let ((old-len (malloc/get-chunk-size old-ptr)))
    (if (>= old-len new-len) (realloc/shrink-chunk old-ptr new-len)
      (let ((new-ptr (malloc new-len)))
        (when new-ptr
          (memcpy new-ptr old-ptr old-len)
          (free old-ptr))
        new-ptr))))

(defun realloc/as-free (ptr)
  (free ptr)
  *malloc-zero-sentinel*)


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
