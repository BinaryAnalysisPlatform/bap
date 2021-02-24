(require string)
(require types)

(defparameter *malloc-max-chunk-size* nil
  "the maximum size of a single memory chunk,
   if nil then there is no limit. ")

(defparameter *malloc-max-arena-size* nil
  "the maximum number of bytes totally allocated by malloc,
   if not set, then there is no limit")

(defparameter *malloc-arena-initial-size* 0x40000
  "the maximum number of bytes totally allocated by malloc,
   if not set, then there is no limit")

(defparameter *malloc-arena-start* brk
  "the starting address of the malloc arena")

(defparameter *malloc-arena-end* brk
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

(defparameter *malloc/brk* brk)

(defparameter *malloc/total-bytes-allocated* 0)

(defun malloc (n)
  "allocates a memory region of size N"
  (declare (external "malloc"))
  (if (= n 0) *malloc-zero-sentinel*
    (if (malloc-will-reach-limit n) 0
      (malloc/grow-arena-if-needed n)
      (+= *malloc/total-bytes-allocated* n)
      (let ((header-size (sizeof int))
            (chunk-size (+ n (* 2 *malloc-guard-edges*) header-size))
            (ptr *malloc/brk*))
        (malloc/initialize ptr chunk-size)
        (+= *malloc/brk* chunk-size)
        (malloc/fill-edges ptr chunk-size)
        (+= ptr *malloc-guard-edges*)
        (malloc/put-chunk-size ptr n)
        (+ ptr header-size)))))

(defun brk ()
  (declare (external "brk"))
  brk)

(defun sbrk (increment)
  (declare (external "sbrk"))
  (+= brk increment))

(defun realloc (ptr len)
  (declare (external "realloc"))
  (if (not ptr) (malloc len)
    (if (not len) (realloc/as-free ptr)
      (realloc/update-chunk ptr len))))


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
  (let ((*malloc-initialize-memory* true)
        (*malloc-initial-value* 0))
    (malloc (* n s))))


(defun malloc-heap-size ()
  *malloc/total-bytes-allocated*)


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


(defun malloc/allocate-arena (len)
  (set *malloc-arena-start* brk)
  (+= brk len)
  (set *malloc-arena-end* brk)
  (if *malloc-initialize-memory*
      (memory-allocate *malloc-arena-start*
                       len
                       *malloc-initial-value*)
    (memory-allocate *malloc-arena-start*
                     len
                     *malloc-uniform-min-value*
                     *malloc-uniform-max-value*)))

(defun malloc/initialize (ptr len)
  (if *malloc-initialize-memory*
      (memory-allocate ptr len *malloc-initial-value*)
    (when (or *malloc-uniform-min-value*
              *malloc-uniform-max-value*)
      (memory-allocate ptr len
                       *malloc-uniform-min-value*
                       *malloc-uniform-max-value*))))

(defun malloc/grow-arena-if-needed (len)
  (let ((free-space (- *malloc-arena-end* *malloc/brk*)))
    (when (> len free-space)
      (malloc/allocate-arena (max *malloc-arena-initial-size* len)))))

(defun realloc/shrink-chunk (ptr len)
  (malloc/put-chunk-size ptr len)
  ptr)

(defun malloc/put-chunk-size (ptr len)
  (write-word int ptr len))

(defun malloc/get-chunk-size (ptr)
  (read-word int (- ptr (sizeof int))))
