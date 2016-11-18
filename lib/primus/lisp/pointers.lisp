(require types)

(defun points-to (v p)
  (= (memory-read p) v))

(defmacro ptr+ (type p n)
  (let ((s (sizeof type)))
    (+ p (* s n))))
(defmacro ptr+1 (type p)
  (ptr+ type p 1))
