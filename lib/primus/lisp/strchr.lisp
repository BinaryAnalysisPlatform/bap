(require pointers)

(defun strchr (p c n)
  (let ((p p) (n n))
    (while (and n (not (points-to c p)))
      (decr n)
      (incr p))
    (if (points-to c p) p 0)))
