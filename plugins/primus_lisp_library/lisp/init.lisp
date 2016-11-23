(defmacro when ($cond$ $body$)
  (if $cond$ $body$ ()))

(defmacro until ($cond$ $body$)
  (if (not $cond) $body ()))


(defun > (x y) (< y x))
(defun /= (x y) (not (= x y)))
(defun +1 (x) (+ x 1))
(defun -1 (x) (- x 1))

(defun non-zero (x) (non (is-zero x)))
(defmacro incr (x) (set x (+1 x)))
(defmacro decr (x) (set x (-1 x)))
(defmacro += (x y) (set x (+ x y)))
