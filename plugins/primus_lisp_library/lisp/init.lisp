(defmacro when ($cond$ $body$)
  (if $cond$ $body$ ()))

(defmacro until ($cond$ $body$)
  (if (not $cond) $body ()))


(defun > (x y) (< y x))
(defun /= (x y) (not (= x y)))
(defun +1 (x) (+ x 1))
(defun -1 (x) (- x 1))

(defun non-zero (x) (non (is-zero x)))
(defmacro += (x y) (set x (+ x y)))


(defmacro incr (x) (set x (+1 x)))
(defmacro incr (x xs) (prog (incr x) (incr xs)))

(defmacro decr (x) (set x (-1 x)))
(defmacro decr (x xs)
  (prog (decr x) (decr xs)))

(defun compare (x y)
  (if (< x y) -1 (if (> x y) 1 0)))
