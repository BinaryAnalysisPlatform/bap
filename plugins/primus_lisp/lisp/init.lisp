(defmacro when (cnd body)
  "if CND is 1:0 then evalute BODY
   and return the value of last expression in BODY,
   otherwise return 0:0"
  (if cnd (prog body) ()))

(defmacro until ($cond$ $body$)
  (while (not $cond) $body))


(defun > (x y) (< y x))
(defun /= (x y) (not (= x y)))
(defun +1 (x) (+ x 1))
(defun -1 (x) (- x 1))

(defun non-zero (x) (non (is-zero x)))
(defmacro += (x y) (set x (+ x y)))


(defmacro incr (x) (set x (+1 x)))
(defmacro incr (x xs)
  (prog (incr x) (incr xs)))

(defmacro decr (x) (set x (-1 x)))
(defmacro decr (x xs)
  (prog (decr x) (decr xs)))

(defmacro and (x) x)
(defmacro and (x xs) (if x (and xs) x))

(defmacro or (x) x)
(defmacro or (x xs) (if x x (or xs)))

(defun compare (x y)
  (if (< x y) -1 (if (> x y) 1 0)))


(defmacro assert (c m)
  (when (not c)
    (msg m)
    (fail m)))

(defmacro expand-clause (c xs)
  (when c xs))

(defmacro cond (x xs)
  (expand-clause s)
  (cond xs))
