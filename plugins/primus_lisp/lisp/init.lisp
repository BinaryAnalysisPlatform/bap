(defconstant true 1:1)
(defconstant false 0:1)
(defconstant nil false)

(defmacro when (cnd body)
  "if CND is true then evalute BODY
   and return the value of last expression in BODY,
   otherwise return false"
  (if cnd (prog body) ()))

(defmacro until (c b)
  (while (not c) b))

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
