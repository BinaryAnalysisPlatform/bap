(defconstant true 1:1)
(defconstant false 0:1)
(defconstant nil false)

(defmacro when (cnd body)
  "if CND is true then evalute BODY
   and return the value of last expression
   in BODY, otherwise return false"
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
(defmacro and (x xs)
  (let ((r x)) (if r (and xs) r)))

(defmacro or (x xs)
  "(or <expr> ...) evaluates a sequence of expressions EXPR from left
   to right until it meets the first expression that evaluates to the
   truth value, that will become the value of the whole form. If no
   expression returned the truth value, then the result of the whole
   form is 0:1"
  (let ((r x)) (if r r (or xs))))

(defmacro or (x) x)

(defun compare (x y)
  (if (< x y) -1 (if (> x y) 1 0)))


(defmacro assert (c m)
  (when (not c)
    (msg m)
    (error m)))
