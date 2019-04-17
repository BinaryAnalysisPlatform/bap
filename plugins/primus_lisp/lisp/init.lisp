
(defconstant true 1:1  "true is another name for 1:1")
(defconstant false 0:1 "false is another name for 0:1")
(defconstant nil false "nil is another name for false")

(defmacro when (cnd body)
  "(when CND BODY) if CND is true then evaluates BODY and returns the
   value of last expression in BODY, otherwise returns false."
  (if cnd (prog body) ()))

(defmacro until (c b)
  "(unit COND BODY) if CND is not true then evaluates BODY and returns
   the value of the last expression in BODY, otherwise returns false. "
  (while (not c) b))

(defun non-zero (x)
  "(non-zero X) is true if X is not zero"
  (not (is-zero x)))

(defmacro += (x y)
  "(+= X Y) assigns a sum of X and Y to variable X"
  (set x (+ x y)))

(defun -1 (x)
  "(-1 X) returns the predecessor of X"
  (- x 1))

(defun +1 (x)
  "(+1 x) returns the successor of X"
  (+ x 1))

(defmacro incr (x)
  "(incr X Y ...) increments the value bound with the variables X, Y, ..."
  (set x (+1 x)))

(defmacro incr (x xs)
  (prog (incr x) (incr xs)))

(defmacro decr (x)
  "(decr X Y ...) decrements the value bound with the variables X, Y, ..."
  (set x (-1 x)))
(defmacro decr (x xs)
  (prog (decr x) (decr xs)))

(defmacro and (x)
  "(and X Y ...) evaluates expressions X,Y,... until the first
  expression that returns false. The value of the whole form is the
  value of the last evaluated expression." x)
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

(defmacro sign (x)
  "returns 1 if X > 0, 0 if X = 0, and -1 if X < 0"
  (if (< x 0) -1 (if (> x 0) 1 0)))

(defmacro compare (x y)
  "(compare X Y) returns 0 if X = Y, a negative value if X<Y
   and a positive value if X>Y"
  (sign (- x y)))

(defmacro assert (c)
  "(assert COND) terminates program if COND is false"
  (when (not c)
    (msg "Assertion (assert $0) failed" c)
    (error "Assert_failure")))

(defmacro is-in (x y)
  "(is-in X A B C ...) returns true if X is equal A or B or C or ..."
  (= x y))
(defmacro is-in (x y ys)
  (or (is-in x y) (is-in x ys)))

(defmacro fold (f a x)
  "(fold F A X Y ...) expands to (F (F A X) Y) ..."
  (f a x))
(defmacro fold (f a x xs) (fold f (f a x) xs))

(defmacro min (x)
  "(min X Y ...) returns the lower bound of the (X,Y,...) set"
      x)
(defmacro min (p q)    (let ((x p) (y q)) (if (< x y) x y)))
(defmacro min (x y ys) (min (min x y) ys))
(defmacro max (x)
  "(max X Y ...) returns the upper bound of the (X,Y,...) set"
  x)
(defmacro max (p q)    (let ((x p) (y q)) (if (> x y) x y)))
(defmacro max (x y ys) (max (max x y) ys))
