(require bits)

(defpackage pcode (:use core target))
(in-package pcode)

(defmacro cast-word (x) (cast word-width x))

(defun set# (typ dst src)
  (if (is-symbol typ)
      (store-word (cast-word dst) src)
    (let ((dtyp (max typ (word-width dst)))
          (mask (coerce dtyp (- (lshift 1 typ) 1))))
      (set$ dst (logor
                 (logandnot dst mask)
                 (cast-unsigned (word-width dst) src))))))

(defmacro get# (typ src)
  (if (is-symbol typ) (load-word (cast-word src))
    (coerce typ src)))

(defmacro get# (op typ src)
  (op (get# typ src)))

(defmacro get# (op tx x ty y)
  (op (get# tx x) (get# ty y)))

(defun COPY (td d ts s)
  (set# td d (get# ts s)))

(defun STORE (_ _ _ ptr typ val)
  (store-word ptr (get# typ val)))

(defun LOAD (td dst _ _ _ ptr)
  (set# td dst (load-bits td ptr)))

(defun branch (typ dst)
  (if (is-symbol typ)
      (exec-addr dst)
    (goto-subinstruction dst)))

(defun BRANCH (typ dst)
  (branch typ dst))

(defun CBRANCH (typ dst tc cnd)
  (when (get# tc cnd) (branch typ dst)))

(defun BRANCHIND (_ dst)
  (exec-addr dst))

(defun CALL (typ dst)
  (exec-addr dst))

(defun CALLIND (typ dst)
  (exec-addr dst))

(defun RETURN (typ dst)
  (exec-addr dst))

(defun PIECE (tr r tx x ty y)
  (set# tr r (get# concat tx x ty y)))

(defun SUBPIECE (tr r tx x ts s)
  (set# tr r (rshift (get# tx x) (* 8 (get# ts s)))))

(defun INT_EQUAL (tr r tx x ty y)
  (set# tr r (get# = tx x ty y)))

(defun INT_NOTEQUAL (tr r tx x ty y)
  (set# tr r (get# /= tx x ty y)))

(defun INT_LESS (tr r tx x ty y)
  (set# tr r (get# < tx x ty y)))

(defun INT_SLESS (tr r tx x ty y)
  (set# tr r (get# s< tx x ty y)))

(defun INT_LESSEQUAL (tr r tx x ty y)
  (set# tr r (get# <= tx x ty y)))

(defun INT_SLESSEQUAL (tr r tx x ty y)
  (set# tr r (get# s<= tx x ty y)))

(defun INT_ZEXT (tr r tx x)
  (set# tr r (cast-unsigned tr (get# tx x))))

(defun INT_SEXT (tr r tx x)
  (set# tr r (cast-signed tr (get# tx x))))

(defun INT_ADD (tr r tx x ty y)
  (set# tr r (get# + tx x ty y)))

(defun INT_SUB (tr r tx x ty y)
  (set# tr r (get# - tx x ty y)))

(defun INT_CARRY (tr r tx x ty y)
  (let ((x (get# tx x))
        (y (get# ty y))
        (z (+ x y)))
    (set# tr r (coerce tr (carry z x y)))))

(defun INT_SCARRY (tr r tx x ty y)
  (let ((x (get# tx x))
        (y (get# ty y))
        (z (+ x y)))
    (set# tr r (coerce tr (overflow z x y)))))

(defun INT_SBORROW (tr r tx x ty y)
  (let ((x (get# tx x))
        (y (get# ty y))
        (z (- x y)))
    (set# tr r (coerce tr (overflow z x (- y))))))

(defun INT_2COMP (tr r tx x)
  (set# tr r (get# - tx x)))

(defun INT_NEGATE (tr r tx x)
  (set# tr r (get# lnot tx x)))

(defun INT_XOR (tr r tx x ty y)
  (set# tr r (get# logxor tx x ty y)))

(defun INT_AND (tr r tx x ty y)
  (set# tr r (get# logand tx x ty y)))

(defun INT_OR (tr r tx x ty y)
  (set# tr r (get# logor tx x ty y)))

(defun INT_LEFT (tr r tx x ty y)
  (set# tr r (get# lshift tx x ty y)))

(defun INT_RIGHT (tr r tx x ty y)
  (set# tr r (get# rshift tx x ty y)))

(defun INT_SRIGHT (tr r tx x ty y)
  (set# tr r (get# arshift tx x ty y)))

(defun INT_MULT (tr r tx x ty y)
  (set# tr r (get# * tx x ty y)))

(defun INT_DIV (tr r tx x ty y)
  (set# tr r (get# / tx x ty y)))

(defun INT_SDIV (tr r tx x ty y)
  (set# tr r (get# s/ tx x ty y)))

(defun INT_MOD (tr r tx x ty y)
  (set# tr r (get# mod tx x ty y)))

(defun INT_SMOD (tr r tx x ty y)
  (set# tr r (get# signed-mod tx x ty y)))

(defun BOOL_NEGATE (tr r tx x)
  (set# tr r (get# not tx x)))

(defun BOOL_AND (tr r tx x ty y)
  (set# tr r (get# logand tx x ty y)))

(defun BOOL_OR (tr r tx x ty y)
  (set# tr r (get# logor tx x ty y)))

(defun BOOL_XOR (tr r tx x ty y)
  (set# tr r (get# logxor tx x ty y)))

(defparameter fp-rmode 'rne
  "the floating-point operations rounding mode")

(defparameter fp-format 'ieee754_binary
  "the floating-point representation")


(defmacro fp-binary (name tr r tx x ty y)
  (set# tr r
        (intrinsic
         (symbol-concat name fp-rmode fp-format :sep '_)
         (get# tx x)
         (get# ty y)
         :result tr)))

(defmacro fp-unary (name tr r tx x)
  (set# tr r
        (intrinsic
         (symbol-concat name fp-format :sep '_)
         (get# tx x)
         :result tr)))

(defmacro fp-unary/rmode (name tr r tx x)
  (set# tr r
        (intrinsic
         (symbol-concat name fp-rmode fp-format :sep '_)
         (get# tx x)
         :result tr)))

(defmacro fp-predicate (name tr r tx x)
  (set$ r
        (intrinsic
         (symbol-concat 'is name fp-format :sep '_)
         (get# tx x)
         :result 8)))

(defun fp-order (tr tx x ty y)
  (intrinsic 'forder_ieee754_binary
             (get# tx x)
             (get# ty y)
             :result tr))

(defun fp-round (tr tx x)
  (intrinsic
   (symbol-concat 'fround fp-rmode fp-format :sep '_)
   (get# tx x)
   :result tr))

(defun INT2FLOAT (tr r tx x)
  (set# tr r
        (intrinsic
         (symbol-concat 'cast_sfloat fp-rmode fp-format :sep '_)
         (get# tx x)
         :result tr)))

(defun TRUNC (tr r tx x)
  (set# tr r
        (intrinsic
         (symbol-concat 'cast_sint 'rtz fp-format :sep '_)
         (get# tx x)
         :result tr)))

(defun FLOAT2FLOAT (tr r tx x)
  (set# tr r
        (intrinsic
         (symbol-concat 'fconvert fp-rmode fp-format)
         (get# tx x)
         :result tr)))

(defun FLOAT_ADD (tr r tx x ty y)
  (fp-binary 'fadd tr r tx x ty y))

(defun FLOAT_SUB (tr r tx x ty y)
  (fp-binary 'fsub tr r tx x ty y))

(defun FLOAT_DIV (tr r tx x ty y)
  (fp-binary 'fdiv tr r tx x ty y))

(defun FLOAT_MULT (tr r tx x ty y)
  (fp-binary 'fmul tr r tx x ty y))

(defun FLOAT_NEG (tr r tx x)
  (fp-unary 'fneg tr r tx x))

(defun FLOAT_ABS (tr r tx x)
  (fp-unary 'fabs tr r tx x))

(defun FLOAT_SQRT (tr r tx x)
  (fp-unary/rmode 'fsqrt tr r tx x))

(defun FLOAT_NAN (tr r tx x)
  (fp-predicate 'nan tr r tx x))

(defun FLOAT_EQUAL (tr r tx x ty y)
  (set$ r (not
           (fp-order tr tx x ty y)
           (fp-order tr ty y tx x))))

(defun FLOAT_NOTEQUAL (tr r tx x ty y)
  (set$ r (or (fp-order tr tx x ty y)
              (fp-order tr ty y tx x))))

(defun FLOAT_LESS (tr r tx x ty y)
  (set$ r (fp-order tr tx x ty y)))

(defun FLOAT_LESSEQUAL (tr r tx x ty y)
  (set$ r (not (fp-order tr ty y tx x))))

(defun FLOAT_ROUND (tr r tx x)
  (set# tr r (fp-round tr tx x)))

(defun FLOAT_CEIL (tr r tx x)
  (let ((fp-rmode 'rtp))
    (set# tr r (fp-round tr tx x))))

(defun FLOAT_FLOOR (tr r tx x)
  (let ((fp-rmode 'rtn))
    (set# tr r (fp-round tr tx x))))
