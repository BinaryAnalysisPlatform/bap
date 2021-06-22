(require bits)

(defpackage pcode (:use core target))
(in-package pcode)

(defun COPY (dst src)
  (set$ dst src))

(defun STORE (_ ptr val)
  (store-word ptr val))

(defun LOAD (dst _ ptr)
  (set$ dst (load-word ptr)))

(defun BRANCH (dst)
  (exec-addr dst))

(defun CBRANCH (dst cnd)
  (when cnd (exec-addr dst)))

(defun BRANCHIND (dst)
  (exec-addr dst))

(defun CALL (dst)
  (exec-addr dst))

(defun CALLIND (dst)
  (exec-addr dst))

(defun RETURN (dst)
  (exec-addr dst))

(defun PIECE (r x y)
  (set$ r (concat x y)))

(defun SUBPIECE (r x s)
  (set$ r (rshift x (* 8 s))))

(defun INT_EQUAL (r x y)
  (set$ r (= x y)))

(defun INT_NOTEQUAL (r x y)
  (set$ r (/= x y)))

(defun INT_LESS (r x y)
  (set$ r (< x y)))

;; (defun INT_SLESS (r x y)
;;   (set$ r (s< x y)))

(defun INT_LESSEQUAL (r x y)
  (set$ r (<= x y)))

;; (defun INT_SLESSEQUAL (r x y)
;;   (set$ r (s<= x y)))

(defun INT_ZEXT (r x)
  ;; TODO: fix the width
  (set$ r (cast-unsigned (word-width) x)))

(defun INT_SEXT (r x)
  ;; TODO: fix the width
  (set$ r (cast-signed (word-width) x)))

(defun INT_ADD (r x y)
  (set$ r (+ x y)))

(defun INT_SUB (r x y)
  (set$ r (- x y)))

(defun INT_CARRY (r x y)
  (let ((z (+ x y)))
    (set$ r (carry z x y))))

(defun INT_SCARRY (r x y)
  (let ((z (+ x y)))
    (set$ r (overflow z x y))))

(defun INT_SBORROW (r x y)
  (let ((z (- x y)))
    (set$ r (overflow z x (- y)))))

(defun INT_2COMP (r x)
  (set$ r (- x)))

(defun INT_NEGATE (r x)
  (set$ r (lnot x)))

(defun INT_XOR (r x y)
  (set$ r (logxor x y)))

(defun INT_AND (r x y)
  (set$ r (logand x y)))

(defun INT_OR (r x y)
  (set$ r (logor x y)))

(defun INT_LEFT (r x y)
  (set$ r (lshift x y)))

(defun INT_RIGHT (r x y)
  (set$ r (rshift x y)))

(defun INT_SRIGHT (r x y)
  (set$ r (arshift x y)))

(defun INT_MULT (r x y)
  (set$ r (* x y)))

(defun INT_DIV (r x y)
  (set$ r (/ x y)))

(defun INT_SDIV (r x y)
  (set$ r (s/ x y)))

(defun INT_MOD (r x y)
  (set$ r (mod x y)))

(defun INT_SMOD (r x y)
  (set$ r (signed-mod x y)))

(defun BOOL_AND (r x y)
  (set$ r (logand x y)))

(defun BOOL_OR (r x y)
  (set$ r (logor x y)))

(defun BOOL_XOR (r x y)
  (set$ r (logxor x y)))

(defun pcode-extra:CGOTO (dst cnd)
  (when cnd (goto-subinstruction dst)))

(defun pcode-extra:GOTO (dst)
  (goto-subinstruction dst))
