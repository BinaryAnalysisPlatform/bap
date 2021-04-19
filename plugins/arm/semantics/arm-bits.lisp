(defpackage arm (:use core target))
(declare  (context (target armv4+le)))

(in-package arm)

(defun add-with-carry (rd x y c)
  (let ((r (+ c y x)))
    (set NF (msb r))
    (set VF (overflow r x y))
    (set ZF (is-zero r))
    (set CF (carry r x y))
    (set$ rd r)))

(defun logandnot (rd rn)
  (logand rd (lnot rn)))

(defmacro shift-with-carry (shift rd rn rm)
  (let ((r (cast-signed (word-width) rn)))
    (set CF (msb r))
    (set$ rd (shift r rm))
    (set ZF (is-zero rd))
    (set NF (msb rd))))
