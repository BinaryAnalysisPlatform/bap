(declare (context (target x86)))

(defpackage x86-common (:use core target))
(defpackage x86-32 (:use x86-common))
(defpackage x86-64 (:use x86-common))
(defpackage llvm-x86 (:use x86-32))
(defpackage llvm-x86_64 (:use x86-64))

(in-package x86-common)

(defun HLT ()
  (intrinsic 'hlt))

(defun NOOP ()
  (empty))

(defun NOOPL (_ _ _ _ _)
  (empty))

(defun NOOPW (_ _ _ _ _)
  (empty))

;; xchgb reg, off(base)
;; Reference: Vol. 2D 6-32

(defun XCHG8rm (_ reg base _ _ off _)
  (let ((p (+ base off))
        (x (load-byte p)))
    (store-byte p reg)
    (set$ reg x)))

(defun XCHG8rr (dst src _ _)
  (let ((x dst))
    (set$ dst src)
    (set$ src x)))
