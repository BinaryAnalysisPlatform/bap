(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; LOGICAL

(defun ANDv8i8  (vd vn vm) (set$ vd (logand vn vm)))
(defun ANDv16i8 (vd vn vm) (set$ vd (logand vn vm)))

;; the ISA expresses (logxor vn vm) as
;; (logxor vm (logand (logor (zeros (word-width vn)) vn) (ones (word-width vn))))
;; I've simplified it to just this.
(defun EORv8i8  (vd vn vm) (set$ vd (logxor vn vm)))
(defun EORv16i8 (vd vn vm) (set$ vd (logxor vn vm)))

;; the ISA says NOT acts element-wise, but this is
;; equivalent to just (lnot vn). Not sure why it does this.
(defun NOTv8i8  (vd vn)    (set$ vd (lnot vn)))
(defun NOTv16i8 (vd vn)    (set$ vd (lnot vn)))

(defun ORRv8i8  (vd vn vm) (set$ vd (logor  vn vm)))
(defun ORRv16i8 (vd vn vm) (set$ vd (logor  vn vm)))

(defun ORNv8i8  (vd vn vm) (set$ vd (logor  vn (lnot vm))))
(defun ORNv16i8 (vd vn vm) (set$ vd (logor  vn (lnot vm))))
