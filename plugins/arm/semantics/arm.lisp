(declare  (context (target arm-family)))
(defpackage llvm-armv7 (:use arm))
(in-package arm)

(require bits)

(defun CLZ (rd rn pre _)
  (when (condition-holds pre)
    (set$ rd (clz rn))))

(defun CLZWr (rd rn)
    (setw rd (clz rn)))

(defun CLZXr (rd rn)
    (set$ rd (clz rn)))