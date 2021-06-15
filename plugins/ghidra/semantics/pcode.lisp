(require bits)

(defpackage ghidra (:use core target))
(defpackage ghidra-x86-unknown (:use ghidra))

(in-package ghidra)

(defun COPY (dst src)
  (set$ dst src))
