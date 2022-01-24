(declare (context (target x86)))

(defpackage x86-common (:use core target))
(defpackage x86-32 (:use x86-common))
(defpackage x86-64 (:use x86-common))
(defpackage llvm-x86 (:use x86-32))
(defpackage llvm-x86_64 (:use x86-64))

(in-package x86-common)

(defun HLT ()
  (special :hlt))

(defun NOOP ()
  (empty))

(defun NOOPL (_ _ _ _ _)
  (empty))

(defun NOOPW (_ _ _ _ _)
  (empty))
