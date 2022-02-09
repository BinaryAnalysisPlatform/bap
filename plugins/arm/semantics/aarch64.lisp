(declare (context (target arm armv8-a+le)))

;; helper functions
(require bits)
(require arm-bits)

;; implementations
(require arithmetic)
(require atomic)
(require branch)
(require logical)
(require register-movements)
(require special)


(defpackage aarch64 (:use core target arm))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun word () (word-width))

;; instructions are sorted by the categories defined here
;; https://github.com/UQ-PAC/bap/wiki/All-aarch64-Instructions-by-Category

