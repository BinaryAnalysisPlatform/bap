(declare (context (target arm armv8-a+le)))

(defpackage aarch64 (:use core target arm))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

;; helper functions
(require bits)
(require arm-bits)
(require aarch64-helper)

;; instructions are sorted by the categories defined here
;; https://github.com/UQ-PAC/bap/wiki/All-aarch64-Instructions-by-Category
(require aarch64-arithmetic)
(require aarch64-atomic)
(require aarch64-branch)
(require aarch64-logical)
(require aarch64-data-movement)
(require aarch64-special)
