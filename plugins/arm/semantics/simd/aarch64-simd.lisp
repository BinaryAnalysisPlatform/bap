(declare (context (target arm armv8-a+le)))

(defpackage aarch64-simd (:use aarch64))

(in-package aarch64-simd)

(require aarch64-simd-arithmetic)
(require aarch64-simd-load)
(require aarch64-simd-logical)
(require aarch64-simd-mov-ins-ext)
(require aarch64-simd-store)