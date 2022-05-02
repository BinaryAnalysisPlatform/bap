(defpackage intrinsic (:use core target))

(in-package intrinsic)

(declare (global x0 x1 x2 y0))

(defun fadd_rne_ieee754_binary ()
  (set y0 (+. :rne x0 x1)))

(defun fsub_rne_ieee754_binary ()
  (set y0 (-. :rne x0 x1)))

(defun fmul_rne_ieee754_binary ()
  (set y0 (*. :rne x0 x1)))

(defun fdiv_rne_ieee754_binary ()
  (set y0 (/. :rne x0 x1)))

(defun forder_ieee754_binary ()
  (set y0 (<. x0 x1)))

(defun is_nan_ieee754_binary ()
  (set y0 (is-nan x0)))

(defun cast_sfloat_rne_binary ()
  (set y0 (cast-sfloat :rne x0 x1)))
