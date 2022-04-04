(defpackage intrinsic (:use core))

(in-package intrinsic)

(declare (global x0 x1 x2 x3
                 y0 y1 y2 y3))

(defun fadd ()
  (declare (external "intrinsic:fadd_rne_ieee754_binary"))
  (set y0 (ieee754-add 64 x0 x1)))

(defun fdiv ()
  (declare (external "intrinsic:fdiv_rne_ieee754_binary"))
  (set y0 (ieee754-div 64 x0 x1)))

(defun is-nan ()
  (declare (external "intrinsic:is_nan_ieee754_binary"))
  (set y0 (cast word-width (ieee754-is-nan 64 x0))))

(defun forder ()
  (declare (external "intrinsic:forder_ieee754_binary"))
  (set y0 (cast word-width (ieee754-lt 64 x0 x1))))
