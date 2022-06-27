(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; INS

(defun INSvi32gpr (vd redundant index gpr)
  "(INSvi32gpr vd ts index gpr) inserts an element in the general purpose register gpr into vecter register vd at index. NOTE: does not encode Security state & Exception level"
	(insert-element-into-vector vd index gpr 32))

(defun INSvi32lane (vd redundant index vn index2)
  "NOTE: does not encode Security state & Exception level"
 	(let ((element (get-vector-S-element index2 vn)))
		(insert-element-into-vector vd index element 32)))

;;; LDs..
