(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; INS
;; with gp register

;; (defmacro INSvi*gpr () ())

(defun INSvi32gpr (vd redundant index gpr)
  "(INSvi32gpr vd ts index gpr) inserts an element in the general purpose register gpr into vecter register vd at index. NOTE: does not encode Security state & Exception level"
  (let ((element (extract 32 0 gpr))
	(highIndex (* 32 (+ index 1)))
	(lowIndex (- (* 32 index) 1))
	(topPart (rshift vd highIndex))
	(mask (replicate-to-fill (extract 0 0 0x1) lowIndex))
	(bottomPart (logand vd mask)))
    (msg "width top: $0" (alias-base-register vd))
    (set-symbol-value vd (extract 127 0 (concat topPart element bottomPart)))))

(defun INSvi32lane (vd redundant index1 vn index2)
  "NOTE: does not encode Security state & Exception level"
  (let ((selem (get-vector-element index2 vn))
	(highIndex (* 32 (+ index1 1)))
	(lowIndex (- (* 32 index1) 1))
	(topPart (rshift vd highIndex))
	(mask (replicate-to-fill (extract 0 0 0x1) lowIndex))
	(bottomPart (logand vd mask)))
    (set-symbol-value vd (extract 127 0 (concat topPart selem bottomPart)))))

;; load (multiple structures)

;; opcde = 1000, rpt = 1, selem = 2
;; L = 1, MEMOP_LOAD
;; T = 16B, imm = #32, Q = 1, size = 00
;; datasize = 128, esize = 8
;; elements = 16
;; pseudocode:

;; for r = 0 to r = 0
;; for e = 0 to e = 15
;; tt = (UInt(qa) + 0) MOD 32;
;; for s = 0 to s = 1
;; rval = V(tt)
;; Elem[rval, e, 8] = Mem[address+offs, 1, AccType_VEC]
;; V[tt] = rval
;; offs = offs + 1
;; tt = (tt + 1) MOD 32;
;; if xn given, offs = X[n]
;; set if address/X reg given
(defun LD2Twov16b_POST (redundant qa_qb xn imm) 
  "NOTE: does not encode Security state & Exception level"
  (msg "offset: $0" (alias-base-register imm)))
