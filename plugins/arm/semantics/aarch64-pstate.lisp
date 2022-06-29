(declare (context (target arm-family)
                  (bits 64)))

(in-package aarch64)

;;; SPECIFIC PROCESSOR STATE INSTRUCTIONS

(defmacro CCMN** (rn rm-or-imm nzcv cnd)
  "(CCMN** rn rm-or-imm nzcv cnd) implements either CCMN*i
   or CCMN*r. The semantics are the same at the lisp level;
   an immediate or register value is given as second argument."
  (if (condition-holds cnd)
    (set-nzcv-from-registers (+ rn rm-or-imm) rn rm-or-imm)
    (set-nzcv nzcv)))

(defun CCMNWi (rn imm nzcv cnd) (CCMN** rn imm nzcv cnd))
(defun CCMNXi (rn imm nzcv cnd) (CCMN** rn imm nzcv cnd))
(defun CCMNWr (rn rm  nzcv cnd) (CCMN** rn rm  nzcv cnd))
(defun CCMNXr (rn rm  nzcv cnd) (CCMN** rn rm  nzcv cnd))

(defmacro CCMP** (rn rm-or-imm nzcv cnd)
  "(CCMP** rn rm-or-imm nzcv cnd) implements either CCMP*i
   or CCMP*r. The semantics are the same at the lisp level;
   an immediate or register value is given as second argument."
  (if (condition-holds cnd)
    (let ((rm-or-imm (lnot rm-or-imm)))
      (set-nzcv-from-registers (+ rn rm-or-imm 1) rn rm-or-imm))
    (set-nzcv nzcv)))

(defun CCMPWi (rn imm nzcv cnd) (CCMP** rn imm nzcv cnd))
(defun CCMPXi (rn imm nzcv cnd) (CCMP** rn imm nzcv cnd))
(defun CCMPWr (rn rm  nzcv cnd) (CCMP** rn rm  nzcv cnd))
(defun CCMPXr (rn rm  nzcv cnd) (CCMP** rn rm  nzcv cnd))

(defun CFINV () (set CF (lnot CF)))
;; (defun MSR () )

(defun RMIF (rn imm nzcv-mask)
  (let ((tmp (extract 3 0 (rotate-right rn imm))))
    (when (= 1 (select 3 nzcv-mask))
      (set NF (select 3 tmp)))
    (when (= 1 (select 2 nzcv-mask))
      (set ZF (select 2 tmp)))
    (when (= 1 (select 1 nzcv-mask))
      (set CF (select 1 tmp)))
    (when (= 1 (select 0 nzcv-mask))
      (set VF (select 0 tmp)))))

(defun SETF8 (rn)
  (set NF (select 7 rn))
  (set ZF (is-zero (extract 7 0 rn)))
  (set VF (logxor (select (+ 7 1) rn) (select 7 rn))))

(defun SETF16 (rn)
  (set NF (select 15 rn))
  (set ZF (is-zero (extract 15 0 rn)))
  (set VF (logxor (select (+ 15 1) rn) (select 15 rn))))
