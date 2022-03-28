(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; SPECIFIC PROCESSOR STATE INSTRUCTIONS

(defmacro CCMN** (rn rm-or-imm nzcv cnd)
  "(CCMN** rn rm-or-imm nzcv cnd) implements either CCMN*i
   or CCMN*r. The semantics are the same at the lisp level;
   an immediate or register value is given as second argument."
  (if (condition-holds cnd)
    (set-flags (+ rn rm-or-imm) rn rm-or-imm)
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
      (set-flags (+ rn rm-or-imm 1) rn rm-or-imm))
    (set-nzcv nzcv)))

(defun CCMPWi (rn imm nzcv cnd) (CCMP** rn imm nzcv cnd))
(defun CCMPXi (rn imm nzcv cnd) (CCMP** rn imm nzcv cnd))
(defun CCMPWr (rn rm  nzcv cnd) (CCMP** rn rm  nzcv cnd))
(defun CCMPXr (rn rm  nzcv cnd) (CCMP** rn rm  nzcv cnd))

;; CFINV gets turned into MSR (register) by LLVM even though they're not aliases.
;; --show-knowledge shows that LLVM returned "MSR 0x200 XZR" for CFINV, but
;; this isn't even a valid opcode? For MSR (register) both arguments need to be registers..

;; (defun CFINV () (set CF (lnot CF)))
;; (defun MSR () )

;; LLVM can't seem to disassemble the RMIF instruction at all;
;; try `bap-mc --show-bil --arch=aarch64 -- "00 04 00 ba"`
;; which corresponds to "RMIF x0, 0, 0".
;; I also don't know whether "RMIF" is the correct LLVM opcode to
;; use as the function name.
;; It also can't do SETF8 and SETF16, and all three are from ARMv8.4.
;; I suspect ARMv8.4 support has not been implemented yet.
(defun RMIF (rn imm nzcv-mask)
  (let ((tmp (extract 3 0 (rotate-right rn imm))))
    (when (= 1 (select 3 nzcv-mask))
      (set NF (select 3 tmp)))
    (when (= 1 (select 3 nzcv-mask))
      (set NF (select 3 tmp)))
    (when (= 1 (select 3 nzcv-mask))
      (set NF (select 3 tmp)))
    (when (= 1 (select 3 nzcv-mask))
      (set NF (select 3 tmp)))))

;; LLVM can't disassemble SETF8 and SETF16 instructions (same as RMIF instruction).
;; See comment in RMIF function. Again, just guessing the LLVM opcode here.
(defun SETF8W (rn)
  (set NF (select 7 rn))
  (set ZF (is_zero (extract 7 0 rn)))
  (set VF (logxor (select (+ 7 1) rn) (select 7 rn))))

(defun SETF16W (rn)
  (set NF (select 15 rn))
  (set ZF (is_zero (extract 15 0 rn)))
  (set VF (logxor (select (+ 15 1) rn) (select 15 rn))))
