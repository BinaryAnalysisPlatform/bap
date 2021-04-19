(require bits)
(require arm-bits)

(declare (context (target armv4+le)))

(defpackage thumb (:use core target arm))
(defpackage llvm-thumbv7 (:use thumb))

(in-package thumb)

(defmacro tLOGs (op rd rn rm)
  (prog (set$ rd (op rn rm))
     (set ZF (is-zero rd))
     (set NF (msb rd))))

(defun tEOR (rd _ rn rm _ _)
  (tLOGs logxor rd rn rm))

(defun tAND (rd _ rn rm _ _)
  (tLOGs logand rd rn rm))

(defun tBIC (rd _ rn rm _ _)
  "bics rd, rn, rm ; with rn = rd"
  (tLOGs logandnot rd rn rm))

(defun tMVN (rd _ rn _ _)
  (set$ rd (lnot rn))
  (set ZF (is-zero rd))
  (set NF (msb rd)))

(defun tREV (rd rn _ _)
  (set$ rd (concat
            (extract 7 0 rn)
            (extract 15 8 rn)
            (extract 23 16 rn)
            (extract 31 24 rn))))

(defun tLSLrr (rd _ rn rm _ _)
  "lsls rd, rn, rm"
  (shift-with-carry lshift rd rn rm))

(defun tLSRrr (rd _ rn rm _ _)
  "lsrs rd, rn, rm"
  (shift-with-carry rshift rd rn rm))

(defun tTST (rn rm _ _)
  "tst rn, rm"
  (let ((rd (logand rn rm)))
    (set ZF (is-zero rd))
    (set NF (msb rd))))

(defun tADDhirr (rd rn rm _ _)
  (set$ rd (+ rn rm)))

(defun tSBC (rd _ rn rm _ _)
  (add-with-carry rd rn (- rm) CF))

(defun tRSB (rd _ rn _ _)
  "rsbs	r3, r2, #0"
  (add-with-carry rd 0 (lnot rn) 1))

(defun tMUL (rd _ rn rm _ _)
  (set$ rd (* rn rm))
  (set ZF (is-zero rd))
  (set NF (msb rd)))
