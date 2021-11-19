(require bits)
(require arm-bits)


;; Note: page references are from ARM DDI 0403E.b

(declare (context (target arm)))

(defpackage thumb (:use core target arm))
(defpackage llvm-thumb (:use thumb))

(in-package thumb)

(defmacro tLOGs (op rd rn rm cnd)
  (prog (set$ rd (op rn rm))
     (when (is-unconditional cnd)
       (set ZF (is-zero rd))
       (set NF (msb rd)))))

(defun tEOR (rd _ rn rm cnd _)
  (tLOGs logxor rd rn rm cnd))

(defun tAND (rd _ rn rm cnd _)
  (tLOGs logand rd rn rm cnd))

(defun tBIC (rd _ rn rm cnd _)
  "bics rd, rn, rm ; with rn = rd"
  (tLOGs logandnot rd rn rm cnd))

(defun tMVN (rd _ rn cnd _)
  (set$ rd (lnot rn))
  (when (is-unconditional cnd)
    (set ZF (is-zero rd))
    (set NF (msb rd))))

(defun tREV (rd rn cnd _)
  (when (condition-holds cnd)
    (set$ rd (concat
              (extract 7 0 rn)
              (extract 15 8 rn)
              (extract 23 16 rn)
              (extract 31 24 rn)))))

(defun tLSLrr (rd _ rn rm cnd _)
  "lsls rd, rn, rm"
  (shift-with-carry lshift rd rn rm cnd))

(defun tLSRrr (rd _ rn rm cnd _)
  "lsrs rd, rn, rm"
  (shift-with-carry rshift rd rn rm cnd))

(defun tTST (rn rm _ _)
  "tst rn, rm"
  (let ((rd (logand rn rm)))
    (set ZF (is-zero rd))
    (set NF (msb rd))))

(defun tADDhirr (rd rn rm cnd _)
  (when (condition-holds cnd)
    (set$ rd (+ rn (t2reg rm)))))

(defun tSBC (rd _ rn rm cnd _)
  (add-with-carry/it-block rd rn (lnot rm) CF cnd))

(defun tRSB (rd _ rn cnd _)
  "rsbs	r3, r2, #0"
  (add-with-carry/it-block rd 0 (lnot rn) 1 cnd))

(defun tMUL (rd _ rn rm cnd _)
  (when (condition-holds cnd)
    (set$ rd (* rn rm))
    (when (is-unconditional cnd)
      (set ZF (is-zero rd))
      (set NF (msb rd)))))

(defun t2STRDi8 (rt1 rt2 rn imm pre _)
  "strd rt1, rt2, [rn, off]"
  (when (condition-holds pre)
    (store-word (+ rn imm) rt1)
    (store-word (+ rn imm (sizeof word-width)) rt2)))

(defun t2ADDri12 (rd rn imm pre _)
  "addw rd, rn, imm; A7-189, T4 "
  (when (condition-holds pre)
    (set$ rd (+ rn imm))))

(defun t2STRHi12 (rt rn imm pre _)
  "strh.w rt, [rn, imm]; A7-442; T2"
  (when (condition-holds pre)
    (store-word (+ rn imm) (cast-low 16 rt))))

(defun t2B (off pre _)
  "b.w imm; A7-207, T3"
  (when (condition-holds pre)
    (exec-addr (+ (t2pc) off))))

(defun t2LDRs (rt rn rm imm pre _)
  (when (condition-holds pre)
    (t2set rt (load-word (+ rn (lshift rm imm))))))

(defun t2LDRSBi12 (rt rn imm pre _)
  "ldrsb.w rt, [rn, imm]"
  (when (condition-holds pre)
    (t2set rt (cast-signed 32 (load-bits 8 (+ rn imm))))))

(defun t2LDRSHi12 (rt rn imm pre _)
  "ldrsh.w rt, [rn, imm]"
  (when (condition-holds pre)
    (t2set rt (cast-signed 32 (load-bits 16 (+ rn imm))))))

(defun t2LDRHi12 (rt rn imm pre _)
  "ldrh.w rt, [rn, imm]"
  (when (condition-holds pre)
    (t2set rt (cast-unsigned 32 (load-bits 16 (+ rn imm))))))

(defun tMOVr (rt rn pre _)
  (when (condition-holds pre)
    (t2set rt rn)))

(defun t2pc () (+ (get-program-counter) 4))

(defun is-pc (r) (= (symbol r) 'PC))

(defun t2reg (r)
  (if (is-pc r) (t2pc) r))

(defun t2set (reg val)
  (if (is-pc reg)
      (exec-addr val)
    (set$ reg val)))
