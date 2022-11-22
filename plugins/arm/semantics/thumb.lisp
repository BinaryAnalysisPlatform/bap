(require bits)
(require arm-bits)


;; Note: page references are from ARM DDI 0403E.b

(declare (context (target arm-family)))

(defpackage thumb (:use core target arm))
(defpackage llvm-thumb (:use thumb))

(in-package thumb)

(defun tCMPhir (rn rm cnd _)
  "cmp rn, rn"
  (when (condition-holds cnd)
    (let ((r (- rn rm)))
      (set-nzcv-from-registers r rn rm))))

(defun tADR (rd lbl cnd _)
  "adr rd, lbl"
  (when (condition-holds cnd)
    (let ((pc (* 4 (/ (t2pc) 4))))
      (set$ rd (+ pc (lshift lbl 2))))))

(defmacro tLOGs (op rd rn rm cnd)
  (prog (set$ rd (op rn rm))
     (when (is-unconditional cnd)
       (set ZF (is-zero rd))
       (set NF (msb rd)))))

(defun tEOR (rd _ rn rm cnd _)
  (tLOGs logxor rd rn rm cnd))

(defun t2EORrs (rd rn rm simm cnd _ _)
  "eor.w rd, rn, rm, simm"
  (when (condition-holds cnd)
    (set$ rd (logxor rn (i-shift rm simm)))))

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

(defun tREV16 (rd rm cnd _)
  "rev16 rd rm"
  (when (condition-holds cnd)
    (set$ rd (concat
              (extract 23 16 rm)
              (extract 31 24 rm)
              (extract 7 0 rm)
              (extract 15 8 rm)))))

(defun tLSLrr (rd _ rn rm cnd _)
  "lsls rd, rn, rm"
  (shift-with-carry lshift rd rn rm cnd))

(defun t2LSLri (rd rm imm cnd _ _)
  "lsl.w rd, rm, #imm"
  (when (condition-holds cnd)
    (set$ rd (lshift rm imm))))

(defun tLSRrr (rd _ rn rm cnd _)
  "lsrs rd, rn, rm"
  (shift-with-carry rshift rd rn rm cnd))

(defun t2LSRri (rd rm imm cnd _ _)
  "lsr.w rd, rm, #imm"
  (when (condition-holds cnd)
    (set$ rd (rshift rm imm))))

(defun tTST (rn rm _ _)
  "tst rn, rm"
  (let ((rd (logand rn rm)))
    (set ZF (is-zero rd))
    (set NF (msb rd))))

(defun tADDhirr (rd rn rm cnd _)
  (when (condition-holds cnd)
    (set$ rd (+ rn (t2reg rm)))))

(defun t2ADDrs (rd rn rm simm cnd _ _)
  "add.w rd, rn, rm, simm"
  (when (condition-holds cnd)
    (set$ rd (+ rn (i-shift rn simm)))))

(defun tSBC (rd _ rn rm cnd _)
  (add-with-carry/it-block rd rn (lnot rm) CF cnd))

(defun tRSB (rd _ rn cnd _)
  "rsbs	r3, r2, #0"
  (add-with-carry/it-block rd 0 (lnot rn) 1 cnd))

(defun t2RSBrs (rd rn rm simm cnd _ _)
  "rsb rd, rn, rm, simm"
  (when (condition-holds cnd)
    (set$ rd (- (i-shift rm simm) rn))))

(defun tMUL (rd _ rn rm cnd _)
  (when (condition-holds cnd)
    (set$ rd (* rn rm))
    (when (is-unconditional cnd)
      (set ZF (is-zero rd))
      (set NF (msb rd)))))

(defun t2STR_PRE (_ rt rn off cnd _)
  "str rt [rn, #off]!"
  (when (condition-holds cnd)
    (set$ rn (+ rn off))
    (store-word rn rt)))

(defun t2STRDi8 (rt1 rt2 rn imm pre _)
  "strd rt1, rt2, [rn, off]"
  (when (condition-holds pre)
    (store-word (+ rn imm) rt1)
    (store-word (+ rn imm (sizeof word-width)) rt2)))

(defun t2STRs (rt rn rm imm cnd _)
  "str.w rt [rn, rm, lsl imm]"
  (when (condition-holds cnd)
    (store-word (+ rn (lshift rm imm)) rt)))

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

(defun t2Bcc (off pre _)
  "bcc.w imm"
  (when (condition-holds pre)
    (exec-addr (+ (t2pc) off))))

(defun t2TBB (rn rm _ _)
  "tbb [rn, rm]"
  (let ((addr (+ (t2reg rn) (t2reg rm)))
        (halfwords (cast-unsigned 32 (load-bits 8 addr))))
    (exec-addr (+ (t2pc) (* halfwords 2)))))

(defun t2TBH (rn rm _ _)
  "tbh [rn, rm, lsl #1]"
  (let ((addr (+ (t2reg rn) (lshift rm 1)))
        (halfwords (cast-unsigned 32 (load-hword addr))))
    (exec-addr (+ (t2pc) (* halfwords 2)))))

(defun t2LDRs (rt rn rm imm pre _)
  (when (condition-holds pre)
    (t2set rt (load-word (+ rn (lshift rm imm))))))

(defun t2LDRi8 (rt rn imm cnd _)
  "ldr rt, [rn, #-imm]"
  (when (condition-holds cnd)
    (set$ rt (load-word (+ rn imm)))))

(defun t2LDRDi8 (rt rt2 rn imm cnd _)
  "ldrd rt, rt2, [rn, #imm]"
  (when (condition-holds cnd)
    (set$ rt (load-word (+ rn imm)))
    (set$ rt2 (load-word (+ rn imm 4)))))

(defun t2LDR_POST (rt _ rn off cnd _)
  "ldr rt, [rn], #imm"
  (when (condition-holds cnd)
    (let ((tmp rn))
      (set$ rn (+ rn off))
      (t2set rt (load-word tmp)))))

(defun t2LDRB_PRE (rt _ rn off cnd _)
  "ldrb rt, [rn, #off]!"
  (when (condition-holds cnd)
    (set$ rn (+ rn off))
    (set$ rt (cast-unsigned 32 (load-bits 8 rn)))))

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
