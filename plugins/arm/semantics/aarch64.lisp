(require bits)

(declare (context (target armv8-a+le)))
(defpackage aarch64 (:use core target))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun word () (word-width))

(defun MOVZXi (dst imm pos)
  (set$ dst (lshift imm pos)))

(defun MOVZWi (dst imm pos)
  (set$ (base-reg dst) (lshift imm pos)))

(defun ADDXri (dst src imm _shift)
  (set$ dst (+ src imm)))

(defun LDRXui (dst reg off)
  (set$ dst (load-word (+ reg (lshift off 3)))))

(defun LDRSWui (dst base off)
  (set$ dst (cast-signed
             (word)
             (load-word (+ base (lshift off 2))))))

(defun LDRWui (dst reg off)
  (set$ (base-reg dst)
        (cast-unsigned (word) (load-hword (+ reg (lshift off 2))))))

(defun LDRBBui (dst reg off)
  (set$ (base-reg dst)
        (cast-unsigned (word) (load-hword (+ reg off)))))

(defmacro make-BFM (cast xd xr ir is)
  (let ((rs (word)))
    (if (< is ir)
        (if (and (/= is (- rs 1)) (= (+ is 1) ir))
            (set$ xd (lshift xr (- rs ir)))
            (set$ xd (lshift
                      (cast rs (extract is 0 xr))
                      (- rs ir))))
        (if (= is (- rs 1))
            (set$ xd (rshift xr ir))
            (set$ xd (cast rs (extract is ir xr)))))))

(defun UBFMXri (xd xr ir is)
  (make-BFM cast-unsigned xd xr ir is))

(defun SBFMXri (xd xr ir is)
  (make-BFM cast-signed xd xr ir is))

(defun ORRXrs (rd rn rm is)
  (set$ rd (logor rn (shifted rm is))))

(defun ORRWrs (rd rn rm is)
  (set$ (base-reg rd)
        (logor (base-reg rn)
               (shifted (base-reg rm) is))))

(defun ADRP (dst imm)
  (set$ dst (+ (get-program-counter)
               (cast-signed (word) (lshift imm 12)))))

(defun ADDWrs (dst r1 v s)
  (set$ dst (+ r1 (lshift v s))))

(defun ADDWri (dst r1 imm s)
  (ADDWrs dst r1 imm s))

(defun add-with-carry (rd x y c)
  (let ((r (+ c y x)))
    (set NF (msb r))
    (set VF (overflow r x y))
    (set ZF (is-zero r))
    (set CF (carry r x y))
    (set$ rd r)))

(defun SUBXrx64 (rd rn rm off)
  (set$ rd (- rn (shifted rm off))))

(defun SUBSXrs (rd rn rm off)
  (add-with-carry rd rn (lnot (shifted rm off)) 1))

(defun SUBSWrs (rd rn rm off)
  (add-with-carry
   (base-reg rd)
   (base-reg rn) (lnot (shifted (base-reg rm) off)) 1))

(defun SUBSWri (rd rn imm off)
  (add-with-carry (base-reg rd) (base-reg rn) (lnot (shifted imm off)) 1))

(defun SUBSXri (rd rn imm off)
  (add-with-carry rd rn (lnot (shifted imm off)) 1))

(defun SUBXrs (rd rn rm off)
  (set$ rd (- rn (shifted rm off))))

(defun SUBXri (rd rn imm off)
  (set$ rd (- rn (shifted imm off))))

(defun ADDXrs (rd rn rm off)
  (set$ rd (+ rn (shifted rm off))))

(defun shifted (rm off)
  (declare (visibility :private))
  (let ((typ (extract 7 6 off))
        (off (extract 5 0 off)))
    (case typ
      0b00 (lshift rm off)
      0b01 (rshift rm off)
      0b10 (arshift rm off))))

(defun STPXpre (dst t1 t2 _ off)
  (let ((off (lshift off 3)))
    (store-word (+ dst off) t1)
    (store-word (+ dst off (sizeof word)) t2)
    (set$ dst (+ dst off))))

(defun LDPXpost (dst r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word base))
    (set$ r2 (load-word (+ base (sizeof word))))
    (set$ dst (+ dst off))))

(defun LDPXi (r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word (+ base off)))
    (set$ r2 (load-word (+ base off (sizeof word))))))

(defun STRXui (src reg off)
  (let ((off (lshift off 3)))
    (store-word (+ reg off) src)))

(defun STRXroX (rt rn rm _ shift)
  (store-word (+ rn (lshift rm (* shift 3))) rt))

(defun LDRXroX (rt rn rm _ shift)
  (set$ rt (load-word (+ rn (lshift rm (* shift 3))))))

(defun STRWui (src reg off)
  (let ((off (lshift off 2)))
    (store-word (+ reg off) src)))

(defun STRBBui (src reg off)
  (store-byte (+ reg off) (base-reg src)))

(defun relative-jump (off)
  (exec-addr (+ (get-program-counter) (lshift off 2))))

(defun BL (off)
  (set LR (+ (get-program-counter) 4))
  (relative-jump off))

(defun BR (reg)
  (exec-addr reg))

(defun BLR (reg)
  (set LR (+ (get-program-counter) 4))
  (exec-addr reg))

(defun B (off)
  (relative-jump off))


(defun RET (dst)
  (exec-addr dst))

(defun CBZX (reg off)
  (when (is-zero reg)
    (relative-jump off)))

(defun CBZW (reg off)
  (when (is-zero (base-reg reg))
    (relative-jump off)))

(defun CBNZX (reg off)
  (when (/= reg 0)
    (relative-jump off)))

(defun CBNZW (reg off)
  (when (/= (base-reg reg) 0)
    (relative-jump off)))

(defun Bcc (cnd off)
  (when (condition-holds cnd)
    (relative-jump off)))

(defun condition-holds (cnd)
  (case cnd
    0b000 ZF
    0b001 CF
    0b010 NF
    0b011 VF
    0b100 (logand CF (lnot ZF))
    0b101 (= NF VF)
    0b110 (logand (= NF VF) (= ZF 0))
    true))

(defun base-reg (reg)
  (case (symbol reg)
    'W0  X0
    'W1  X1
    'W2  X2
    'W3  X3
    'W4  X4
    'W5  X5
    'W6  X6
    'W7  X7
    'W8  X8
    'W9  X9
    'W10 X10
    'W11 X11
    'W12 X12
    'W13 X13
    'W14 X14
    'W15 X15
    'W16 X16
    'W17 X17
    'W18 X18
    'W19 X19
    'W20 X20
    'W21 X21
    'W22 X22
    'W23 X23
    'W24 X24
    'W25 X25
    'W26 X26
    'W27 X27
    'W28 X28
    'W29 FP
    'W30 LR
    'WSP SP
    'WZR XZR
    (msg "unknown register $0" reg)))
