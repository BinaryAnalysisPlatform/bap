(require bits)
(require arm-bits)

(declare (context (target armv8-a+le)))
(defpackage aarch64 (:use core target arm))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun word () (word-width))

(defun MOVZXi (dst imm pos)
  (set$ dst (lshift imm pos)))

(defun MOVZWi (dst imm pos)
  (set$ (base-reg dst) (lshift imm pos)))

(defun ADDXri (dst src imm off)
  (set$ dst (+ src (lshift imm off))))

(defun LDRXui (dst reg off)
  (set$ dst (load-word (+ reg (lshift off 3)))))

(defun LDRSWui (dst base off)
  (set$ dst (cast-signed
             (word)
             (load-hword (+ base (lshift off 2))))))

(defun LDRWui (dst reg off)
  (set$ (base-reg dst)
        (cast-unsigned (word) (load-hword (+ reg (lshift off 2))))))

(defun LDRBBui (dst reg off)
  (set$ (base-reg dst)
        (cast-unsigned (word) (load-byte (+ reg off)))))

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
  (set$ dst (+
             (logand (get-program-counter) (lshift -1 12))
             (cast-signed (word) (lshift imm 12)))))

(defun ADDWrs (dst r1 v s)
  (set$ (base-reg dst) (+ (base-reg r1) (lshift (base-reg v) s))))

(defun ADDWri (dst r1 imm s)
  (set$ (base-reg dst) (+ (base-reg r1) (lshift imm s))))


(defun SUBXrx64 (rd rn rm off)
  (set$ rd (- rn (extended rm off))))

(defun SUBSXrs (rd rn rm off)
  (add-with-carry rd rn (lnot (shifted rm off)) 1))

(defun SUBSWrs (rd rn rm off)
  (add-with-carry
   (base-reg rd)
   (base-reg rn) (lnot (shifted (base-reg rm) off)) 1))

(defun SUBSWri (rd rn imm off)
  (add-with-carry (base-reg rd) (base-reg rn) (lnot (lshift imm off)) 1))


(defun SUBSXri (rd rn imm off)
  (add-with-carry rd rn (lnot (lshift imm off)) 1))

(defun SUBXrs (rd rn rm off)
  (set$ rd (- rn (shifted rm off))))

(defun SUBXri (rd rn imm off)
  (set$ rd (- rn (lshift imm off))))

(defun SUBWri (rd rn imm off)
  (set$ (base-reg rd) (- (base-reg rn) (lshift imm off))))

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

(defun unsigned-extend (n rm)
  (cast-unsigned (word) (cast-low n rm)))

(defun signed-extend (n rm)
  (cast-signed (word) (cast-low n rm)))

(defun extended (rm bits)
  (declare (visibility :private))
  (let ((typ (extract 5 3 bits))
        (off (extract 2 0 bits)))
    (lshift (case typ
              0b000 (unsigned-extend 8 rm)
              0b001 (unsigned-extend 16 rm)
              0b010 (unsigned-extend 32 rm)
              0b011 rm
              0b100 (signed-extend 8 rm)
              0b101 (signed-extend 16 rm)
              0b110 (signed-extend 32 rm)
              0b111 rm)
            off)))

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

(defun STRWui (src reg off)
  (let ((off (lshift off 2)))
    (store-word (+ reg off) (cast-low 32 (base-reg src)))))

(defun STRXroX (rt rn rm _ shift)
  (store-word (+ rn (lshift rm (* shift 3))) rt))

(defun LDRXroX (rt rn rm _ shift)
  (set$ rt (load-word (+ rn (lshift rm (* shift 3))))))



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
    0b0000 ZF
    0b0001 (lnot ZF)
    0b0010 CF
    0b0010 (lnot CF)
    0b0100 NF
    0b0101 (lnot NF)
    0b0110 VF
    0b0111 (lnot VF)
    0b1000 (logand CF (lnot ZF))
    0b1001 (logor (lnot CF) ZF)
    0b1010 (= NF VF)
    0b1011 (/= NF VF)
    0b1100 (logand (= NF VF) (= ZF 0))
    0b1101 (logor (/= NF VF) (/= ZF 0))
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
