(require bits)

(declare (context (target armv8-a+le)))
(defpackage aarch64 (:use core target))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun MOVZXi (dst imm shift)
  (set$ dst imm))

(defun ADDXri (dst src imm _shift)
  (set$ dst (+ src imm)))

(defun LDRXui (dst reg off)
  (set$ dst (load-word (+ reg (lshift off 3)))))

(defun LDRSWui (dst reg off)
  (set$ dst (cast-signed
             (word-width)
             (load-word (+ reg (lshift off 2))))))

(defun UBFMXri (xd xr ir is)
  (let ((rs (word-width)))
    (if (< is ir)
        (if (and (/= is (- rs 1)) (= (+ is 1) ir))
            (set$ xd (lshift xr (- rs ir)))
            (set$ xd (lshift
                      (cast-unsigned rs (extract is 0 xr))
                      (- rs ir))))
        (if (= is (- rs 1))
            (set$ xd (rshift xr ir))
            (set$ xd (cast-unsigned rs (extract is ir xr)))))))


(defun ORRXrs (rd rn rm is)
  (set$ rd (logor rn (lshift rm is))))

(defun setw (rd x)
  (set$ rd (cast-unsigned (word-width) x)))

(defun ADRP (dst imm)
  (set$ dst (+ (get-program-counter)
               (cast-signed (word-width) (lshift imm 12)))))

(defun ADDWrs (dst r1 v s)
  (set$ dst (+ r1 (lshift v s))))

(defun ADDWri (dst r1 imm s)
  (ADDWrs dst r1 imm s))

(defun add-with-carry (rd x y c)
  (let ((r (+ x y c)))
    (set NF (msb r))
    (set VF (overflow r x y))
    (set ZF (is-zero r))
    (set CF (carry r x y))
    (set$ rd r)))

(defun SUBXrx64 (rd rn rm off)
  (set$ rd (- rn (shifted rm off))))

(defun SUBSXrs (rd rn rm off)
  (add-with-carry rd rn (lnot (shifted rm off)) 1))

(defun SUBXrs (rd rn rm off)
  (set$ rd (- rn (shifted rm off))))

(defun ADDXrs (rd rn rm off)
  (set$ rd (+ rn (shifted rm off))))

(defun shifted (rm off)
  (let ((typ (extract 7 6 off))
        (off (extract 5 0 off)))
    (case typ
      0b00 (lshift rm off)
      0b01 (rshift rm off)
      0b10 (arshift rm off))))

(defun STPXpre (dst t1 t2 _ off)
  (let ((word (/ (word-width) 8))
        (off (lshift off 3)))
    (store-word (+ dst off) t1)
    (store-word (+ dst off word) t2)
    (set$ dst (+ dst off))))

(defun LDPXpost (dst r1 r2 base off)
  (let ((word (/ (word-width) 8))
        (off (lshift off 3)))
    (set$ r1 (load-word base))
    (set$ r2 (load-word (+ base word)))
    (set$ dst (+ dst off))))

(defun STRXui (src reg off)
  (let ((off (lshift off 3)))
    (store-word (+ reg off) src)))


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
