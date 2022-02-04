(declare (context (target arm armv8-a+le)))

(require bits)
(require arm-bits)

(defpackage aarch64 (:use core target arm))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun word () (word-width))

(defun MOVZXi (dst imm pos)
  (set$ dst (lshift imm pos)))

(defun MOVZWi (dst imm pos)
  (setw dst (lshift imm pos)))

(defun MOVNWi (dst imm off)
  (setw dst (lnot (lshift imm off))))

(defmacro MOVK*i (dst reg imm off)
  (let ((mask (lnot (lshift (- (lshift 1 16) 1) off))))
    (set$ dst (logor (logand reg mask) (lshift imm off)))))

(defun MOVKWi (dst reg imm off) (MOVK*i dst reg imm off))
(defun MOVKXi (dst reg imm off) (MOVK*i dst reg imm off))

(defun ADDXri (dst src imm off)
  (set$ dst (+ src (lshift imm off))))

(defun LDRXui (dst reg off)
  (set$ dst (load-word (+ reg (lshift off 3)))))

(defun LDRSWui (dst base off)
  (set$ dst (cast-signed
             (word)
             (load-hword (+ base (lshift off 2))))))

(defun LDRWui (dst reg off)
  (setw dst
        (cast-unsigned (word) (load-hword (+ reg (lshift off 2))))))

(defun LDRBBui (dst reg off)
  (setw dst
        (cast-unsigned (word) (load-byte (+ reg off)))))

(defun LDRBBroX (dst reg off _ _)
  (set$ dst
        (cast-unsigned (word) (load-byte (+ reg off)))))

(defmacro make-BFM (set cast xd xr ir is)
  (let ((rs (word)))
    (if (< is ir)
        (if (and (/= is (- rs 1)) (= (+ is 1) ir))
            (set xd (lshift xr (- rs ir)))
            (set xd (lshift
                      (cast rs (extract is 0 xr))
                      (- rs ir))))
        (if (= is (- rs 1))
            (set xd (rshift xr ir))
            (set xd (cast rs (extract is ir xr)))))))

(defun UBFMXri (xd xr ir is)
  (make-BFM set$ cast-unsigned xd xr ir is))

(defun UBFMWri (xd xr ir is)
  (make-BFM setw cast-unsigned xd xr ir is))

(defun SBFMXri (xd xr ir is)
  (make-BFM set$ cast-signed xd xr ir is))

(defun SBFMWri (xd xr ir is)
  (make-BFM setw cast-signed xd xr ir is))

(defmacro ORN*rs (set rd rn rm is)
  (set rd (logor rn (lnot (lshift rm is)))))

(defun ORNWrs (rd rn rm is) (ORN*rs setw rd rn rm is))
(defun ORNXrs (rd rn rm is) (ORN*rs set$ rd rn rm is))

(defmacro log*rs (set op rd rn rm is)
  (set rd (op rn (shifted rm is))))

(defun ORRWrs (rd rn rm is) (log*rs setw logor  rd rn rm is))
(defun EORWrs (rd rn rm is) (log*rs setw logxor rd rn rm is))
(defun ANDWrs (rd rn rm is) (log*rs setw logand rd rn rm is))
(defun ORRXrs (rd rn rm is) (log*rs set$ logor  rd rn rm is))
(defun EORXrs (rd rn rm is) (log*rs set$ logxor rd rn rm is))
(defun ANDXrs (rd rn rm is) (log*rs set$ logand rd rn rm is))

(defmacro log*ri (set op rd rn imm)
  "(log*ri set op rd rn imm) implements the logical operation instruction
   accepting either a W or X register. op is the binary logical operation."
  (set rd (op rn (immediate-from-bitmask imm))))

(defun ANDWri (rd rn imm) (log*ri setw logand rd rn imm))
(defun ANDXri (rd rn imm) (log*ri set$ logand rd rn imm))
(defun EORWri (rd rn imm) (log*ri setw logxor rd rn imm))
(defun EORXri (rd rn imm) (log*ri set$ logxor rd rn imm))
(defun ORRWri (rd rn imm) (log*ri setw logor rd rn imm))
(defun ORRXri (rd rn imm) (log*ri set$ logor rd rn imm))


(defun ADRP (dst imm)
  (set$ dst (+
             (logand (get-program-counter) (lshift -1 12))
             (cast-signed (word) (lshift imm 12)))))

(defun ADDWrs (dst r1 v s)
  (setw dst (+ r1 (lshift v s))))

(defun SUBWrs (dst r1 v s)
  (setw dst (- r1 (lshift v s))))

(defun ADDWri (dst r1 imm s)
  (setw dst (+ r1 (lshift imm s))))


(defun SUBXrx64 (rd rn rm off)
  (set$ rd (- rn (extended rm off))))

(defun SUBSXrs (rd rn rm off)
  (add-with-carry rd rn (lnot (shifted rm off)) 1))

(defun SUBSWrs (rd rn rm off)
  (add-with-carry/clear-base
   rd
   rn (lnot (shifted rm off)) 1))

(defun SUBSWri (rd rn imm off)
  (add-with-carry/clear-base rd rn (lnot (lshift imm off)) 1))


(defun SUBSXri (rd rn imm off)
  (add-with-carry rd rn (lnot (lshift imm off)) 1))

(defun SUBXrs (rd rn rm off)
  (set$ rd (- rn (shifted rm off))))

(defun SUBXri (rd rn imm off)
  (set$ rd (- rn (lshift imm off))))

(defun SUBWri (rd rn imm off)
  (setw rd (- rn (lshift imm off))))

(defun ADDXrs (rd rn rm off)
  (set$ rd (+ rn (shifted rm off))))


(defmacro Mop*rrr (set op rd rn rm ra)
  "(Mop*rrr set op rd rn rm ra) implements multiply-add, multiply-subtract
   etc with W or X registers. op is the binary operation used after *."
  (set rd (op ra (* rn rm))))

;; MUL*rr is alias of MADD*rrr and gets converted
(defun MADDWrrr (rd rn rm ra) (Mop*rrr setw + rd rn rm ra))
(defun MADDXrrr (rd rn rm ra) (Mop*rrr set$ + rd rn rm ra))
;; MNEG*rr is alias of MSUB*rrr and gets converted
(defun MSUBWrrr (rd rn rm ra) (Mop*rrr setw - rd rn rm ra))
(defun MSUBXrrr (rd rn rm ra) (Mop*rrr set$ - rd rn rm ra))


(defmacro *DIV*r (set div rd rn rm)
  "(*DIV*r set div rd rn rm) implements the SDIV or UDIV instructions
   on W or X registers, with div set to s/ or / respectively."
  (if (= rm 0)
    (set rd 0)
    (set rd (div rn rm))))

(defun SDIVWr (rd rn rm) (*DIV*r setw s/ rd rn rm))
(defun SDIVXr (rd rn rm) (*DIV*r set$ s/ rd rn rm))
(defun UDIVWr (rd rn rm) (*DIV*r setw /  rd rn rm))
(defun UDIVXr (rd rn rm) (*DIV*r set$ /  rd rn rm))


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

(defun STPXi (t1 t2 base off)
  (let ((off (lshift off 4)))
    (store-word base (+ base off))
    (store-word base (+ base off (sizeof word)))))

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
    (store-word (+ reg off) (cast-low 32 src))))

(defun STRXroX (rt rn rm _ shift)
  (store-word (+ rn (lshift rm (* shift 3))) rt))

(defun LDRXroX (rt rn rm _ shift)
  (set$ rt (load-word (+ rn (lshift rm (* shift 3))))))


(defmacro CSop*r (set op rd rn rm cnd)
  "(CSop*r set op rd rn rm cnd) implements the conditional select
   instruction on W or X registers, with op being applied to rm
   when cnd is false."
  (if (condition-holds cnd)
    (set rd rn)
    (set rd (op rm))))

(defun id (arg) (declare (visibility :private)) arg)

(defun CSELWr  (rd rn rm cnd) (CSop*r setw id   rd rn rm cnd))
(defun CSELXr  (rd rn rm cnd) (CSop*r set$ id   rd rn rm cnd))
(defun CSINCWr (rd rn rm cnd) (CSop*r setw +1   rd rn rm cnd))
(defun CSINCXr (rd rn rm cnd) (CSop*r set$ +1   rd rn rm cnd))
(defun CSINVWr (rd rn rm cnd) (CSop*r setw lnot rd rn rm cnd))
(defun CSINVXr (rd rn rm cnd) (CSop*r set$ lnot rd rn rm cnd))
(defun CSNEGWr (rd rn rm cnd) (CSop*r setw neg  rd rn rm cnd))  ;; 2's complement negation
(defun CSNEGXr (rd rn rm cnd) (CSop*r set$ neg  rd rn rm cnd))  ;; 2's complement negation


(defun STRBBui (src reg off)
  (store-byte (+ reg off) src))

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
  (when (is-zero reg)
    (relative-jump off)))

(defun CBNZX (reg off)
  (when (/= reg 0)
    (relative-jump off)))

(defun CBNZW (reg off)
  (when (/= reg 0)
    (relative-jump off)))

(defun Bcc (cnd off)
  (when (condition-holds cnd)
    (relative-jump off)))


(defun DMB (option)
  (special (barrier-option-to-symbol :dmb option)))

(defun DSB (option)
  (special (barrier-option-to-symbol :dsb option)))

(defun ISB (option)
  ;; strictly speaking, only the sy option is valid and is
  ;; the default option (it can be omitted from the mnemonic).
  ;; still including option here though
  (special (barrier-option-to-symbol :dmb option)))

(defun HINT (_)
  (empty))

(defun UDF (exn)
  (special :undefined-instruction))
