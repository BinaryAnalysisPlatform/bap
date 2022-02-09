(declare (context (target arm armv8-a+le)))

(require bits)
(require arm-bits)

(defpackage aarch64 (:use core target arm))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun word () (word-width))

;; instructions are sorted by the categories defined here
;; https://github.com/UQ-PAC/bap/wiki/All-aarch64-Instructions-by-Category


;;; LOADS, MOVES, STORES

;; LD...

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

(defun LDPXpost (dst r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word base))
    (set$ r2 (load-word (+ base (sizeof word))))
    (set$ dst (+ dst off))))

(defun LDPXi (r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word (+ base off)))
    (set$ r2 (load-word (+ base off (sizeof word))))))

(defun LDRXroX (rt rn rm _ shift)
  (set$ rt (load-word (+ rn (lshift rm (* shift 3))))))

;; MOV...

(defmacro MOVZ*i (set dst imm off)
  (set dst (lshift imm off)))

(defun MOVZWi (dst imm off) (MOVZ*i setw dst imm off))
(defun MOVZXi (dst imm off) (MOVZ*i set$ dst imm off))

(defmacro MOVN*i (set dst imm off)
  (set dst (lnot (lshift imm off))))

(defun MOVNWi (dst imm off) (MOVN*i setw dst imm off))
(defun MOVNXi (dst imm off) (MOVN*i set$ dst imm off))

(defmacro MOVK*i (dst reg imm off)
  (let ((mask (lnot (lshift (- (lshift 1 16) 1) off))))
    (set$ dst (logor (logand reg mask) (lshift imm off)))))

(defun MOVKWi (dst reg imm off) (MOVK*i dst reg imm off))
(defun MOVKXi (dst reg imm off) (MOVK*i dst reg imm off))

;; ST...

(defun STRBBui (src reg off)
  (store-byte (+ reg off) src))

(defun STPXpre (dst t1 t2 _ off)
  (let ((off (lshift off 3)))
    (store-word (+ dst off) t1)
    (store-word (+ dst off (sizeof word)) t2)
    (set$ dst (+ dst off))))

(defun STPXi (t1 t2 base off)
  (let ((off (lshift off 4)))
    (store-word base (+ base off))
    (store-word base (+ base off (sizeof word)))))

(defun STRXui (src reg off)
  (let ((off (lshift off 3)))
    (store-word (+ reg off) src)))

(defun STRWui (src reg off)
  (let ((off (lshift off 2)))
    (store-word (+ reg off) (cast-low 32 src))))

(defun STRXroX (rt rn rm _ shift)
  (store-word (+ rn (lshift rm (* shift 3))) rt))

(defmacro STUR*i (src base off size)
  "Takes `size` bits from src and stores at base + off"
  (store-word (+ base off) (cast-low size src)))

(defun STURXi  (src base off) (STUR*i src base off 64))

(defun STURWi  (src base off) (STUR*i src base off 32))

(defun STURHHi  (src base off) (STUR*i src base off 16))

(defun STURBBi (src base off) (STUR*i src base off 8))

;;; LOGICAL/BITFIELD OPERATIONS

;; Logical

(defmacro ORN*rs (set rd rn rm is)
  (set rd (logor rn (lnot (lshift rm is)))))

(defun ORNWrs (rd rn rm is) (ORN*rs setw rd rn rm is))
(defun ORNXrs (rd rn rm is) (ORN*rs set$ rd rn rm is))

(defmacro log*rs (set op rd rn rm is)
  "(log*rs set op rd rn is) implements the logical operation (shift) instruction
   accepting either a W or X register. op is the binary logical operation."
  (set rd (op rn (shifted rm is))))

(defun ORRWrs (rd rn rm is) (log*rs setw logor  rd rn rm is))
(defun EORWrs (rd rn rm is) (log*rs setw logxor rd rn rm is))
(defun ANDWrs (rd rn rm is) (log*rs setw logand rd rn rm is))
(defun ORRXrs (rd rn rm is) (log*rs set$ logor  rd rn rm is))
(defun EORXrs (rd rn rm is) (log*rs set$ logxor rd rn rm is))
(defun ANDXrs (rd rn rm is) (log*rs set$ logand rd rn rm is))

(defmacro log*ri (set op rd rn imm)
  "(log*ri set op rd rn imm) implements the logical operation (immediate) instruction
   accepting either a W or X register. op is the binary logical operation."
  (set rd (op rn (immediate-from-bitmask imm))))

(defun ANDWri (rd rn imm) (log*ri setw logand rd rn imm))
(defun ANDXri (rd rn imm) (log*ri set$ logand rd rn imm))
(defun EORWri (rd rn imm) (log*ri setw logxor rd rn imm))
(defun EORXri (rd rn imm) (log*ri set$ logxor rd rn imm))
(defun ORRWri (rd rn imm) (log*ri setw logor rd rn imm))
(defun ORRXri (rd rn imm) (log*ri set$ logor rd rn imm))

;; UBFM and SBFM

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


;;; INTEGER ARITHMETIC

(defmacro ADD*r* (set shift-function rd rn imm-or-rm off)
  "Implements ADD*ri and ADD*rs by specifying the shift function."
  (set rd (+ rn (shift-function imm-or-rm off))))

;; ADD*ri only uses lshift since the shift arg only zero-extends
;; and doesn't actually change from lshift
(defun ADDWri (rd rn imm off) (ADD*r* setw lshift rd rn imm off))
(defun ADDXri (rd rn imm off) (ADD*r* set$ lshift rd rn imm off))
;; shifted decodes the shift type and shifts
(defun ADDWrs (rd rn rm off) (ADD*r* setw shifted rd rn rm off))
(defun ADDXrs (rd rn rm off) (ADD*r* set$ shifted rd rn rm off))

(defun ADRP (dst imm)
  (set$ dst (+
             (logand (get-program-counter) (lshift -1 12))
             (cast-signed (word) (lshift imm 12)))))

(defmacro SUB*r* (set shift-function rd rn imm-or-rm off)
  "Implements SUB*ri and SUB*rs by specifying the shift function."
  (set rd (- rn (shift-function imm-or-rm off))))

;; see ADD*ri vs ADD*rs
(defun SUBWri (rd rn rm off) (SUB*r* setw lshift rd rn rm off))
(defun SUBXri (rd rn rm off) (SUB*r* set$ lshift rd rn rm off))
(defun SUBWrs (rd rn rm off) (SUB*r* setw shifted rd rn rm off))
(defun SUBXrs (rd rn rm off) (SUB*r* set$ shifted rd rn rm off))

(defun SUBXrx64 (rd rn rm off)
  (set$ rd (- rn (extended rm off))))

(defun SUBSWrs (rd rn rm off)
  (add-with-carry/clear-base rd rn (lnot (shifted rm off)) 1))

(defun SUBSXrs (rd rn rm off)
  (add-with-carry rd rn (lnot (shifted rm off)) 1))

(defun SUBSWri (rd rn imm off)
  (add-with-carry/clear-base rd rn (lnot (lshift imm off)) 1))

(defun SUBSXri (rd rn imm off)
  (add-with-carry rd rn (lnot (lshift imm off)) 1))

(defmacro Mop*rrr (set op rd rn rm ra)
  "(Mop*rrr set op rd rn rm ra) implements multiply-add, multiply-subtract
   etc with W or X registers. op is the binary operation used after *."
  (set rd (op ra (* rn rm))))

(defun MADDWrrr (rd rn rm ra) (Mop*rrr setw + rd rn rm ra))
(defun MADDXrrr (rd rn rm ra) (Mop*rrr set$ + rd rn rm ra))
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


;;; BRANCH INSTRUCTIONS

(defun relative-jump (off)
  (exec-addr (+ (get-program-counter) (lshift off 2))))

(defun B (off)
  (relative-jump off))

(defun Bcc (cnd off)
  (when (condition-holds cnd)
    (relative-jump off)))

(defun BL (off)
  (set LR (+ (get-program-counter) 4))
  (relative-jump off))

(defun BLR (reg)
  (set LR (+ (get-program-counter) 4))
  (exec-addr reg))

(defun BR (reg)
  (exec-addr reg))

(defmacro CB** (comparison reg off)
  "(CB** comparison reg off) implements CBZ and CBNZ by specifying
   the comparison (is-zero or non-zero)."
  (when (comparison reg)
    (relative-jump off)))

(defun CBZW  (reg off) (CB** is-zero  reg off))
(defun CBZX  (reg off) (CB** is-zero  reg off))
(defun CBNZW (reg off) (CB** non-zero reg off))
(defun CBNZX (reg off) (CB** non-zero reg off))

(defun RET (dst)
  (exec-addr dst))

(defmacro TB** (comparison reg pos off)
  "(TB** comparison reg pos off) implements TBZ and TBNZ
   by specifying the comparison (is-zero or non-zero)."
  (when (comparison (select pos reg))
    (relative-jump off)))

(defun TBZW  (reg pos off) (TB** is-zero  reg pos off))
(defun TBZX  (reg pos off) (TB** is-zero  reg pos off))
(defun TBNZW (reg pos off) (TB** non-zero reg pos off))
(defun TBNZX (reg pos off) (TB** non-zero reg pos off))


;;; OTHER ATOMIC OPERATIONS

(defmacro CSop*r (set op rd rn rm cnd)
  "(CSop*r set op rd rn rm cnd) implements the conditional select
   instruction on W or X registers, with op being applied to rm
   when cnd is false."
  (if (condition-holds cnd)
    (set rd rn)
    (set rd (op rm))))

(defun id (arg) "identity function" (declare (visibility :private)) arg)

(defun CSELWr  (rd rn rm cnd) (CSop*r setw id   rd rn rm cnd))
(defun CSELXr  (rd rn rm cnd) (CSop*r set$ id   rd rn rm cnd))
(defun CSINCWr (rd rn rm cnd) (CSop*r setw +1   rd rn rm cnd))
(defun CSINCXr (rd rn rm cnd) (CSop*r set$ +1   rd rn rm cnd))
(defun CSINVWr (rd rn rm cnd) (CSop*r setw lnot rd rn rm cnd))
(defun CSINVXr (rd rn rm cnd) (CSop*r set$ lnot rd rn rm cnd))
(defun CSNEGWr (rd rn rm cnd) (CSop*r setw neg  rd rn rm cnd))  ;; 2's complement negation
(defun CSNEGXr (rd rn rm cnd) (CSop*r set$ neg  rd rn rm cnd))  ;; 2's complement negation


;;; SPECIAL INSTRUCTIONS

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
