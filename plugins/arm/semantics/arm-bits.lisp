(defpackage arm (:use core target))
(declare  (context (target arm)))

(in-package arm)

(defun set-nzcv-from-registers (r x y)
  "(set-nzcv-from-registers r x y) sets the processor state flags
   to the result of some arithmetic operation (op x y) with r as the result.
   Common examples include:
    (set-nzcv-from-registers (+ x y) x y)
   or
    (set-nzcv-from-registers (+ x y 1) x y)
   This function was formerly named set-flags, but was renamed to improve clarity."
  (set NF (msb r))
  (set VF (overflow r x y))
  (set ZF (is-zero r))
  (set CF (carry r x y)))

(defun set-nzcv (nzcv)
  "(set-nzcv nzcv) sets the negative, zero, carry and overflow flags to
   the bottom 4 bits of nzcv."
  (set NF (select 3 nzcv))
  (set ZF (select 2 nzcv))
  (set CF (select 1 nzcv))
  (set VF (select 0 nzcv)))

(defun add-with-carry (rd x y c)
  "(add-with-carry rd x y c) sets rd to the result of adding x and y
   with carry bit c, and sets processor flags."
  (let ((r (+ c y x)))
    (set-nzcv-from-registers r x y)
    (set$ rd r)))

(defun add-with-carry/clear-base (rd x y c)
  "(add-with-carry/clear-base rd x y c) sets rd to the result of adding x and y
   with carry bit c after clearing the base register rd, and sets processor flags."
  (let ((r (+ c y x)))
    (set-nzcv-from-registers r y x)
    (clear-base rd)
    (set$ rd r)))

(defun add-with-carry/it-block (rd x y c cnd)
  "(add-with-carry/it-block rd x y c cnd) sets rd to the result of adding x and y
   with carry bit c if cnd holds, and sets processor flags if cnd is unconditional."
  (when (condition-holds cnd)
    (let ((r (+ c y x)))
      (when (is-unconditional cnd)
        (set-nzcv-from-registers r x y))
      (set$ rd r))))

(defun logandnot (rd rn)
  (logand rd (lnot rn)))

(defmacro shift-with-carry (shift rd rn rm cnd)
  "(shift-with-carry shift rd rn rm cnd) sets rd to the shifted
   value of rn and rm, and relevant processor flags, when cnd holds.
   The overflow flag is not changed."
  (when (condition-holds cnd)
    (let ((r (cast-signed (word-width) rn)))
      (when (is-unconditional cnd)
        (set CF (msb r))
        (set$ rd (shift r rm))
        (set ZF (is-zero rd))
        (set NF (msb rd))))))

(defun condition-holds (cnd)
  "(condition-holds cnd) calculates the result of the given condition cnd
   based on the values of processor flags."
  (case cnd
    0b0000 ZF
    0b0001 (lnot ZF)
    0b0010 CF
    0b0011 (lnot CF)
    0b0100 NF
    0b0101 (lnot NF)
    0b0110 VF
    0b0111 (lnot VF)
    0b1000 (logand CF (lnot ZF))
    0b1001 (logor (lnot CF) ZF)
    0b1010 (= NF VF)
    0b1011 (/= NF VF)
    0b1100 (logand (= NF VF) (lnot ZF))
    0b1101 (logor (/= NF VF) ZF)
    true))

(defun is-unconditional (cnd)
  "(is-unconditional cnd) checks whether cnd is unconditional, i.e. 0b1110."
  (= cnd 0b1110))

(defun clear-base (reg)
  "(clear-base reg) clears all of the register reg."
  (set$ (alias-base-register reg) 0))

(defmacro setw (reg val)
  "(set Wx V) sets a Wx register clearing the upper 32 bits."
  (let ((res val))
    (clear-base reg)
    (set$ reg res)))
