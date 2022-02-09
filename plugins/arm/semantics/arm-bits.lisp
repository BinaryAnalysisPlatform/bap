(defpackage arm (:use core target))
(declare  (context (target arm)))

(in-package arm)

(defun set-flags (r x y)
  (set NF (msb r))
  (set VF (overflow r x y))
  (set ZF (is-zero r))
  (set CF (carry r x y)))

(defun add-with-carry (rd x y c)
  (let ((r (+ c y x)))
    (set-flags r x y)
    (set$ rd r)))

(defun add-with-carry/clear-base (rd x y c)
  (let ((r (+ c y x)))
    (set-flags r y x)
    (clear-base rd)
    (set$ rd r)))

(defun add-with-carry/it-block (rd x y c cnd)
  (when (condition-holds cnd)
    (let ((r (+ c y x)))
      (when (is-unconditional cnd)
        (set-flags r x y))
      (set$ rd r))))

(defun logandnot (rd rn)
  (logand rd (lnot rn)))

(defmacro shift-with-carry (shift rd rn rm cnd)
  (when (condition-holds cnd)
    (let ((r (cast-signed (word-width) rn)))
      (when (is-unconditional cnd)
        (set CF (msb r))
        (set$ rd (shift r rm))
        (set ZF (is-zero rd))
        (set NF (msb rd))))))

(defun condition-holds (cnd)
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
  (= cnd 0b1110))

(defun clear-base (reg)
  (set$ (alias-base-register reg) 0))

(defmacro setw (reg val)
  "(set Wx V) sets a Wx register clearing the upper 32 bits."
  (let ((res val))
    (clear-base reg)
    (set$ reg res)))
