(declare (context (target arm-family)
                  (bits 64)))

(in-package aarch64)

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
