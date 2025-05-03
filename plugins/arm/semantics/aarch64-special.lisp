(declare (context (target arm-family)
                  (bits 64)))

(in-package aarch64)

;;; SPECIAL INSTRUCTIONS

(defun make-barrier (barrier-type option)
  (intrinsic (symbol-concat 'barrier
                             barrier-type
                             (barrier-option-to-symbol option)
                             :sep '_)))

(defun DMB (option) (make-barrier 'dmb option))

(defun DSB (option) (make-barrier 'dsb option))

;; strictly speaking, only the sy option is valid and is
;; the default option (it can be omitted from the mnemonic).
;; still including option here though
(defun ISB (option) (make-barrier 'isb option))

(defun HINT (_)
  (empty))

(defun UDF (exn)
  (intrinsic 'undefined-instruction))

(defun BRK (option)
  (intrinsic 'software-breakpoint option))
