(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; SPECIAL INSTRUCTIONS

(defun make-barrier (barrier-type option)
  (special (symbol-concat :barrier barrier-type (barrier-option-to-symbol option))))

(defun DMB (option) (make-barrier :dmb option))

(defun DSB (option) (make-barrier :dsb option))

;; strictly speaking, only the sy option is valid and is
;; the default option (it can be omitted from the mnemonic).
;; still including option here though
(defun ISB (option) (make-barrier :isb option))

(defun HINT (_)
  (empty))

(defun UDF (exn)
  (special :undefined-instruction))