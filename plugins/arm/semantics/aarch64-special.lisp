(declare (context (target arm armv8-a+le)))

(in-package aarch64)

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