(in-package aarch64)

(require arm-bits)

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