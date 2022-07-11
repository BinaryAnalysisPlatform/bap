(declare (context (target arm armv8-a+le)))

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


(defun bvectosymbol (bv sym) 
  (if (>= (word-width bv) 4)
    (bvectosymbol 
      (cast-low (- (word-width bv) 4) bv) 
      (symbol-concat 
        sym
        (case (cast-high 4 bv)
            0x0 '0
            0x1 '1
            0x2 '2
            0x3 '3
            0x4 '4
            0x5 '5
            0x6 '6
            0x7 '7
            0x8 '8
            0x9 '9
            0xa 'a
            0xb 'b
            0xc 'c
            0xd 'd
            0xe 'e
            0xf 'f
            0 'x
    )))
  sym))

(defun BRK (option)
  (intrinsic (symbol-concat 'software-breakpoint-$ (bvectosymbol option '0x))))
