(declare (context (target amd64) (bits 64)))
(require x86-common)

(defpackage x86-64 (:use core target amd64 x86_64))
(defpackage llvm-x86_64 (:use x86_64))

(in-package x86-64)

(defconstant +rne+ 0)
(defconstant +rtn+ 1)
(defconstant +rtp+ 2)
(defconstant +rtz+ 3)

;; set it to (read-rounding-mode-from-mxcsr) to enable
;; the rounding mode dynamic dispatch
(defparameter *sse-rmode* 'rne)

(defparameter *sse-format* 'ieee754_binary)

(defun ADDSDrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fadd 64 rd rn ptr off))

(defun ADDSDrr_Int (rd rn rm)
  (fp-binary 'fadd 64 rd rn rm))

(defun ADDSSrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fadd 32 rd rn ptr off))

(defun ADDSSrr_Int (rd rn rm)
  (fp-binary 'fadd 32 rd rn rm))

(defun SUBSDrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fsub 64 rd rn ptr off))

(defun SUBSDrr_Int (rd rn rm)
  (fp-binary 'fsub 64 rd rn rm))

(defun SUBSSrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fsub 32 rd rn ptr off))

(defun SUBSSrr_Int (rd rn rm)
  (fp-binary 'fsub 32 rd rn rm))

(defun MULSDrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fmul 64 rd rn ptr off))

(defun MULSDrr_Int (rd rn rm)
  (fp-binary 'fmul 64 rd rn rm))

(defun MULSSrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fmul 32 rd rn ptr off))

(defun MULSSrr_Int (rd rn rm)
  (fp-binary 'fmul 32 rd rn rm))

(defun DIVSDrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fdiv 64 rd rn ptr off))

(defun DIVSDrr_Int (rd rn rm)
  (fp-binary 'fdiv 64 rd rn rm))

(defun DIVSSrm_Int (rd rn ptr _ _ off _)
  (fp-binary 'fdiv 32 rd rn ptr off))

(defun DIVSSrr_Int (rd rn rm)
  (fp-binary 'fdiv 32 rd rn rm))

(defun UCOMISDrm (rn ptr _ _ off _)
  (fp-compare 64 rn ptr off))


(defun sse-binary (name rt rn rm)
  (if (is-symbol *sse-rmode*)
      (intrinsic
       (symbol-concat name *sse-rmode* *sse-format* :sep '_)
       (coerce rt rn)
       (coerce rt rm)
       :result rt)
    (case *sse-rmode*
      +rne+ (with-rmode 'rne (sse-binary name rt rn rm))
      +rtn+ (with-rmode 'rtn (sse-binary name rt rn rm))
      +rtp+ (with-rmode 'rtp (sse-binary name rt rn rm))
      +rtz+ (with-rmode 'rtz (sse-binary name rt rn rm)))))

(defmacro fp-binary (op rt rd rn ptr off)
  (set$ rd (sse-binary op rt rn (load-mem ptr off))))

(defmacro fp-binary (op rt rd rn rm)
  (set$ rd (sse-binary op rt rn rm)))

(defmacro fp-compare (rt rn ptr off)
  (let ((rm (load-mem ptr off)))
    (compare-floats rt rn rm)))

(defmacro fp-compare (rt rn rm)
  (compare-floats rt rm rm))

(defun ieee754-is-nan (rt rn)
  (intrinsic 'is_nan_ieee754_binary
             (coerce rt rn)
             :result 1))

(defun ieee754-forder (rt rn rm)
  (intrinsic 'forder_ieee754_binary
             (coerce rt rn)
             (coerce rt rm)
             :result 1))

(defun set-unordered ()
  (set ZF 1)
  (set PF 1)
  (set CF 1))

(defun set-less ()
  (set ZF 0)
  (set PF 0)
  (set CF 1))

(defun set-greater ()
  (set ZF 0)
  (set PF 0)
  (set CF 0))

(defun set-equal ()
  (set ZF 1)
  (set PF 0)
  (set CF 0))

(defun compare-floats (rt rn rm)
  (if (ieee754-is-nan rt rn)
      (set-unordered)
    (if (ieee754-is-nan rt rm)
        (set-unordered)
      (if (ieee754-forder rt rn rm)
          (set-less)
        (if (ieee754-forder rt rm rn)
            (set-greater)
          (set-equal))))))

(defun read-rounding-mode-from-mxcsr ()
  (rshift (logand MXCSR 0x6000) 13))

(defmacro with-rmode (mode body)
  (let ((*sse-rmode* mode))
    body))


(defun is-rip (reg)
  (= (symbol reg) 'RIP))

(defun reg# (reg)
  (if (is-rip reg)
      (+ (get-program-counter) 8)
    reg))

(defun load-mem (reg off)
  (load-word (+ (reg# reg) off)))
