(declare (context (target amd64) (bits 64)))
(require x86-common)

(defpackage x86-64 (:use core target amd64 x86_64))
(defpackage llvm-x86_64 (:use x86_64))

(in-package x86-64)


(defun TRAP ()
  (intrinsic '__ud2 :aborts))

;; If the ESP register is used as a base register for addressing a
;; destination operand in memory, the POP instruction computes the
;; effective address of the operand after it increments the ESP register.
;; The POP ESP instruction increments the stack pointer (ESP) before data
;; at the old top of stack is written into the destination.
(defun POP64rmm (ptr _ _ _ _)
  (+= RSP 8)
  (store-word ptr (load-word (- RSP 8))))

(defun is-rip (reg)
  (= (symbol reg) 'RIP))

(defun reg# (reg)
  (if (is-rip reg)
      (+ (get-program-counter) 8)
    reg))

(defun load-mem (reg off)
  (load-word (+ (reg# reg) off)))


;; shufps (shuffle packed single precision floating-point values)
;; Example (intel): shufps xmm0, xmm1, 1
;; Reference: Vol.2B 4-647
(defun SHUFPSrri (dst _ src ctl)
  (shufps dst src ctl))

(defun SHUFPSrmi (dst _ base _  _ off _ ctl)
  (let ((src (load-dword (+ base off))))
    (shufps dst src ctl)))

(defun shufps (dst src ctl)
  (declare (visibility :private))
  (set$ dst (concat
             (select4 src (extract 7 6 ctl))
             (select4 src (extract 5 4 ctl))
             (select4 src (extract 3 2 ctl))
             (select4 src (extract 1 0 ctl)))))

;; (select4 R N) selects an Nth 32-bit word from R.
;; Reference: Vol. 2B 4-648
(defun select4 (src ctl)
  (declare (visibility :private))
  (case ctl
    0 (extract 31 0 src)
    1 (extract 63 32 src)
    2 (extract 95 64 src)
    3 (extract 127 96 src)
    (error "expects a 2-bit selector")))

;; pmuludq
;; Reference: Vol. 2B 4-370
(defun PMULDQrr (dst _ src)
  (pmul dst src))

(defun PMULDQrm (dst _ base _ _ off _)
  (let ((src (load-dword (+ base off))))
    (pmul dst src)))

(defun pmul (dst src)
  (declare (visibility :private))
  (let ((hi (* (cast-signed 64 (extract 95 64 dst))
               (cast-signed 64 (extract 95 64 src))))
        (lo (* (cast-signed 64 (extract 31  0 dst))
               (cast-signed 64 (extract 31  0 src)))))
    (set$ dst (concat hi lo))))


;; pack{u,s}sdw/pack{u,s}swb
;; converts 4 32-bit words into 8 16-bit words
(defun PACKSSDWrr (dst _ src)
  (packsdw/128 :signed dst dst src))

(defun PACKSSDWrm (dst _ base _ _ off _)
  (let ((src (load-dword (+ base off))))
    (packsdw/128 :signed dst dst src)))

(defun VPACKSSDWrr (dst src1 src2)
  (packsdw/128 :signed dst src1 src2))

(defun VPACKSSDWYrr (dst src1 src2)
  (packsdw/256 :signed dst src1 src2))

(defun VPACKSSDWrm (dst src1 base _ _ off _)
  (let ((src2 (load-dword (+ base off))))
    (packsdw/128 :signed dst src1 src2)))

(defun VPACKSSDWYrm (dst src1 base _ _ off _)
  (let ((src2 (load-qword (+ base off))))
    (packsdw/256 :signed dst src1 src2)))


(defun PACKUSDWrr (dst _ src)
  (packsdw/128 :unsigned dst dst src))

(defun PACKUSDWrm (dst _ base _ _ off _)
  (let ((src (load-dword (+ base off))))
    (packsdw/128 :unsigned dst dst src)))

(defun VPACKUSDWrr (dst src1 src2)
  (packsdw/128 :unsigned dst src1 src2))

(defun VPACKUSDWYrr (dst src1 src2)
  (packsdw/256 :unsigned dst src1 src2))

(defun VPACKUSDWrm (dst src1 base _ _ off _)
  (let ((src2 (load-dword (+ base off))))
    (packsdw/128 :unsigned dst src1 src2)))

(defun VPACKUSDWYrm (dst src1 base _ _ off _)
  (let ((src2 (load-qword (+ base off))))
    (packsdw/256 :unsigned dst src1 src2)))

(defun packsdw/128 (kind dst src1 src2)
  (let ((w0 (extract-saturate kind 16  31  0 src1))
        (w1 (extract-saturate kind 16  63 32 src1))
        (w2 (extract-saturate kind 16  95 64 src1))
        (w3 (extract-saturate kind 16 127 96 src1))
        (w4 (extract-saturate kind 16  31  0 src2))
        (w5 (extract-saturate kind 16  63 32 src2))
        (w6 (extract-saturate kind 16  95 64 src2))
        (w7 (extract-saturate kind 16 127 96 src2)))
    (set$ dst (concat w7 w6 w5 w4 w3 w2 w1 w0))))

(defun packsdw/256 (kind dst src1 src2)
  (let ((w0  (extract-saturate kind 16  31   0 src1))
        (w1  (extract-saturate kind 16  63  32 src1))
        (w2  (extract-saturate kind 16  95  64 src1))
        (w3  (extract-saturate kind 16 127  96 src1))
        (w4  (extract-saturate kind 16  31   0 src2))
        (w5  (extract-saturate kind 16  63  32 src2))
        (w6  (extract-saturate kind 16  95  64 src2))
        (w7  (extract-saturate kind 16 127  96 src2))
        (w8  (extract-saturate kind 16 159 128 src1))
        (w9  (extract-saturate kind 16 191 160 src1))
        (w10 (extract-saturate kind 16 223 192 src1))
        (w11 (extract-saturate kind 16 255 224 src1))
        (w12 (extract-saturate kind 16 159 128 src2))
        (w13 (extract-saturate kind 16 191 160 src2))
        (w14 (extract-saturate kind 16 223 192 src2))
        (w15 (extract-saturate kind 16 255 224 src2)))
    (set$ dst (concat
               w15 w14 w13 w12 w11 w10 w9 w8
                w7  w6  w5  w4  w3  w2 w1 w0))))

(defun PACKSSWBrr (dst _ src)
  (packswb/128 :signed dst dst src))

(defun VPACKSSWBrr (dst src1 src2)
  (packswb/128 :signed dst src1 src2))

(defun VPACKSSWBYrr (dst src1 src2)
  (packswb/256 :signed dst src1 src2))

(defun PACKSSWBrm (dst _ base _ _ off _)
  (let ((src (load-dword (+ base off))))
    (packswb/128 :signed dst dst src)))

(defun VPACKSSWBrm (dst src1 base _ _ off _)
  (let ((src2 (load-dword (+ base off))))
    (packswb/128 :signed dst src1 src2)))

(defun VPACKSSWBYrm (dst src1 base _ _ off _)
  (let ((src2 (load-qword (+ base off))))
    (packswb/128 :signed dst src1 src2)))


(defun PACKUSWBrr (dst _ src)
  (packswb/128 :unsigned dst dst src))

(defun VPACKUSWBrr (dst src1 src2)
  (packswb/128 :unsigned dst src1 src2))

(defun VPACKUSWBYrr (dst src1 src2)
  (packswb/256 :unsigned dst src1 src2))

(defun PACKUSWBrm (dst _ base _ _ off _)
  (let ((src (load-dword (+ base off))))
    (packswb/128 :unsigned dst dst src)))

(defun VPACKUSWBrm (dst src1 base _ _ off _)
  (let ((src2 (load-dword (+ base off))))
    (packswb/128 :unsigned dst src1 src2)))

(defun VPACKUSWBYrm (dst src1 base _ _ off _)
  (let ((src2 (load-qword (+ base off))))
    (packswb/128 :unsigned dst src1 src2)))

(defun packswb/128 (kind dst src1 src2)
  (let ((b0  (extract-saturate kind 8  15   0 src1))
        (b1  (extract-saturate kind 8  31  16 src1))
        (b2  (extract-saturate kind 8  47  32 src1))
        (b3  (extract-saturate kind 8  63  48 src1))
        (b4  (extract-saturate kind 8  79  64 src1))
        (b5  (extract-saturate kind 8  95  80 src1))
        (b6  (extract-saturate kind 8 111  96 src1))
        (b7  (extract-saturate kind 8 127 112 src1))
        (b8  (extract-saturate kind 8  15   0 src2))
        (b9  (extract-saturate kind 8  31  16 src2))
        (b10 (extract-saturate kind 8  47  32 src2))
        (b11 (extract-saturate kind 8  63  48 src2))
        (b12 (extract-saturate kind 8  79  64 src2))
        (b13 (extract-saturate kind 8  95  80 src2))
        (b14 (extract-saturate kind 8 111  96 src2))
        (b15 (extract-saturate kind 8 127 112 src2)))
    (set$ dst (concat
               b15 b14 b13 b12 b11 b10 b9 b8
                b7  b6  b5  b4  b3  b2 b1 b0))))

(defun packswb/256 (kind dst src1 src2)
  (let ((b0  (extract-saturate kind 8  15   0 src1))
        (b1  (extract-saturate kind 8  31  16 src1))
        (b2  (extract-saturate kind 8  47  32 src1))
        (b3  (extract-saturate kind 8  63  48 src1))
        (b4  (extract-saturate kind 8  79  64 src1))
        (b5  (extract-saturate kind 8  95  80 src1))
        (b6  (extract-saturate kind 8 111  96 src1))
        (b7  (extract-saturate kind 8 127 112 src1))
        (b8  (extract-saturate kind 8  15   0 src2))
        (b9  (extract-saturate kind 8  31  16 src2))
        (b10 (extract-saturate kind 8  47  32 src2))
        (b11 (extract-saturate kind 8  63  48 src2))
        (b12 (extract-saturate kind 8  79  64 src2))
        (b13 (extract-saturate kind 8  95  80 src2))
        (b14 (extract-saturate kind 8 111  96 src2))
        (b15 (extract-saturate kind 8 127 112 src2))
        (b16 (extract-saturate kind 8 143 128 src1))
        (b17 (extract-saturate kind 8 159 144 src1))
        (b18 (extract-saturate kind 8 175 160 src1))
        (b19 (extract-saturate kind 8 191 176 src1))
        (b20 (extract-saturate kind 8 207 192 src1))
        (b21 (extract-saturate kind 8 223 208 src1))
        (b22 (extract-saturate kind 8 239 224 src1))
        (b23 (extract-saturate kind 8 255 240 src1))
        (b24 (extract-saturate kind 8 143 128 src2))
        (b25 (extract-saturate kind 8 159 144 src2))
        (b26 (extract-saturate kind 8 175 160 src2))
        (b27 (extract-saturate kind 8 191 176 src2))
        (b28 (extract-saturate kind 8 207 192 src2))
        (b29 (extract-saturate kind 8 223 208 src2))
        (b30 (extract-saturate kind 8 239 224 src2))
        (b31 (extract-saturate kind 8 255 240 src2)))
    (set$ dst (concat
               b31 b30 b29 b28 b27 b26 b25 b24
               b23 b22 b21 b20 b19 b18 b17 b16
               b15 b14 b13 b12 b11 b10  b9  b8
                b7  b6  b5  b4  b3  b2  b1  b0))))

(defun extract-saturate (type size hi lo src)
  (let ((x (extract hi lo src)))
    (cast-saturate type size x)))

(defun VZEROUPPER ()
  (dolist zero-high-128-bits
    'YMM0 'YMM1 'YMM2  'YMM3  'YMM4  'YMM5  'YMM6  'YMM7
    'YMM8 'YMM9 'YMM10 'YMM11 'YMM12 'YMM13 'YMM14 'YMM15))

(defun zero-high-128-bits (reg)
  (set$ reg (concat 0:128 (cast-low 128 (unquote reg)))))

;; MOVNTDQ - Store packed Integers Using Non-Temporal Hint
(defun MOVNTDQmr (base _ _ off _ src)
  (non-temporal-store-word base off src))

(defun VMOVNTDQmr (base _ _ off _ src)
  (non-temporal-store-word base off src))

(defun VMOVNTDQYmr (base _ _ off _ src)
  (non-temporal-store-word base off src))

;; to be overriden by specialized analyses
(defun non-temporal-store-word (base off src)
  (store-word (+ base off) src))
