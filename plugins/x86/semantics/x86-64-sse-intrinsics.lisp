(declare (context (target amd64) (bits 64)
                  (x86-floating-points intrinsic-semantics)))
(require x86-common)
(require x86-64)

(in-package x86-64)

(defconstant +rne+ 0)
(defconstant +rtn+ 1)
(defconstant +rtp+ 2)
(defconstant +rtz+ 3)

;; set it to (read-rounding-mode-from-mxcsr) to enable
;; the rounding mode dynamic dispatch
(defparameter *sse-rmode* 'rne)

(defparameter *sse-format* 'ieee754_binary)

(defun ADDSDrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fadd 64 rd rn ptr sc ix off))

(defun ADDSDrr_Int (rd rn rm)
  (fp-binary 'fadd 64 rd rn rm))

(defun ADDSSrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fadd 32 rd rn ptr sc ix off))

(defun ADDSSrr_Int (rd rn rm)
  (fp-binary 'fadd 32 rd rn rm))

(defun SUBSDrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fsub 64 rd rn ptr sc ix off))

(defun SUBSDrr_Int (rd rn rm)
  (fp-binary 'fsub 64 rd rn rm))

(defun SUBSSrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fsub 32 rd rn ptr sc ix off))

(defun SUBSSrr_Int (rd rn rm)
  (fp-binary 'fsub 32 rd rn rm))

(defun MULSDrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fmul 64 rd rn ptr sc ix off))

(defun MULSDrr_Int (rd rn rm)
  (fp-binary 'fmul 64 rd rn rm))

(defun MULSSrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fmul 32 rd rn ptr sc ix off))

(defun MULSSrr_Int (rd rn rm)
  (fp-binary 'fmul 32 rd rn rm))

(defun DIVSDrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fdiv 64 rd rn ptr sc ix off))

(defun DIVSDrr_Int (rd rn rm)
  (fp-binary 'fdiv 64 rd rn rm))

(defun DIVSSrm_Int (rd rn ptr sc ix off _)
  (fp-binary 'fdiv 32 rd rn ptr sc ix off))

(defun DIVSSrr_Int (rd rn rm)
  (fp-binary 'fdiv 32 rd rn rm))

(defun UCOMISDrm (rn ptr sc ix off _)
  (fp-compare 64 rn ptr sc ix off))

(defun UCOMISSrm (rn ptr sc ix off _)
  (fp-compare 32 rn ptr sc ix off))

(defun UCOMISDrr (rn rm)
  (fp-compare 64 rn rm))

(defun UCOMISSrr (rn rm)
  (fp-compare 32 rn rm))

(defun CVTSI642SDrr_Int (dst _ src)
  (set-sse dst (sse-convert 'cast_sfloat 64 64 src)))

(defun CVTSI642SDrm_Int (dst _ ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sfloat 64 64 (load-bits 64 (ea ptr sc ix off)))))

(defun CVTSI642SSrr_Int (dst _ src)
  (set-sse dst (sse-convert 'cast_sfloat 32 64 src)))

(defun CVTSI642SSrm_Int (dst _ ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sfloat 32 64 (load-bits 64 (ea ptr sc ix off)))))

;; the rm form: (dst base scale index disp segment), like the MCInst
;; operands; the load reads [base + index*scale + disp]
(defun CVTTSS2SI64rr_Int (dst src)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 64 32 src))))

(defun CVTTSS2SI64rm_Int (dst ptr sc ix off _)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 64 32 (load-bits 32 (ea ptr sc ix off))))))

(defun CVTTSS2SIrr_Int (dst src)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 32 32 src))))

(defun CVTTSS2SIrm_Int (dst ptr sc ix off _)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 32 32 (load-bits 32 (ea ptr sc ix off))))))

(defun CVTSS2SI64rr_Int (dst src)
  (set-sse dst (sse-convert 'cast_sint 64 32 src)))

(defun CVTSS2SI64rm_Int (dst ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sint 64 32 (load-bits 32 (ea ptr sc ix off)))))

(defun CVTSS2SIrr_Int (dst src)
  (set-sse dst (sse-convert 'cast_sint 32 32 src)))

(defun CVTSS2SIrm_Int (dst ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sint 32 32 (load-bits 32 (ea ptr sc ix off)))))

(defun CVTTSD2SI64rr_Int (dst src)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 64 64 src))))

(defun CVTTSD2SI64rm_Int (dst ptr sc ix off _)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 64 64 (load-bits 64 (ea ptr sc ix off))))))

(defun CVTTSD2SIrr_Int (dst src)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 32 64 src))))

(defun CVTTSD2SIrm_Int (dst ptr sc ix off _)
  (set-sse dst (with-rmode 'rtz (sse-convert 'cast_sint 32 64 (load-bits 64 (ea ptr sc ix off))))))

(defun CVTSD2SI64rr_Int (dst src)
  (set-sse dst (sse-convert 'cast_sint 64 64 src)))

(defun CVTSD2SI64rm_Int (dst ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sint 64 64 (load-bits 64 (ea ptr sc ix off)))))

(defun CVTSD2SIrr_Int (dst src)
  (set-sse dst (sse-convert 'cast_sint 32 64 src)))

(defun CVTSD2SIrm_Int (dst ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sint 32 64 (load-bits 64 (ea ptr sc ix off)))))

(defun CVTPS2PDrr (dst src)
  (let ((lo (sse-convert 'fconvert 64 32 (extract 31  0 src)))
        (hi (sse-convert 'fconvert 64 32 (extract 63 32 src))))
    (set-sse dst (concat hi lo))))

;; The 32-bit integer source forms: a doubleword operand, signed.
(defun CVTSI2SSrm_Int (dst _ ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sfloat 32 32 (load-bits 32 (ea ptr sc ix off)))))

(defun CVTSI2SDrm_Int (dst _ ptr sc ix off _)
  (set-sse dst (sse-convert 'cast_sfloat 64 32 (load-bits 32 (ea ptr sc ix off)))))

(defun CVTSI2SSrr_Int (dst _ src)
  (set-sse dst (sse-convert 'cast_sfloat 32 32 src)))

(defun CVTSI2SDrr_Int (dst _ src)
  (set-sse dst (sse-convert 'cast_sfloat 64 32 src)))

(defun CVTSD2SSrr_Int (dst _ src)
  (set-sse dst (sse-convert 'fconvert 32 64 src)))

(defun CVTSD2SSrm_Int (dst _ ptr sc ix off _)
  (set-sse dst (sse-convert 'fconvert 32 64 (load-bits 64 (ea ptr sc ix off)))))

(defun CVTSS2SDrr_Int (dst _ src)
  (set-sse dst (sse-convert 'fconvert 64 32 src)))

(defun CVTSS2SDrm_Int (dst _ ptr sc ix off _)
  (set-sse dst (sse-convert 'fconvert 64 32 (load-bits 32 (ea ptr sc ix off)))))

;; MIN/MAX select the destination (rm) only on a strict order; an
;; unordered or equal pair selects the source (rn).  Only the low lane
;; is written.
(defun sse-min (rt rd rm rn)
  (set-sse rd (cast-low rt (if (ieee754-forder rt rm rn) rm rn))))

(defun sse-max (rt rd rm rn)
  (set-sse rd (cast-low rt (if (ieee754-forder rt rn rm) rm rn))))

(defun MINSDrr_Int (rd rm rn) (sse-min 64 rd rm rn))
(defun MAXSDrr_Int (rd rm rn) (sse-max 64 rd rm rn))
(defun MINSSrr_Int (rd rm rn) (sse-min 32 rd rm rn))
(defun MAXSSrr_Int (rd rm rn) (sse-max 32 rd rm rn))

(defun MINSDrm_Int (rd rm ptr sc ix off _)
  (sse-min 64 rd rm (load-bits 64 (ea ptr sc ix off))))
(defun MAXSDrm_Int (rd rm ptr sc ix off _)
  (sse-max 64 rd rm (load-bits 64 (ea ptr sc ix off))))
(defun MINSSrm_Int (rd rm ptr sc ix off _)
  (sse-min 32 rd rm (load-bits 32 (ea ptr sc ix off))))
(defun MAXSSrm_Int (rd rm ptr sc ix off _)
  (sse-max 32 rd rm (load-bits 32 (ea ptr sc ix off))))

;; COMIS differs from UCOMIS only in signaling on a quiet NaN, which is
;; not modeled; the flags are the same.
(defun COMISDrr (rn rm) (fp-compare 64 rn rm))
(defun COMISSrr (rn rm) (fp-compare 32 rn rm))
(defun COMISDrm (rn ptr sc ix off _) (fp-compare 64 rn ptr sc ix off))
(defun COMISSrm (rn ptr sc ix off _) (fp-compare 32 rn ptr sc ix off))

;; CMPSS/CMPSD: the predicate (imm8[2:0]) of the low lanes, as an
;; all-ones or all-zeros mask in the destination's low lane.
(defun sse-cmp-predicate (rt cc a b)
  (let ((nan-a (ieee754-is-nan rt a))
        (nan-b (ieee754-is-nan rt b))
        (lt (ieee754-forder rt a b))
        (gt (ieee754-forder rt b a))
        (nan (logor nan-a nan-b)))
    (case (logand cc 7)
      0 (not nan lt gt)
      1 lt
      2 (logor lt (not nan lt gt))
      3 nan
      4 (logor nan lt gt)
      5 (not lt)
      6 (not lt (not nan lt gt))
      7 (not nan))))

(defun sse-cmp (rt rd a b cc)
  (let ((zero (cast-low rt 0)))
    (set-sse rd (if (sse-cmp-predicate rt cc a b) (lnot zero) zero))))

(defun CMPSDrri_Int (rd rm rn cc) (sse-cmp 64 rd rm rn cc))
(defun CMPSSrri_Int (rd rm rn cc) (sse-cmp 32 rd rm rn cc))
(defun CMPSDrmi_Int (rd rm ptr sc ix off _ cc)
  (sse-cmp 64 rd rm (load-bits 64 (ea ptr sc ix off)) cc))
(defun CMPSSrmi_Int (rd rm ptr sc ix off _ cc)
  (sse-cmp 32 rd rm (load-bits 32 (ea ptr sc ix off)) cc))

;; SQRTSS/SQRTSD: the square root of the source's low lane.
(defun sse-unary (name rt rn)
  (if (is-symbol *sse-rmode*)
      (intrinsic
       (symbol-concat name *sse-rmode* *sse-format* :sep '_)
       (coerce rt rn)
       :result rt)
    (case *sse-rmode*
      +rne+ (with-rmode 'rne (sse-unary name rt rn))
      +rtn+ (with-rmode 'rtn (sse-unary name rt rn))
      +rtp+ (with-rmode 'rtp (sse-unary name rt rn))
      +rtz+ (with-rmode 'rtz (sse-unary name rt rn)))))

(defun SQRTSDr_Int (rd _ rn) (set-sse rd (sse-unary 'fsqrt 64 rn)))
(defun SQRTSSr_Int (rd _ rn) (set-sse rd (sse-unary 'fsqrt 32 rn)))
(defun SQRTSDm_Int (rd _ ptr sc ix off _)
  (set-sse rd (sse-unary 'fsqrt 64 (load-bits 64 (ea ptr sc ix off)))))
(defun SQRTSSm_Int (rd _ ptr sc ix off _)
  (set-sse rd (sse-unary 'fsqrt 32 (load-bits 32 (ea ptr sc ix off)))))

(defun MFENCE ()
  (intrinsic 'mfence))

(defun LFENCE ()
  (intrinsic 'lfence))

(defun SFENCE ()
  (intrinsic 'sfence))

(defun sse-truncate (name rt rs rn)
  (intrinsic
   (symbol-concat name 'rtz *sse-format* :sep '_)
   (coerce rs rn)
   :result rt))

(defun symbol-of-size (sz)
  (case sz
    16  '16
    32  '32
    64  '64
    80  '80
    128 '128
    256 '256
    'unknown))

(defun sse-convert (name rt rs rn)
  (if (is-symbol *sse-rmode*)
      (intrinsic
       (symbol-concat
        name
        *sse-rmode* *sse-format*
        (symbol-of-size rt)
        :sep '_)
       (coerce rs rn)
       :result rt)
    (case *sse-rmode*
      +rne+ (with-rmode 'rne (sse-convert name rt rs rn))
      +rtn+ (with-rmode 'rtn (sse-convert name rt rs rn))
      +rtp+ (with-rmode 'rtp (sse-convert name rt rs rn))
      +rtz+ (with-rmode 'rtz (sse-convert name rt rs rn)))))

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

(defmacro set-sse (rd x)
  (let ((wd (word-width rd))
        (wx (word-width x)))
    (set$ rd (concat (cast-high (- wd wx) rd)
                     (cast-low wx x)))))

(defmacro fp-binary (op rt rd rn ptr sc ix off)
  (set-sse rd (sse-binary op rt rn (load-bits rt (ea ptr sc ix off)))))

(defmacro fp-binary (op rt rd rn rm)
  (set-sse rd (sse-binary op rt rn rm)))

(defmacro fp-compare (rt rn ptr sc ix off)
  (let ((rm (load-bits rt (ea ptr sc ix off))))
    (compare-floats rt rn rm)))

(defmacro fp-compare (rt rn rm)
  (compare-floats rt rn rm))

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
  (let ((is-nan-rn (ieee754-is-nan rt rn))
        (is-nan-rm (ieee754-is-nan rt rm))
        (n<m (ieee754-forder rt rn rm))
        (m<n (ieee754-forder rt rm rn)))
    (set PF (logor is-nan-rn is-nan-rm))
    (set CF (logor PF n<m))
    (set ZF (logor PF (not n<m m<n)))))

(defun read-rounding-mode-from-mxcsr ()
  (rshift (logand MXCSR 0x6000) 13))

(defmacro with-rmode (mode body)
  (let ((*sse-rmode* mode))
    body))
