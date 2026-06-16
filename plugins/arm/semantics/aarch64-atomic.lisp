(declare (context (target arm-family)
                  (bits 64)))

(in-package aarch64)

;;; ATOMIC OPERATIONS

(defmacro CASord* (set load store rs rt rn acquire release)
  "(CASord* set load store rs rt rn acquire release)
   implements a generic compare-and-swap instruction on a W or X register.
   set is the function to assign to the size of rs and rt.
   load and store are functions to load/store to/from the size of rs and rt.
   acquire and release are booleans indicating whether load-acquire and
   store-release ordering is to be enforced."
  (let ((data (load rn)))
    (when acquire (intrinsic 'load-acquire))
    (when (= data rs)
      (when release (intrinsic 'store-release))
      (store rn rt))
    (set rs data)))

(defmacro store-hword (dst src) (store-word dst (cast-low 32 src)))
(defmacro load-quarter-word (addr) (load-bits 16 addr))
(defmacro store-quarter-word (dst src) (store-word dst (cast-low 16 src)))

(defmacro CASordX (rs rt rn acquire release)
  "Specialisation of CASord* for X registers."
  (CASord* set$ load-word store-word rs rt rn acquire release))

(defmacro CASordW (rs rt rn acquire release)
  "Specialisation of CASord* for W registers."
  (CASord* setw load-hword store-hword rs rt rn acquire release))

(defmacro CASordB (rs rt rn acquire release)
  "Specialisation of CASord* operating on individual bytes."
  (CASord* setw memory-read store-byte rs rt rn acquire release))

(defmacro CASordH (rs rt rn acquire release)
  "Specialisation of CASord* for 16-bit values."
  (CASord* setw load-quarter-word store-quarter-word rs rt rn acquire release))

;; not sure why llvm returns 4 arguments.
;; when i've tested it, the first and second arguments are always the same value
;; so i'm just assuming they're the same and ignoring the second.
(defun CASX   (rs _ rt rn) (CASordX rs rt rn false false))
(defun CASAX  (rs _ rt rn) (CASordX rs rt rn true  false))
(defun CASLX  (rs _ rt rn) (CASordX rs rt rn false true))
(defun CASALX (rs _ rt rn) (CASordX rs rt rn true  true))

(defun CASW   (rs _ rt rn) (CASordW rs rt rn false false))
(defun CASAW  (rs _ rt rn) (CASordW rs rt rn true  false))
(defun CASLW  (rs _ rt rn) (CASordW rs rt rn false true))
(defun CASALW (rs _ rt rn) (CASordW rs rt rn true  true))

(defun CASB   (rs _ rt rn) (CASordB rs rt rn false false))
(defun CASAB  (rs _ rt rn) (CASordB rs rt rn true  false))
(defun CASLB  (rs _ rt rn) (CASordB rs rt rn false true))
(defun CASALB (rs _ rt rn) (CASordB rs rt rn true  true))

(defun CASH   (rs _ rt rn) (CASordH rs rt rn false false))
(defun CASAH  (rs _ rt rn) (CASordH rs rt rn true  false))
(defun CASLH  (rs _ rt rn) (CASordH rs rt rn false true))
(defun CASALH (rs _ rt rn) (CASordH rs rt rn true  true))

(defun first  (x y) (declare (visibility :private)) x)
(defun second (x y) (declare (visibility :private)) y)

(defmacro CASPord* (set load rs-pair rt-pair rn register-width acquire release)
  "(CASP* set load store rs-pair rt-pair rn register-width acquire release)
   implements a compare-and-swap-pair instruction for W and X registers.
   set is the functions to set to a register in the pair.
   register-width is 64 or 32, depending on the size of register used.
   load either loads 128 bits or 64 (the size of the whole pair).
   acquire and release are as in the CASord* macro."
  (let ((data (load rn))
        (lower (cast-low  register-width data))
        (upper (cast-high register-width data)))
    (when acquire (intrinsic 'load-acquire))
    (when (= data (register-pair-concat rs-pair))
      (when release (intrinsic 'store-release))
      (store-word rn (register-pair-concat rt-pair)))
    (set$ (nth-reg-in-group rs-pair 0) (endian first  upper lower))
    (set$ (nth-reg-in-group rs-pair 1) (endian second upper lower))))

(defmacro CASPordX (rs-pair rt-pair rn acquire release)
  "Specialisation of CASPord* for X registers."
  (CASPord* set$ load-dword rs-pair rt-pair rn 64 acquire release))

(defmacro CASPordW (rs-pair rt-pair rn acquire release)
  "Specialisation of CASPord* for W registers."
  (CASPord* setw load-word rs-pair rt-pair rn 32 acquire release))

(defun CASPX   (rs-pair _ rt-pair rn) (CASPordX rs-pair rt-pair rn false false))
(defun CASPAX  (rs-pair _ rt-pair rn) (CASPordX rs-pair rt-pair rn true  false))
(defun CASPLX  (rs-pair _ rt-pair rn) (CASPordX rs-pair rt-pair rn false true))
(defun CASPALX (rs-pair _ rt-pair rn) (CASPordX rs-pair rt-pair rn true  true))

(defun CASPW   (rs-pair _ rt-pair rn) (CASPordW rs-pair rt-pair rn false false))
(defun CASPAW  (rs-pair _ rt-pair rn) (CASPordW rs-pair rt-pair rn true  false))
(defun CASPLW  (rs-pair _ rt-pair rn) (CASPordW rs-pair rt-pair rn false true))
(defun CASPALW (rs-pair _ rt-pair rn) (CASPordW rs-pair rt-pair rn true  true))

(defmacro CSop*r (set op rd rn rm cnd)
  "(CSop*r set op rd rn rm cnd) implements the conditional select
   instruction on W or X registers, with op being applied to rm
   when cnd is false."
  (if (condition-holds cnd)
    (set rd rn)
    (set rd (op rm))))

(defun id (arg) "identity function" (declare (visibility :private)) arg)

(defun CSELWr  (rd rn rm cnd) (CSop*r setw id   rd rn rm cnd))
(defun CSELXr  (rd rn rm cnd) (CSop*r set$ id   rd rn rm cnd))
(defun CSINCWr (rd rn rm cnd) (CSop*r setw +1   rd rn rm cnd))
(defun CSINCXr (rd rn rm cnd) (CSop*r set$ +1   rd rn rm cnd))
(defun CSINVWr (rd rn rm cnd) (CSop*r setw lnot rd rn rm cnd))
(defun CSINVXr (rd rn rm cnd) (CSop*r set$ lnot rd rn rm cnd))
(defun CSNEGWr (rd rn rm cnd) (CSop*r setw neg  rd rn rm cnd))  ;; 2's complement negation
(defun CSNEGXr (rd rn rm cnd) (CSop*r set$ neg  rd rn rm cnd))  ;; 2's complement negation
