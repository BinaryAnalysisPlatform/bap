(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; ATOMIC OPERATIONS

(defmacro CASord* (set load store rs rt rn acquire-ordering release-ordering)
  "(CASord* set load store rs rt rn acquire-ordering release-ordering)
   implements a generic compare-and-swap instruction on a W or X register.
   set is the function to assign to the size of rs and rt.
   load and store are functions to load/store to/from the size of rs and rt.
   acquire-ordering and release-ordering are booleans indicating whether
   load-acquire and store-release ordering is to be enforced."
   (let ((data (load rn)))
    (when acquire-ordering (special :load-acquire))
    (when (= data rs)
      (when release-ordering (special :store-release))
      (store rn rt))
    (set rs data)))

(defmacro store-hword (dst src) (store-word dst (cast-low 32 src)))
(defmacro load-quarter-word (addr) (load-bits 16 addr))
(defmacro store-quarter-word (dst src) (store-word dst (cast-low 16 src)))

(defmacro CASordX (rs rt rn acquire-ordering release-ordering)
  "Specialisation of CASord* for X registers."
  (CASord* set$ load-word store-word rs rt rn acquire-ordering release-ordering))

(defmacro CASordW (rs rt rn acquire-ordering release-ordering)
  "Specialisation of CASord* for W registers."
  (CASord* setw load-hword store-hword rs rt rn acquire-ordering release-ordering))

(defmacro CASordB (rs rt rn acquire-ordering release-ordering)
  "Specialisation of CASord* operating on individual bytes."
  (CASord* setw memory-read store-byte rs rt rn acquire-ordering release-ordering))

(defmacro CASordH (rs rt rn acquire-ordering release-ordering)
  "Specialisation of CASord* for 16-bit values."
  (CASord* setw load-quarter-word store-quarter-word rs rt rn acquire-ordering release-ordering))

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