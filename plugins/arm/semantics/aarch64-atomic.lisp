(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; ATOMIC OPERATIONS

;; WARNING: these llvm mnemonics are very likely incorrect,
;; need to investigate why bap's llvm can't disassemble any instructions
;; from armv8.1 or later.

(defmacro CASordXr (rs rt rn acquire-ordering release-ordering)
  "(CASord*r set load store rs rt rn acquire-ordering release-ordering)
   implements a generic compare-and-swap instruction on a X register.
   acquire-ordering and release-ordering are booleans indicating whether
   load-acquire and store-release ordering is to be enforced."
   (let ((data) (load-word rn))
    (if (acquire-ordering) (special :load-acquire))
    (if (= data rs)
      (if (release-odering) (special :store-release))
      (store-word rn rt))
    (set$ rs data)))

(defun CASXr   (rs rt rn) (CASordXr rs rt rn false false))
(defun CASAXr  (rs rt rn) (CASordXr rs rt rn true  false))
(defun CASLXr  (rs rt rn) (CASordXr rs rt rn false true))
(defun CASALXr (rs rt rn) (CASordXr rs rt rn true  true))

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