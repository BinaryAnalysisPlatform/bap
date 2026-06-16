(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; STR

(defun STR.ro* (scale rt rn rm signed shift)
  "stores rt to (rn + rm << (shift * scale)) with signed or unsigned extension 
   of rm, where rt is a register of size (8 << scale). Note that rm can be an X 
   or W register and it chooses the appropriate extend mode implicitly. rn must 
   be an X register."
  (assert (< signed 2))
  (assert-msg (= (word-width rt) (lshift 8 scale))
      "STR.ro*: scale must match size of rt") 
  (store-word
    (+ rn 
      (if (= signed 1) 
        (signed-extend   (word-width rm) (lshift rm (* shift scale)))
        (unsigned-extend (word-width rm) (lshift rm (* shift scale))))) 
    rt))

;; no differences in X or W address variants
(defun STRBroX (rt rn rm option shift) (STR.ro* 0 rt rn rm option shift))
(defun STRBroW (rt rn rm option shift) (STR.ro* 0 rt rn rm option shift))
(defun STRHroX (rt rn rm option shift) (STR.ro* 1 (cast-low 16 rt) rn rm option shift))
(defun STRHroW (rt rn rm option shift) (STR.ro* 1 (cast-low 16 rt) rn rm option shift))
(defun STRSroX (rt rn rm option shift) (STR.ro* 2 rt rn rm option shift))
(defun STRSroW (rt rn rm option shift) (STR.ro* 2 rt rn rm option shift))
(defun STRDroX (rt rn rm option shift) (STR.ro* 3 rt rn rm option shift))
(defun STRDroW (rt rn rm option shift) (STR.ro* 3 rt rn rm option shift))
(defun STRQroX (rt rn rm option shift) (STR.ro* 4 rt rn rm option shift))
(defun STRQroW (rt rn rm option shift) (STR.ro* 4 rt rn rm option shift))

(defun STR.post (xreg src off)
  "stores all of src to xreg, and post-indexes reg (reg += off)."
  (store-word xreg src)
  (set$ xreg (+ xreg off)))

(defun STRQpost (_ rt rn simm) (STR.post rn rt simm))
(defun STRDpost (_ rt rn simm) (STR.post rn rt simm))
(defun STRSpost (_ rt rn simm) (STR.post rn (cast-low 32 rt) simm))
(defun STRHpost (_ rt rn simm) (STR.post rn (cast-low 16 rt) simm))
(defun STRBpost (_ rt rn simm) (STR.post rn (cast-low 8 rt) simm))

(defun STR.pre (xreg src off)
  "stores all of src to xreg, and pre-indexes reg (reg += off)."
  (store-word (+ xreg off) src)
  (set$ xreg (+ xreg off)))

(defun STRQpre (_ rt rn simm) (STR.pre rn rt simm))
(defun STRDpre (_ rt rn simm) (STR.pre rn rt simm))
(defun STRSpre (_ rt rn simm) (STR.pre rn (cast-low 32 rt) simm))
(defun STRHpre (_ rt rn simm) (STR.pre rn (cast-low 16 rt) simm))
(defun STRBpre (_ rt rn simm) (STR.pre rn (cast-low 8 rt) simm))

(defun STR.ui (scale src reg off) 
  "Stores a register of size (8 << scale) to the memory address 
   (reg + (off << scale))."
  (assert-msg (= (word-width src) (lshift 8 scale))
      "STR.ui: scale must match size of register") 
  (store-word (+ reg (lshift off scale)) 
    (cast-unsigned (lshift 8 scale) src)))

(defun STRQui (src reg off) (STR*ui 4 src reg off))
(defun STRDui (src reg off) (STR*ui 3 src reg off))
(defun STRSui (src reg off) (STR*ui 2 src reg off))
(defun STRHui (src reg off) (STR*ui 1 src reg off))
(defun STRBui (src reg off) (STR*ui 0 src reg off))

;;; STP

;; these use store-pair from aarch64-helper.lisp

(defun STPQpost (_ t1 t2 dst off) (store-pair 4 'post t1 t2 dst off))
(defun STPDpost (_ t1 t2 dst off) (store-pair 3 'post t1 t2 dst off))
(defun STPSpost (_ t1 t2 dst off) (store-pair 2 'post t1 t2 dst off))

(defun STPQpre (_ t1 t2 dst off) (store-pair 4 'pre t1 t2 dst off))
(defun STPDpre (_ t1 t2 dst off) (store-pair 3 'pre t1 t2 dst off))
(defun STPSpre (_ t1 t2 dst off) (store-pair 2 'pre t1 t2 dst off))

(defun STPQi (rt rt2 base imm) (store-pair 4 'offset rt rt2 base imm))
(defun STPDi (rt rt2 base imm) (store-pair 3 'offset rt rt2 base imm))
(defun STPSi (rt rt2 base imm) (store-pair 2 'offset rt rt2 base imm))

;;; STUR

(defun STURQi (rn rt imm) (store-word (+ rt imm) rn))
(defun STURDi (rn rt imm) (store-word (+ rt imm) rn))
(defun STURSi (rn rt imm) (store-word (+ rt imm) rn))
(defun STURHi (rn rt imm) (store-word (+ rt imm) rn))
(defun STURBi (rn rt imm) (store-word (+ rt imm) rn))