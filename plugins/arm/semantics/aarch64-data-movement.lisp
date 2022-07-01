(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; LOADS, MOVES, STORES

;; LD...

(defun LDRXui (dst reg off)
  (set$ dst (load-word (+ reg (lshift off 3)))))

(defun LDRSWui (dst base off)
  (set$ dst (cast-signed
             (word)
             (load-hword (+ base (lshift off 2))))))

(defun LDRWui (dst reg off)
  (setw dst
        (cast-unsigned (word) (load-hword (+ reg (lshift off 2))))))

(defun LDRBBpost (_ dst base simm)
	(setw dst (cast-unsigned 32 (load-byte base)))
	(set$ base (+ base simm)))

(defun LDRBBpre (_ dst base simm)
	(setw dst (cast-unsigned 32 (load-byte (+ base simm)))))

(defun LDRBBui (dst reg off)
  (setw dst
        (cast-unsigned (word) (load-byte (+ reg off)))))

(defun LDRBBroW (dst reg off signed shift)
	(if (= signed 1)
			(setw dst (cast-unsigned 32 (load-byte (+ reg (cast-signed 64 off)))))
		(setw dst (cast-unsigned 32 (load-byte (+ reg (cast-unsigned 64 off)))))))

(defun LDRBBroX (dst reg off _ _)
  (setw dst
        (cast-unsigned (word) (load-byte (+ reg (cast-signed 64 off))))))

(defun LDPXpost (dst r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word base))
    (set$ r2 (load-word (+ base (sizeof word))))
    (set$ dst (+ dst off))))

(defun LDPXi (r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word (+ base off)))
    (set$ r2 (load-word (+ base off (sizeof word))))))

(defun LDPWi (wn wm xn off)
	(let ((off (lshift off 2)))
		(setw wn (load-hword (+ xn off)))
		(setw wm (load-hword (+ xn off 4)))))

(defun LDRXroX (rt rn rm _ shift)
  (set$ rt (load-word (+ rn (lshift rm (* shift 3))))))

;; MOV...

(defmacro MOVZ*i (set dst imm off)
  (set dst (lshift imm off)))

(defun MOVZWi (dst imm off) (MOVZ*i setw dst imm off))
(defun MOVZXi (dst imm off) (MOVZ*i set$ dst imm off))

(defmacro MOVN*i (set dst imm off)
  (set dst (lnot (lshift imm off))))

(defun MOVNWi (dst imm off) (MOVN*i setw dst imm off))
(defun MOVNXi (dst imm off) (MOVN*i set$ dst imm off))

(defmacro MOVK*i (dst reg imm off)
  (let ((mask (lnot (lshift (- (lshift 1 16) 1) off))))
    (set$ dst (logor (logand reg mask) (lshift imm off)))))

(defun MOVKWi (dst reg imm off) (MOVK*i dst reg imm off))
(defun MOVKXi (dst reg imm off) (MOVK*i dst reg imm off))

;; ST...

; STRB
(defun STRBBui (src reg off)
  (store-byte (+ reg off) src))

; STR (register)
(defun str-reg (scale rt rn rm signed shift)
  "stores rt to (rn + rm << (shift * scale)) with signed or unsigned extension 
  of rm, where rt is a register of size (8 << scale). Note that rm can be an X 
  or W register and it chooses the appropriate extend mode implicitly. rn must 
  be an X register."
  (assert (< signed 2))
  (assert-msg (= (word-width rt) (lshift 8 scale))
      "(aarch64-data-movement.lisp:str-reg) scale must match size of rt") 
  (store-word (+ rn 
     (if (= signed 1) 
       (signed-extend   (word-width rm) (lshift rm (* shift scale)))
       (unsigned-extend (word-width rm) (lshift rm (* shift scale))))) 
      rt))

; rm is an X register
(defun STRWroX  (rt rn rm option shift)
 (str-reg 2 rt rn rm option shift))

(defun STRXroX (rt rn rm option shift)
 (str-reg 3 rt rn rm option shift))

(defun STRBroX  (rt rn rm option shift)
 (str-reg 0 rt rn rm option shift))

(defun STRHroX  (rt rn rm option shift)
 (str-reg 1 rt rn rm option shift))

(defun STRSroX  (rt rn rm option shift)
 (str-reg 2 rt rn rm option shift))

(defun STRDroX (rt rn rm option shift)
  (str-reg 3 rt rn rm option shift))

(defun STRQroX (rt rn rm option shift)
  (str-reg 4 rt rn rm option shift))

; rm is a W register
(defun STRWroW  (rt rn rm option shift)
 (str-reg 2 rt rn rm option shift))

(defun STRXroW (rt rn rm option shift)
 (str-reg 3 rt rn rm option shift))

(defun STRBroW  (rt rn rm option shift)
 (str-reg 0 rt rn rm option shift))

(defun STRHroW  (rt rn rm option shift)
 (str-reg 1 rt rn rm option shift))

(defun STRSroW  (rt rn rm option shift)
 (str-reg 2 rt rn rm option shift))

(defun STRDroW (rt rn rm option shift)
  (str-reg 3 rt rn rm option shift))

(defun STRQroW (rt rn rm option shift)
  (str-reg 4 rt rn rm option shift))

; STRHHroX
(defun STRHHroX (rt rn rm option shift)
  (str-reg 0 rt rn rm option shift))

; STR (immediate) (base registers):
(defun str-post (xreg src off)
  "stores all of src to xreg, and post-indexes reg (reg += off)."
  (store-word xreg src)
  (set$ xreg (+ xreg off)))

(defun STRWpost (_ rt rn simm)
  (str-post rn rt simm))

(defun STRXpost (_ rt rn simm)
  (str-post rn rt simm))

; STR (SIMD registers)
(defun STRQpost (_ rt rn simm)
  (str-post rn rt simm))

(defun STRDpost (_ rt rn simm)
  (str-post rn rt simm))

(defun STRSpost (_ rt rn simm)
  (str-post rn (cast-low 32 rt) simm))

(defun STRHpost (_ rt rn simm)
  (str-post rn (cast-low 16 rt) simm))

(defun STRBpost (_ rt rn simm)
  (str-post rn (cast-low 8 rt) simm))

(defun STR*ui (scale src reg off) 
  "Stores a register of size (8 << scale) to the memory address 
  (reg + (off << scale))."
  (assert-msg (= (word-width src) (lshift 8 scale))
      "(aarch64-data-movement.lisp:STR*ui) scale must match size of register") 
  (store-word (+ reg (lshift off scale)) 
    (cast-unsigned (lshift 8 scale) src)))

(defun STRQui (src reg off)
  (STR*ui 4 src reg off))

(defun STRDui (src reg off)
  (STR*ui 3 src reg off))

(defun STRSui (src reg off)
  (STR*ui 2 src reg off))

(defun STRHui (src reg off)
  (STR*ui 1 src reg off))

(defun STRBui (src reg off)
  (STR*ui 0 src reg off))

(defun STRXui (src reg off)
  (STR*ui 3 src reg off))

(defun STRWui (src reg off)
  (STR*ui 2 src reg off))

; STRH (base reg), signed offset variant
(defun STRHHui (rt rn off)
  (store-word (+ rn off) (cast-low 16 rt)))

; STRB post-indexed
(defun STRBBpost (_ rt base simm)
  (store-byte base rt)
  (set$ base (+ base simm)))

(defun STRBBroW (rt rn rm option shift)
  (let ((off
    (if (= option 1)
        (signed-extend 32 rm)         ; SXTW
      (unsigned-extend 32 rm))))      ; UXTW
    (store-byte (+ rn off) rt)))

(defun STRBBroX (rt rn rm option shift)
  (let ((off 
    (if (= option 1)
        (signed-extend 64 rm)         ; SXTX
      (unsigned-extend 64 rm))))      ; LSL
    (store-byte (+ rn off) rt)))

; STP

(defun store-pair (scale indexing t1 t2 dst off) 
  "store the pair t1,t2 of size (8 << scale)at the register dst plus an offset, 
  using the specified indexing."
  (assert-msg (and (= (word-width t1) (lshift 8 scale)) 
      (= (word-width t2) (lshift 8 scale)))
      "(aarch64-data-movement.lisp) scale must match size of register ") 
  (let ((off (lshift off scale)) (datasize (lshift 8 scale))
      (addr (case indexing
              'post dst
              'pre  (+ dst off)
              'offset (+ dst off)
              (assert-msg (= 1 0) 
      "(aarch64-data-movement.lisp) invalid indexing scheme.")))
            )
    (store-word addr t1)
    (store-word (+ addr datasize) t2)
    (case indexing
       'post (set$ dst (+ addr off))
       'pre  (set$ dst addr)
       'offset )
    ))

; post-indexed
(defun STPWpost (_ t1 t2 dst off)
  (store-pair 2 'post t1 t2 dst off))

(defun STPXpost (_ t1 t2 dst off)
    (store-pair 3 'post t1 t2 dst off))

(defun STPSpost (_ t1 t2 dst off)
  (store-pair 2 'post t1 t2 dst off))

(defun STPDpost (_ t1 t2 dst off)
  (store-pair 3 'post t1 t2 dst off))

(defun STPQpost (_ t1 t2 dst off)
  (store-pair 4 'post t1 t2 dst off))

; pre-indexed
(defun STPXpre (_ t1 t2 dst off)
    (store-pair 3 'pre t1 t2 dst off))

(defun STPWpre (_ t1 t2 dst off)
  (store-pair 2 'pre t1 t2 dst off))

(defun STPSpre (_ t1 t2 dst off)
  (store-pair 2 'pre t1 t2 dst off))

(defun STPDpre (_ t1 t2 dst off)
  (store-pair 3 'pre t1 t2 dst off))

(defun STPQpre (_ t1 t2 dst off)
  (store-pair 4 'pre t1 t2 dst off))

; signed-offset
(defun STPWi (rt rt2 base imm) 
  (store-pair 2 'offset rt rt2 base imm))

(defun STPXi (rt rt2 base imm)
  (store-pair 3 'offset rt rt2 base imm))

(defun STPSi (rt rt2 base imm) 
  (store-pair 2 'offset rt rt2 base imm))

(defun STPDi (rt rt2 base imm) 
  (store-pair 3 'offset rt rt2 base imm))

(defun STPQi (rt rt2 base imm) 
  (store-pair 4 'offset rt rt2 base imm))


; addr + offset indexed STUR
(defmacro STUR*i (src base off size)
  "Takes `size` bits from src and stores at base + off"
  (store-word (+ base off) (cast-low size src)))

(defun STURXi  (src base off) (STUR*i src base off 64))

(defun STURWi  (src base off) (STUR*i src base off 32))

(defun STURHHi  (src base off) (STUR*i src base off 16))

(defun STURBBi (src base off) (STUR*i src base off 8))

(defun STURDi (rn rt imm) (STUR*i rn rt imm 64))

(defun STURQi (rn rt imm) (STUR*i rn rt imm 128)) 

