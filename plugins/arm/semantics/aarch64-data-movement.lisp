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

; STRB (base)
(defun STRBBui (src reg off)
  (store-byte (+ reg off) src))

; STR (register) (base):
; doesn't account for signed/unsigned  extension
(defun str-reg (rt rn rm signed shift)
  (assert (< signed 2))
  (store-word (+ rn 
     (if (= signed 1) 
       (cast-signed 64 (lshift rm shift))
       (cast-unsigned 64 (lshift rm shift)))) 
      rt))

; option encodes the extend type which is not relevant here
(defun STRWroX  (rt rn rm option shift)
 (str-reg rt rn rm option (* shift 2)))

(defun STRXroX (rt rn rm option shift)
 (str-reg rt rn rm option (* shift 3)))

(defun STRQroX (rt rn rm option shift)
  (str-reg rt rn rm option (* shift 4)))

; STRHHroX
(defun STRHHroX (rt rn rm option shift)
  (str-reg rt rn rm option shift))

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
  (str-post rn (extract 31 0 rt) simm))

(defun STRHpost (_ rt rn simm)
  (str-post rn (extract 15 0 rt) simm))


; STRQui
; STRDui

; STRH (base reg)
; STRHHui


; post-indexed STRB
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
  (let ((off (signed-extend 64 rm)))  ; SXTX
    (store-byte (+ rn off) rt)))

(defun STPXpre (dst t1 t2 _ off)
  (let ((off (lshift off 3)))
    (store-word (+ dst off) t1)
    (store-word (+ dst off (sizeof word)) t2)
    (set$ dst (+ dst off))))

(defun STPWi (rt rt2 base imm) 
  (let ((datasize 16) (off (* imm 4)))
    (store-word (+ base off) rt)
    (store-word (+ base off datasize) rt2)))

(defun STPXi (t1 t2 base off)
  (let ((off (* off 8)))
    (store-word (+ base off) t1)
    (store-word (+ base off (sizeof word)) t2)))

; signed offset STP (SIMD/FP)
(defun STPQi (rt rt2 base imm) 
  (let ((datasize 128) (off (* imm 16)))
    (store-word (+ base off) rt)
    (store-word (+ base off datasize) rt2)))

(defun STRXui (src reg off)
  (let ((off (lshift off 3)))
    (store-word (+ reg off) src)))

(defun STRWui (src reg off)
  (let ((off (lshift off 2)))
    (store-word (+ reg off) (cast-low 32 src))))

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

