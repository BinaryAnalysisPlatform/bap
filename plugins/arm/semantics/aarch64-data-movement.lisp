(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; LOADS, MOVES, STORES

;; LD...

;; LDR (register) 

(defmacro LDR*ro* (rt base index signed s scale setf mem-load)
  "(LDR*ro* rt base index signed s scale setf mem-load) loads a register from memory at the address calculated from a base register and optionally shifted and extended offset value. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (let ((shift (* s scale))
        (off (if (= signed 1)
                  (cast-signed 64 (lshift index shift))
               (cast-unsigned 64 (lshift index shift)))))
    (setf rt (mem-load (+ base off)))))

(defmacro LDRWro* (wt base index signed s) (LDR*ro* wt base index signed s 2 setw load-hword))
(defmacro LDRXro* (xt base index signed s) (LDR*ro* xt base index signed s 3 set$ load-word))

(defun LDRWroW (wt base index signed s) (LDRWro* wt base index signed s))
(defun LDRWroX (wt base index signed s) (LDRWro* wt base index signed s))
(defun LDRXroW (xt base index signed s) (LDRXro* xt base index signed s))
(defun LDRXroX (xt base index signed s) (LDRXro* xt base index signed s))

;; LDR (immediate, post-index)

(defmacro LDR*post (dst base off setf)
  (setf dst (mem-read base (/ (word-width dst) 8)))
  (set$ base (+ base (cast-signed 64 off))))

(defun LDRWpost (_ dst base off) (LDR*post dst base off setw))
(defun LDRXpost (_ dst base off) (LDR*post dst base off set$))

;; LDR (immediate, pre-index)

(defmacro LDR*pre (dst base off setf)
  (let ((address (+ base (cast-signed 64 off))))
    (setf dst (mem-read address (/ (word-width dst) 8)))
    (set$ base address))

(defun LDRWpre (_ dst base off) (LDR*pre dst base off setw))
(defun LDRXpre (_ dst base off) (LDR*pre dst base off set$))

;; LDR (immediate, unsigned offset)

(defmacro LDR*ui (dst reg off setf scale)
  (setf dst (mem-read (+ reg (lshift off scale)) (/ (word-width dst) 8))))

(defun LDRXui (dst reg off) (dst reg off set$ 3))
(defun LDRWui (dst reg off) (dst reg off setw 2))

;; LDRB (immediate, post-index)

(defun LDRBBpost (_ dst base simm)
  "(LDRBBpost _ dst base simm) loads a byte from the base address and stores it in the 32 bit dst register, and increments the base register by simm. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), ConstrainUnpredictable()"
  (setw dst (cast-unsigned 32 (load-byte base)))
  (set$ base (+ base simm)))

;; LDRB (immediate, pre-index)

(defun LDRBBpre (_ dst base simm)
  "(LDRBBpre _ dst base simm) loads a byte from the base address and an offset simm and stores it in the 32 bit dst register. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), ConstrainUnpredictable()"
  (setw dst (cast-unsigned 32 (load-byte (+ base simm)))))

;; LDRB (immediate, unsigned offset)

(defun LDRBBui (dst reg off)
  "(LDRBBui _ dst base simm) loads a byte from a preindexed base address and an unsigned offset and stores it in the 32 bit dst register. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), ConstrainUnpredictable()"
  (setw dst
        (cast-unsigned 32 (load-byte (+ reg off)))))

;; LDRB (register)

(defmacro LDRBBro* (dst base index signed)
  "(LDRBBro* dst base index signed) loads a byte from memory from a base address and index and stores it in a 32 bit destination register. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (let ((off (if (= signed 1)
                  (cast-signed 64 index)
                (cast-unsigned 64 index))))
    (setw dst (cast-unsigned 32 (load-byte (+ base off))))))

(defun LDRBBroW (dst base index signed _) (LDRBBro* dst base index signed))
(defun LDRBBroX (dst base index signed _) (LDRBBro* dst base index signed))

;; LDP (post-index)

(defun LDPXpost (dst r1 r2 base off)
  (let ((off (lshift off 3)))
    (set$ r1 (load-word base))
    (set$ r2 (load-word (+ base (sizeof word))))
    (set$ dst (+ dst off))))

;; LDP (signed offset)

(defmacro LDP*i (r1 r2 base imm scale datasize setf mem-load)
  "(LDP*i r1 r2 base imm scale datasize setf mem-load) loads a pair of registers r1 and r2 from the address calculated from a base register value and immediate offset. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (let ((off (lshift (cast-signed 64 imm) scale)))
    (setf r1 (mem-load (+ base off)))
    (setf r2 (mem-load (+ base off (/ datasize 8))))))

(defun LDPXi (r1 r2 base imm) (LDP*i r1 r2 base imm 3 64 set$ load-word))
(defun LDPWi (w1 w2 base imm) (LDP*i w1 w2 base imm 2 32 setw load-hword))

;; LDRH (register)

(defmacro LDRHHro* (wt base index signed s)
  "(LDRHHro* wt base index signed s) loads 2 bytes from the address calculated from a base register address and offset. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (let ((off (if (= signed 1)
                  (cast-signed 64 (lshift index s))
                (cast-unsigned 64 (lshift index s)))))
    (setw wt (load-dbyte (+ base off)))))

(defun LDRHHroX (wt xn xm extend s) (LDRHHro* wt xn xm extend s))
(defun LDRHHroW (wt xn wm extend s) (LDRHHro* wt xn wm extend s))

;; LDRH (immediate, unsigned offset)

(defun LDRHHui (wt xn pimm)
  "(LDRHHui wt xn pimm) loads 2 bytes from the address calculated from a base register and unsigned immediate offset. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (let ((off (lshift (cast-unsigned 64 pimm) 1)))
    (setw wt (load-dbyte (+ xn off)))))

;; LDRSW (immediate, unsigned offset)

(defun LDRSWui (dst base off)
  (set$ dst (cast-signed (word) (load-hword (+ base (lshift off 2))))))

;; LRDSW (register)

(defmacro LDRSWro* (xt base index signed s)
  "(LDRSWro* xt base index signed s) loads 32 bits from memory from a base address and offset and stores it in the destination register xt. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (let ((shift (* s 2))
        (off (if (= signed 1)
                  (cast-signed 64 (lshift index shift))
                (cast-unsigned 64 (lshift index shift)))))
    (set$ xt (load-hword (+ base off)))))

(defun LDRSWroX (xt base xm signed s) (LDRSWro* xt base xm signed s))
(defun LDRSWroW (xt base wm signed s) (LDRSWro* xt base wm signed s))

;; LDURB
  
(defun LDURBBi (wt base simm)
  "(LDURBBi wt base simm) loads a byte from the address calculated from a base register and signed immediate offset and stores it in the 32 bit destination register. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (setw wt (load-byte (+ base simm))))

;; LDUR

(defmacro LDUR*i (rt base simm setf mem-load)
  "(LDUR*i rt base simm setf mem-load) loads a register from the address calculated from a base register and signed immediate offset. NOTE: does not HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment()"
  (setf rt (mem-load (+ base (cast-signed 64 simm)))))

(defun LDURWi (wt base simm) (LDUR*i wt base simm setw load-hword))
(defun LDURXi (xt base simm) (LDUR*i xt base simm set$ load-word))

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

(defun STRBBui (src reg off)
  (store-byte (+ reg off) src))

(defun STPXpre (dst t1 t2 _ off)
  (let ((off (lshift off 3)))
    (store-word (+ dst off) t1)
    (store-word (+ dst off (sizeof word)) t2)
    (set$ dst (+ dst off))))

(defun STPXi (t1 t2 base off)
  (let ((off (lshift off 4)))
    (store-word base (+ base off))
    (store-word base (+ base off (sizeof word)))))

(defun STRXui (src reg off)
  (let ((off (lshift off 3)))
    (store-word (+ reg off) src)))

(defun STRWui (src reg off)
  (let ((off (lshift off 2)))
    (store-word (+ reg off) (cast-low 32 src))))

(defun STRXroX (rt rn rm _ shift)
  (store-word (+ rn (lshift rm (* shift 3))) rt))

(defmacro STUR*i (src base off size)
  "Takes `size` bits from src and stores at base + off"
  (store-word (+ base off) (cast-low size src)))

(defun STURXi  (src base off) (STUR*i src base off 64))

(defun STURWi  (src base off) (STUR*i src base off 32))

(defun STURHHi  (src base off) (STUR*i src base off 16))

(defun STURBBi (src base off) (STUR*i src base off 8))
