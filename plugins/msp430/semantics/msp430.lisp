(declare (context (target msp430)))
(defpackage msp430 (:use core target program))
(defpackage llvm-msp430 (:use msp430))
(in-package msp430)

;;; Core Arithmetic

(defun is-set (x)
  (declare (visibility :private))
  (not (is-zero x)))

(defun ADC (dst)
  (set$ dst (+ dst C)))
;x x x x

(defun ADD (dst src)
  (set$ dst (+ src dst)))
;x x x x

(defun ADDC (dst src)
  (set$ dst (+ C (+ src dst))))
;x x x x

(defun AND (dst src)
  (set$ dst (logand src dst)))
;0 x x x

(defun BIC (dst src)
  (set$ dst (logand (not src) dst)))

;(*BIS*)
;(*BIT*)
;0 x x x

;(*BR*)
;(*CALL*)

(defun CLR (dst)
  (set$ dst 0))

(defun CLRC (dst)
  (set C 0)) 

(defun CLRN (dst)
  (set N 0)) 

(defun CLRZ (dst)
  (set Z 0))  

(defun CMP (dst src)
  (set V (- dst src))) 
;xxxx
;0x38: 0c 93 
; tst r12 ; 
(defun llvm-msp430:CMP16rc (dst src)
  (set V (- dst src)))

;(defun DADC(.B) dst dst + C → dst (decimal) xxxx

;(defun DADD(.B) dst src src + dst + C → dst (decimal) xxxx
(defun DEC (dst) 
  (set dst (- 1 dst)))
;   x x x x

(defun DECD (dst) 
  (set dst (- 2 dst))) 
;  x x x x

;(defun DINT () 
;  (special "DINT"))
;(defun EINT () 
;  (special "EINT"))

(defun INC (dst) 
  (set dst (+ 1 dst)))

(defun INCD (dst) 
  (set dst (+ 2 dst)))
; x x x x

;(defun INV(.B) dst Invert destination xxxx)

(defun conditional-jump (cmp off)
  (declare (visibility :private))
  (let ((pc (get-program-counter)))
    (when cmp
      (exec-addr (+ pc off)))))

(defun JC (label) 
  (conditional-jump (is-set C) label))


;0x3a: f6 23 ; jne $-18 ; 
;(defun llvm-msp430:JCC 
;-0xa 0x1


(defun JHS (label) (JC label))

(defun JZ (label) 
  (conditional-jump (is-set Z) label))

(defun JEQ (label) (JZ label))

(defun JGE (label) 
  (conditional-jump (is-zero (logxor N V)) label))

(defun JL (label) 
  (conditional-jump (is-zero (logxor N V)) label))

(defun JMP (label) 
  (exec-addr label))

(defun JN (label) 
  (conditional-jump (is-set N) label))

(defun JNC (label) 
  (conditional-jump (is-zero C) label))

(defun JLO (label) (JNC label))

(defun JNE (label)
  (conditional-jump (is-zero Z) label))

(defun JNZ (label) (JNE label))

(defun MOV (src dst)
 (set$ dst src))

;0x34: 1c 41 00 00 
;mov off(reg), dst 
(defun llvm-msp430:MOV16rm (dst reg off)
  (set$ dst (core:load-word (+ reg off))))

;0x2e: 81 4c 00 00 
;mov reg, off(dst) 
;(llvm-msp430:MOV16mr SP 0x0 R12)
(defun llvm-msp430:MOV16mr (dst off reg)
  (store-word (+ dst off) reg))


;0x4: b2 40 80 5a 00 00 
;mov #23168, &0 
;(defun llvm-msp430:MOV16mi (ERROR addr val)
;  (store-word addr val))

;0x20: b1 40 10 27 00 00 
;mov #10000, 0(r1)

;(defmacro is-error (sym) (= sym "ERROR"))
(defun llvm-msp430:MOV16mi (reg off val)
;  (if (is-error reg)
;    (store-word off val)
    (store-word (+ reg off) val))
;)

;0x16: 5c 42 00 00 ; mov.b &0, r12 ; (llvm-msp430:MOV8rm R12B ERROR 0x0)
;0xa: 5c 42 00 00 ; mov.b &0, r12 ; (llvm-msp430:MOV8rm R12B ERROR 0x0)
;0x1c: c2 4c 00 00 ; mov.b r12, &0 ; (llvm-msp430:MOV8mr ERROR 0x0 R12B)
;0x10: c2 4c 00 00 ; mov.b r12, &0 ; (llvm-msp430:MOV8mr ERROR 0x0 R12B)



(defun NOP ()
  (empty))

;(defun POP (dst) (set$ sp (+ 2 sp))
;Item from stack, SP+2 → SP)
;(defun PUSH (src) SP - 2 → SP, src → @SP - - - -


;(defun RETI Return from interrupt xxxx
;TOS → SR, SP + 2 → SP
;TOS → PC, SP + 2 → SZP
;(defun RET Return from subroutine ----
;TOS → PC, SP + 2 → SP
;(defun RLA(.B) dst Rotate left arithmetically xxxx
;(defun RLC(.B) dst Rotate left through carry xxxx
;(defun RRA(.B) dst MSB → MSB ....LSB → C 0xxx
;(defun RRC(.B) dst C → MSB .........LSB → C xxxx
;(defun SBC(.B) dst Subtract carry from destination xxxx

(defun SETC ()
  (set C 1))

(defun SETN ()
  (set N 1))

(defun SETZ ()
  (set Z 1))

(defun SUB (src dst) 
  (set dst (+ dst (+ (not src) 1))));   x x x x

(defun SUBC (src dst) 
  (set dst (+ dst (+ (not src) C))));   x x x x

;(defun SWPB dst swap bytes ----
;(defun SXT dst Bit7 → Bit8 ........ Bit15 0 x x x
;(defun TST(.B) dst Test destination xxxx

(defun XOR (src dst) 
  (declare (visibility :private))
  (set dst (logxor src dst))); x x x x

; xor.b #X, rY
; llvm-msp430:XOR8rc
;(defun XOR8rc (dst _ imm)  
;  (set Z 1))






;0x2c: 3c 53 ; add #-1, r12 ; (llvm-msp430:ADD16rc R12 R12 -0x1)
;0xe: 6c d2 ; bis.b #4, r12 ; (llvm-msp430:BIS8rc R12B R12B 0x4)

;0x0: 31 80 02 00 ; sub #2, r1 ; (llvm-msp430:SUB16ri SP SP 0x2)
