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
