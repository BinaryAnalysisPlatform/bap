(declare (context (target armv8-a+le)))
(defpackage aarch64 (:use core target))
(defpackage llvm-aarch64 (:use aarch64))

(in-package aarch64)

(defun MOVZXi (dst imm shift)
  (set$ dst imm))

(defun ADDXri (dst src imm _shift)
  (set$ dst (+ src imm)))

(defun LDRXui (dst reg off)
  (set$ dst (load-word (+ reg off))))

(defun ADRP (dst imm)
  (set$ dst (+ (get-program-counter)
               (cast-signed (word-width) (lshift imm 12)))))


(defun STPXpre (dst t1 t2 _ off)
  (let ((word (/ (word-width) 8))
        (off (lshift off 3)))
    (store-word (+ dst off) t1)
    (store-word (+ dst off word) t2)
    (set$ dst (+ dst off))))


(defun BL (off)
  (let ((pc (get-program-counter)))
    (set LR (+ pc 4))
    (exec-addr (+ pc (lshift off 2)))))


(defun B (off)
  (let ((pc (get-program-counter)))
    (exec-addr (+ pc (lshift off 2)))))


(defun RET (dst)
  (exec-addr dst))
