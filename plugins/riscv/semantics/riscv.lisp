(declare (context (target riscv)))
(defpackage riscv (:use core target program))
(defpackage llvm-riscv64 (:use riscv riscv64))
(defpackage llvm-riscv32 (:use riscv riscv32))
(in-package riscv)

;;; Core Arithmetic

(defun ADDI (dst src off)
  (set$ dst (+ src off)))

(defun C_ADDI (dst src off)
  (set$ dst (+ src off)))

(defun add3 (dst src off)
  (declare (visibility :private))
  (set$ dst (cast-signed
             (word-width)
             (cast-low (/ (word-width) 2)
                       (+ src off)))))

(defun ADDIW (dst src off)
  (add3 dst src off))

(defun C_ADDIW (dst src off)
  (add3 dst src off))

(defun ADDW (dst src off)
  (add3 dst src off))

(defun C_ADDW (rd r1 r2)
  (add3 rd r1 r2))

(defun C_ADD (rd r1 r2)
  (set$ rd (+ r1 r2)))

(defun C_ADDI4SPN (dst src off)
  (set$ dst (+ src off)))

(defun C_ADDI16SP (dst src off)
  (set$ dst (+ src off)))

(defun SUB (rd r1 r2)
  (set$ rd (- r1 r2)))

(defun C_SUB (rd r1 r2)
  (set$ rd (- r1 r2)))


;;; Moves

(defun C_MV (dst src)
  (set$ dst src))

(defun C_LUI (dst imm)
  (set$ dst (lshift (cast-signed
                     (- (word-width) 12)
                     (cast-low 6 imm))
                    12)))

(defun AUIPC (dst off)
  (set$ dst (+ (get-program-counter) off)))

(defun LUI (dst imm)
  (set$ dst (lshift imm 12)))

(defun LI (dst imm)
  (set$ dst (cast-signed (word-width) imm)))

(defun C_LI (dst imm)
  (set$ dst (cast-signed (word-width) imm)))



;;; Memory operations
(defun LD (dst reg off)
  (set$ dst (core:load-word (+ reg off))))

(defun C_LD (dst reg off)
  (set$ dst (core:load-word (+ reg off))))

(defmacro load-word (cast part dst reg off)
  (set$ dst (cast (word-width)
                  (load-bits (/ (word-width) part) (+ reg off)))))

(defun LW (dst reg off)
  (load-word cast-signed 2 dst reg off))

(defun LH (dst reg off)
  (load-word cast-signed 4 dst reg off))

(defun LWU (dst reg off)
  (load-word cast-unsigned 2 dst reg off))

(defun LB (dst reg off)
  (set$ dst (cast-signed (word-width) (load-byte (+ reg off)))))

(defun LBU (dst reg off)
  (set$ dst (cast-unsigned (word-width) (load-byte (+ reg off)))))

(defun LHU (dst reg off)
  (load-word cast-unsigned 4 dst reg off))

(defun C_LDSP (dst reg off)
  (set$ dst (core:load-word (+ reg off))))

(defun C_SDSP (val sp imm)
  (store-word (+ sp imm) val))

(defun SDSP (val sp imm)
  (store-word (+ sp imm) val))

(defun SD (val reg imm)
  (store-word (+ reg imm) val))

(defun C_SD (val reg imm)
  (store-word (+ reg imm) val))

(defun SW (val reg imm)
  (store-word (+ reg imm) (cast-low (/ (word-width) 2) val)))

(defun SH (val reg imm)
  (store-word (+ reg imm) (cast-low (/ (word-width) 4) val)))

(defun SB (val reg imm)
  (store-byte (+ reg imm) val))

;;; Bitwise Operations

(defun ANDI (dst src off)
  (set$ dst (logand src off)))

(defun ORI (dst src off)
  (set$ dst (logor src off)))

(defun XORI (dst src off)
  (set$ dst (logxor src off)))

(defun SRLI (dst reg off)
  (set$ dst (rshift reg off)))

(defun C_SRLI (dst reg off)
  (set$ dst (rshift reg off)))

(defun C_SRAI (dst src imm)
  (set$ dst (arshift src imm)))

(defun SRAI (dst src imm)
  (set$ dst (arshift src imm)))

(defun SLLI (dst reg off)
  (set$ dst (lshift reg off)))

(defun C_SLLI (dst reg off)
  (set$ dst (lshift reg off)))

;;; Comparison
(defun SLTI (dst src off)
  (set$ dst (< dst src off)))

;;; Jumps
(defun JAL (lr off)
  (let ((pc (get-program-counter)))
    (set$ lr (+ pc 4))
    (exec-addr (+ pc off))))

(defun JALR (lr rs off)
  (let ((pc (get-program-counter)))
    (set$ lr (+ pc 4))
    (exec-addr (+ pc rs off))))

(defun C_JR (dst)
  (exec-addr dst))

(defun C_J (dst)
  (exec-addr (+ (get-program-counter) dst)))

(defun C_JALR (dst)
  (set X1 (+ (get-program-counter) 2))
  (exec-addr dst))

(defun conditional-jump (cmp off)
  (declare (visibility :private))
  (let ((pc (get-program-counter)))
    (when cmp
      (exec-addr (+ pc off)))))

(defun BEQ (rs1 rs2 off)
  (conditional-jump (= rs1 rs2) off))

(defun BLT (rs1 rs2 off)
  (conditional-jump (< rs1 rs2) off))

(defun BNE (rs1 rs2 off)
  (conditional-jump (/= rs1 rs2) off))

(defun C_BEQ (rs1 rs2 off)
  (conditional-jump (= rs1 rs2) off))

(defun C_BLT (rs1 rs2 off)
  (conditional-jump (< rs1 rs2) off))

(defun C_BNE (rs1 rs2 off)
  (conditional-jump (/= rs1 rs2) off))

(defun BEQZ (rs1 off)
  (conditional-jump (is-zero rs1) off))

(defun C_BEQZ (rs1 off)
  (conditional-jump (is-zero rs1) off))

(defun BNEZ (rs1 off)
  (conditional-jump (not (is-zero rs1)) off))

(defun C_BNEZ (rs1 off)
  (conditional-jump (not (is-zero rs1)) off))
