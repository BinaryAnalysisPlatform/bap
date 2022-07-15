(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; INS

(defun INSvi32gpr (vd _ index gpr)
  "(INSvi32gpr vd ts index gpr) inserts an element in the general purpose register gpr into vecter register vd at index. NOTE: does not encode Security state & Exception level"
  (insert-element-into-vector vd index gpr 32))

(defun INSvi32lane (vd _ index vn index2)
  "NOTE: does not encode Security state & Exception level"
  (let ((element (get-vector-S-element index2 vn)))
    (insert-element-into-vector vd index element 32)))

;;; LDs..

;; LD2 (multiple structures, no offset)

;;(defmacro LD2Twov16b* (qa_qb xn) 
;;  "(LD2Twov16b_POST redundant qa_qb xn imm) loads multiple 2-element structures from memory at address xn with offset imm and stores it in qa and qb with de-interleaving. NOTE: does not encode Security state & Exception level"
;;  (let ((qa (nth-reg-in-group qa_qb 0))
;;        (qb (nth-reg-in-group qa_qb 1)))
;;    (insert-a qa qb xn 0)))

;;(defun LD2Twov8b (da_db xn) ())
;;(defun LD2Twov16b (qa_qb xn) (LD2Twov16b* qa_qb xn))
;;(defun LD2Twov4h (da_db xn) ())
;;(defun LD2Twov8h (qa_qb xn) ())
;;(defun LD2Twov2s (da_db xn) ())
;;(defun LD2Twov4s (qa_qb xn) ())
;;(defun LD2Twov2d (qa_qb xn) ())

;; LD2 (multiple structures, post index)

(defun LD2Twov16b_POST (_ qa_qb xn xm)
  "(LD2Twov16b_POST _ qa_qb xn imm) loads multiple 2-element structures from memory at address xn with offset imm and stores it in qa and qb with de-interleaving. NOTE: does not encode Security state & Exception level"
  (let ((qa (nth-reg-in-group qa_qb 0))
        (qb (nth-reg-in-group qa_qb 1)))
    (insert-a qa qb xn 0 0 0)
    (set$ xn (+ xn xm))))

(defun insert-a (qa qb addr e acc-a acc-b)
  (if (< e 16)
    (let ((temp (load-byte addr)))
      (insert-b qa qb (+ addr 1) e (if (= e 0) temp (concat temp acc-a)) acc-b))
    (prog
      (set$ qa acc-a)
      (set$ qb acc-b))))

(defun insert-b (qa qb addr e acc-a acc-b)
  (let ((temp (load-byte addr)))
    (insert-a qa qb (+ addr 1) (+ e 1) acc-a (if (= e 0) temp (concat temp acc-b)))))

;; LD1 (multiple structures)

;;(defun LD1Twov16b_POST (_ qa_qb xn xm) 
;;  "(LD2Twov16b_POST redundant qa_qb xn imm) loads multiple 2-element structures from memory at address xn with offset imm and stores it in qa and qb with de-interleaving. NOTE: does not encode Security state & Exception level"
;;  (let ((qa (nth-reg-in-group qa_qb 0))
;;        (qb (nth-reg-in-group qa_qb 1)))
;;    (insert-a qa qb xn 0)
;;    (set$ xn (+ xn xm))))

;; LDP (signed offset)

(defmacro LDP.i (vn vm base imm size scale)
  "(LDP.i qn qm imm size mem-load scale) loads a pair of SIMD&FP registers from memory using the address base and an optional signed immediate offset. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (let ((off (lshift (cast-signed 64 imm) scale))
        (dbytes (/ size 8)))
    (set$ vn (mem-read (+ base off) (/ size 8)))
    (set$ vm (mem-read (+ base off dbytes) (/ size 8)))))

(defun LDPQi (qn qm base imm) (LDP.i qn qm base imm 128 4))
(defun LDPDi (qn qm base imm) (LDP.i qn qm base imm 64 3))
(defun LDPSi (qn qm base imm) (LDP.i qn qm base imm 32 2))

;; LDR (immediate, unsigned offset)

(defmacro LDR.ui (vt base imm size scale)
  "(LDR.ui vt base imm mem-load scale) loads an element from memory from the base address and unsigned immediate offset imm and stores the result in vt. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (let ((off (lshift (cast-unsigned 64 imm) scale)))
    (set$ vt (mem-read (+ base off) (/ size 8)))))

(defun LDRBui (bt base imm) (LDR.ui bt base imm 8 0))
(defun LDRHui (ht base imm) (LDR.ui ht base imm 16 1))
(defun LDRSui (st base imm) (LDR.ui st base imm 32 2))
(defun LDRDui (dt base imm) (LDR.ui dt base imm 64 3))
(defun LDRQui (qt base imm) (LDR.ui qt base imm 128 4))

;; LDR (register)

(defmacro LDR.roX (vt base index signed s scale size)
  "(LDR.roX vt base index signed s scale mem-load) loads a SIMD&FP register from address base and an optionally shifted and extended index. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (let ((shift (if (= s 1)
                   (+ scale 0)
                 (+ 0 0)))
        (off (if (= signed 1)
                 (cast-signed 64 (lshift index shift))
               (cast-unsigned 64 (lshift index shift)))))
    (set$ vt (mem-read (+ base off) (/ size 8)))))

(defun LDRBroX (bt base index signed s) (LDR.roX bt base index signed s 0 8))
(defun LDRHroX (ht base index signed s) (LDR.roX ht base index signed s 1 16))
(defun LDRSroX (st base index signed s) (LDR.roX st base index signed s 2 32))
(defun LDRDroX (dt base index signed s) (LDR.roX dt base index signed s 3 64))
(defun LDRQroX (qt base index signed s) (LDR.roX qt base index signed s 4 128))

;; LDUR

(defmacro LDUR.i (vt base simm size)
  "(LDUR.i vt base simm mem-load) loads a SIMD&FP register from memory at the address calculated from a base register and optional immediate offset. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (set$ vt (mem-read (+ base simm) (/ size 8))))

(defun LDURBi (bt base simm) (LDUR.i bt base simm 8))
(defun LDURHi (ht base simm) (LDUR.i ht base simm 16))
(defun LDURSi (st base simm) (LDUR.i st base simm 32))
(defun LDURDi (dt base simm) (LDUR.i dt base simm 64))
(defun LDURQi (qt base simm) (LDUR.i qt base simm 128))
