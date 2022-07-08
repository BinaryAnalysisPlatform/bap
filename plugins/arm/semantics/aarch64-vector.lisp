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


(defun MOVI* (datasize channelsize vd val shift)
  "Sets every channel of vd to have value. the size of val should be equal to
  the channel width."
  (let ((val (cast-low channelsize (lshift val shift)))
        (result (replicate-to-fill val datasize)))
    (set$ vd result)))

(defun MOVIv8b_ns (vd imm)
  (MOVI* 64 8 vd imm 0))

(defun MOVIv16b_ns (vd imm)
  (MOVI* 128 8 vd imm 0))

(defun MOVIv4i16 (vd imm shift)
  (MOVI* 64 16 vd imm shift))

(defun MOVIv8i16 (vd imm shift)
  (MOVI* 128 16 vd imm shift))

(defun MOVIv2i32 (vd imm shift)
  (MOVI* 64 32 vd imm shift))

(defun MOVIv4i32 (vd imm shift)
  (MOVI* 128 32 vd imm shift))

;;; LDs..

(defun LD2Twov16b_POST (_ qa_qb xn xm) 
  "(LD2Twov16b_POST _ qa_qb xn imm) loads multiple 2-element structures from memory at address xn with offset imm and stores it in qa and qb with de-interleaving. NOTE: does not encode Security state & Exception level"
  (let ((qa (get-first-128b-reg qa_qb))
        (qb (get-second-128b-reg qa_qb)))
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

(defmacro LDPvec*i (vn vm base imm size mem-load scale)
  "(LDP*i qn qm imm size mem-load scale) loads a pair of SIMD&FP registers from memory using the address base and an optional signed immediate offset. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (let ((off (lshift (cast-signed 64 imm) scale))
        (dbytes (/ size 8)))
    (set$ vn (mem-load (+ base off)))
    (set$ vm (mem-load (+ base off dbytes)))))

(defun LDPQi (qn qm base imm) (LDPvec*i qn qm base imm 128 load-dword 4))
(defun LDPDi (qn qm base imm) (LDPvec*i qn qm base imm 64 load-dword 3))
(defun LDPSi (qn qm base imm) (LDPvec*i qn qm base imm 32 load-hword 2))

(defmacro LDR*ui (vt base imm mem-load scale)
  "(LDR*ui vt base imm mem-load scale) loads an element from memory from the base address and unsigned immediate offset imm and stores the result in vt. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (let ((off (lshift (cast-unsigned 64 imm) scale)))
    (set$ vt (mem-load (+ base off)))))

(defun LDRBui (bt base imm) (LDR*ui bt base imm load-byte 0))
(defun LDRHui (ht base imm) (LDR*ui ht base imm load-dbyte 1))
(defun LDRSui (st base imm) (LDR*ui st base imm load-hword 2))
(defun LDRDui (dt base imm) (LDR*ui dt base imm load-word 3))
(defun LDRQui (qt base imm) (LDR*ui qt base imm load-dword 4))

(defmacro LDR*roX (vt base index signed s scale mem-load)
  "(LDR*roX vt base index signed s scale mem-load) loads a SIMD&FP register from address base and an optionally shifted and extended index. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (let ((shift (if (= s 1)
          (+ scale 0)
          (+ 0 0)))
        (off (if (= signed 1)
          (cast-signed 64 (lshift index shift))
          (cast-unsigned 64 (lshift index shift)))))
    (set$ vt (mem-load (+ base off)))))

(defun LDRBroX (bt base index signed s) (LDR*roX bt base index signed s 0 load-byte))
(defun LDRHroX (ht base index signed s) (LDR*roX ht base index signed s 1 load-dbyte))
(defun LDRSroX (st base index signed s) (LDR*roX st base index signed s 2 load-hword))
(defun LDRDroX (dt base index signed s) (LDR*roX dt base index signed s 3 load-word))
(defun LDRQroX (qt base index signed s) (LDR*roX qt base index signed s 4 load-dword))

(defmacro LDURvec*i (vt base simm mem-load)
  "(LDUR*i vt base simm mem-load) loads a SIMD&FP register from memory at the address calculated from a base register and optional immediate offset. NOTE: does not CheckFPAdvSIMDEnabled64(), HaveMTE2Ext(), SetTagCheckedInstruction(), CheckSPAlignment(), Mem[... AccType_VEC]"
  (set$ vt (mem-load (+ base simm))))

(defun LDURBi (bt base simm) (LDURvec*i bt base simm load-byte))
(defun LDURHi (ht base simm) (LDURvec*i ht base simm load-dbyte))
(defun LDURSi (st base simm) (LDURvec*i st base simm load-hword))
(defun LDURDi (dt base simm) (LDURvec*i dt base simm load-word))
(defun LDURQi (qt base simm) (LDURvec*i qt base simm load-dword))
