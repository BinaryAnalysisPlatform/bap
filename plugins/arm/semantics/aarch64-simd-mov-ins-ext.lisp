(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; INS

(defun INSvi32gpr (vd _ index gpr)
  "(INSvi32gpr vd ts index gpr) inserts an element in the general purpose register gpr
   into vecter register vd at index.
   NOTE: does not encode Security state & Exception level"
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

; EXT

(defmacro EXTv* (datasize vd vn vm pos) 
  "Extracts a vector from a pair of vectors. pos is the bit offset that will 
  become the least significant bit of vd."
  (let ((pos (lshift pos 3)))
    (set$ vd (extract (+ pos (- datasize 1)) pos (concat vm vn)))))

(defun EXTv16i8 (vd vn vm pos) 
  (EXTv* 128 vd vn vm pos))

(defun EXTv8i8 (vd vn vm pos) 
  (EXTv* 64 vd vn vm pos))
