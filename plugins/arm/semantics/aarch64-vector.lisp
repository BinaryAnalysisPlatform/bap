(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; ARITHMETIC

(defun sym-to-binop (binop-sym x y)
  (case binop-sym
    'add (+ x y)
    'sub (- x y)
    'mul (* x y)))

(defun vector-binop (sym ecount esize vn vm)
  "(vector-binop sym ecount esize vn vm e) returns the result
   of applying the binary operation specified by sym (see sym-to-binop)
   to each of the elements in vn and vm. For example, with addition,
      Elem[vn, ecount-1, esize] + Elem[vm, ecount-1, esize]
      concat
      ...
      concat
      Elem[vn, 0, esize] + Elem[vm, 0, esize]
   ecount and esize are the number and size of the elements."
  (vector-binop/helper sym ecount esize vn vm 0))

(defun vector-binop/helper (sym ecount esize vn vm e)
  ;; i can't make this a macro and take in the binop as
  ;; a function, because when i try, BAP gets a stack overflow ._.
  (if (>= e (-1 ecount))
    (sym-to-binop sym (extract-elem vn e esize) (extract-elem vm e esize))
    (concat
      (vector-binop/helper sym ecount esize vn vm (+1 e))
      (sym-to-binop sym (extract-elem vn e esize) (extract-elem vm e esize )))))

(defun ADDv*i* (vd vn vm ecount esize)
  (set$ vd (vector-binop 'add ecount esize vn vm)))

(defun ADDv1i64 (vd vn vm) (ADDv*i* vd vn vm 1  64))
(defun ADDv2i64 (vd vn vm) (ADDv*i* vd vn vm 2  64))
(defun ADDv2i32 (vd vn vm) (ADDv*i* vd vn vm 2  32))
(defun ADDv4i32 (vd vn vm) (ADDv*i* vd vn vm 4  32))
(defun ADDv4i16 (vd vn vm) (ADDv*i* vd vn vm 4  16))
(defun ADDv8i16 (vd vn vm) (ADDv*i* vd vn vm 8  16))
(defun ADDv8i8  (vd vn vm) (ADDv*i* vd vn vm 8  8))
(defun ADDv16i8 (vd vn vm) (ADDv*i* vd vn vm 16 8))

(defun SUBv*i* (vd vn vm ecount esize)
  (set$ vd (vector-binop 'sub ecount esize vn vm)))

(defun SUBv1i64 (vd vn vm) (SUBv*i* vd vn vm 1  64))
(defun SUBv2i64 (vd vn vm) (SUBv*i* vd vn vm 2  64))
(defun SUBv2i32 (vd vn vm) (SUBv*i* vd vn vm 2  32))
(defun SUBv4i32 (vd vn vm) (SUBv*i* vd vn vm 4  32))
(defun SUBv4i16 (vd vn vm) (SUBv*i* vd vn vm 4  16))
(defun SUBv8i16 (vd vn vm) (SUBv*i* vd vn vm 8  16))
(defun SUBv8i8  (vd vn vm) (SUBv*i* vd vn vm 8  8))
(defun SUBv16i8 (vd vn vm) (SUBv*i* vd vn vm 16 8))

(defun MULv*i* (vd vn vm ecount esize)
  (set$ vd (vector-binop 'mul ecount esize vn vm)))

(defun MULv1i64 (vd vn vm) (MULv*i* vd vn vm 1  64))
(defun MULv2i64 (vd vn vm) (MULv*i* vd vn vm 2  64))
(defun MULv2i32 (vd vn vm) (MULv*i* vd vn vm 2  32))
(defun MULv4i32 (vd vn vm) (MULv*i* vd vn vm 4  32))
(defun MULv4i16 (vd vn vm) (MULv*i* vd vn vm 4  16))
(defun MULv8i16 (vd vn vm) (MULv*i* vd vn vm 8  16))
(defun MULv8i8  (vd vn vm) (MULv*i* vd vn vm 8  8))
(defun MULv16i8 (vd vn vm) (MULv*i* vd vn vm 16 8))

;;; LOGICAL

(defun ANDv8i8  (vd vn vm) (set$ vd (logand vn vm)))
(defun ANDv16i8 (vd vn vm) (set$ vd (logand vn vm)))

;; the ISA expresses (logxor vn vm) as
;; (logxor vm (logand (logor (zeros (word-width vn)) vn) (ones (word-width vn))))
;; I've simplified it to just this.
(defun EORv8i8  (vd vn vm) (set$ vd (logxor vn vm)))
(defun EORv16i8 (vd vn vm) (set$ vd (logxor vn vm)))

;; the ISA says NOT acts element-wise, but this is
;; equivalent to just (lnot vn). Not sure why it does this.
(defun NOTv8i8  (vd vn)    (set$ vd (lnot vn)))
(defun NOTv16i8 (vd vn)    (set$ vd (lnot vn)))

(defun ORRv8i8  (vd vn vm) (set$ vd (logor  vn vm)))
(defun ORRv16i8 (vd vn vm) (set$ vd (logor  vn vm)))

(defun ORNv8i8  (vd vn vm) (set$ vd (logor  vn (lnot vm))))
(defun ORNv16i8 (vd vn vm) (set$ vd (logor  vn (lnot vm))))

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
