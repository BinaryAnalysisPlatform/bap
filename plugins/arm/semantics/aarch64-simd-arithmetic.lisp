(declare (context (target armv8-a+le)))

(in-package aarch64)

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
