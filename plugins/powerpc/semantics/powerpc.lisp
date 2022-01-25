(declare (context (target powerpc)))

(defpackage powerpc (:use core target))
(defpackage llvm-powerpc32 (:use powerpc))


(defun NOP ()
  (empty))
