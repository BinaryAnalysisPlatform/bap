(declare (context (target i386) (bits 32)))

(require x86-common)
(in-package x86-32)

(defun pop (dst)
  (set$ dst (load-word ESP))
  (+= ESP 4))

(defun POPA32 ()
  (pop 'EDI)
  (pop 'ESI)
  (pop 'EBP)
  (+= ESP 4)
  (pop 'EBX)
  (pop 'EDX)
  (pop 'ECX)
  (pop 'EAX))
