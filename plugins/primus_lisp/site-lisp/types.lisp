(require posix-init)
(in-package posix)

(defun model-ilp32 (type)
  (case type
    'char 8
    'short 16
    'int 32
    'long 32
    'ptr 32))

(defun model-lp32 (type)
  (case type
    'char 8
    'short 16
    'int 16
    'long 32
    'ptr 32))

(defun model-ilp64 (type)
  (case type
    'char 8
    'short 16
    'int 64
    'long 64
    'ptr 64))

(defun model-llp64 (type)
  (case type
    'char 8
    'short 16
    'int 32
    'long 32
    'ptr 64))

(defun model-lp64 (type)
  (case type
    'char 8
    'short 16
    'int 32
    'long 64
    'ptr 64))


(defun char ()  (declare (context (abi eabi))) (model-ilp32 'char))
(defun short () (declare (context (abi eabi))) (model-ilp32 'short))
(defun int ()   (declare (context (abi eabi))) (model-ilp32 'int))
(defun long ()  (declare (context (abi eabi))) (model-ilp32 'long))
(defun ptr_t () (declare (context (abi eabi))) (model-ilp32 'ptr))

(defun char ()  (declare (context (abi mips32))) (model-ilp32 'char))
(defun short () (declare (context (abi mips32))) (model-ilp32 'short))
(defun int ()   (declare (context (abi mips32))) (model-ilp32 'int))
(defun long ()  (declare (context (abi mips32))) (model-ilp32 'long))
(defun ptr_t () (declare (context (abi mips32))) (model-ilp32 'ptr))

(defun char ()  (declare (context (abi mips64))) (model-ilp64 'char))
(defun short () (declare (context (abi mips64))) (model-ilp64 'short))
(defun int ()   (declare (context (abi mips64))) (model-ilp64 'int))
(defun long ()  (declare (context (abi mips64))) (model-ilp64 'long))
(defun ptr_t () (declare (context (abi mips64))) (model-ilp64 'ptr))

(defun char ()  (declare (context (abi ppc32))) (model-ilp32 'char))
(defun short () (declare (context (abi ppc32))) (model-ilp32 'short))
(defun int ()   (declare (context (abi ppc32))) (model-ilp32 'int))
(defun long ()  (declare (context (abi ppc32))) (model-ilp32 'long))
(defun ptr_t () (declare (context (abi ppc32))) (model-ilp32 'ptr))

(defun char ()  (declare (context (arch x86_64 sysv))) (model-lp64 'char))
(defun short () (declare (context (arch x86_64 sysv))) (model-lp64 'short))
(defun int ()   (declare (context (arch x86_64 sysv))) (model-lp64 'int))
(defun long ()  (declare (context (arch x86_64 sysv))) (model-lp64 'long))
(defun ptr_t () (declare (context (arch x86_64 sysv))) (model-lp64 'ptr))

(defun char ()  (declare (context (arch x86_64 ms))) (model-llp64 'char))
(defun short () (declare (context (arch x86_64 ms))) (model-llp64 'short))
(defun int ()   (declare (context (arch x86_64 ms))) (model-llp64 'int))
(defun long ()  (declare (context (arch x86_64 ms))) (model-llp64 'long))
(defun ptr_t () (declare (context (arch x86_64 ms))) (model-llp64 'ptr))

(defun char ()  (declare (context (arch x86))) (model-lp32 'char))
(defun short () (declare (context (arch x86))) (model-lp32 'short))
(defun int ()   (declare (context (arch x86))) (model-lp32 'int))
(defun long ()  (declare (context (arch x86))) (model-lp32 'long))
(defun ptr_t () (declare (context (arch x86))) (model-lp32 'ptr))

;; fallback, in case if we can't guess the ABI
(defun int () (word-width))
(defun long () (word-width))
(defun long-long () (word-width))
(defun char () 8)
(defun int32_t () 32)
(defun int64_t () 64)
(defun ptr_t () (word-width))
(defun double () 64)
(defun float () 32)
