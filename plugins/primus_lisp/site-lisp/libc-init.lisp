;; for the invoke-procedure we need a type of the invoked function
;; so, the function must be present in the static representation of a program.
(require posix-init)
(in-package posix)
(declare (visibility :private))

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main"))
  (exit-with (invoke-subroutine main argc argv)))

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main")
           (context (abi "eabi")))
  (exit-with (invoke-subroutine
              (logand main 0xfffffffe) ; to handle thumb jumps
              argc argv)))


(defun setup-stack-canary ()
  (declare (context (abi "sysv")))
  (set FS_BASE (- brk 0x28))
  (memory-allocate brk (sizeof ptr_t))
  (write-word ptr_t brk 0xDEADBEEFBEAFDEAD)
  (+= brk (sizeof ptr_t)))

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main")
           (context (abi "sysv")))
  (setup-stack-canary)
  (exit-with (invoke-subroutine main argc argv)))

(defun init (args on-exit main)
  "bionic initialization function"
  (declare (external "__libc_init"))
  (invoke-subroutine main args (ptr+1 ptr_t args)))

(defun security-init-cookie ()
  "Windows CRT buffer overrun protection"
  (declare (external "__security_init_cookie")
           (context (abi "ms"))) ; actually we should overload by runtime
  0)

(defun init (argc argv ubpev auxvec fini stinfo stack_on_entry)
  (declare (external "__libc_start_main")
           (context (abi "ppc32")))
  (set R2 (+ stack_on_entry 0x7008))
  (let ((argc (read-word ptr_t stack_on_entry))
        (argv (ptr+1 ptr_t stack_on_entry))
        (main (read-word ptr_t (+ stinfo 4))))
    (invoke-subroutine main argc argv)))


(defun fini ()
  (declare (external "__libc_csu_fini"))
  (exit-with 0))

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main")
           (context (arch "mips")))
  (set T9 main)
  (set RA @__libc_csu_fini)
  (exit-with (invoke-subroutine main argc argv)))

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main")
           (context (arch "mips64") (abi "mips64")))
  (set T9 main)
  (set RA @__libc_csu_fini)
  (exit-with (invoke-subroutine main argc argv)))
