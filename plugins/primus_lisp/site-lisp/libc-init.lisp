;; for the invoke-procedure we need a type of the invoked function
;; so, the function must be present in the static representation of a program.
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

(defun riscv-reset-LR-to-prevent-infinite-loop (main argv base other)
  (declare (context (target riscv))
           (advice :before __libc_start_main)
           (visibility :private))
  (set X1 0))


(defun setup-thread-local-storage ()
  (declare (context (abi "sysv"))
           (global x86-64:FS_BASE))
  (let ((tcb-size (+ (* 6 (sizeof ptr_t))
                     (* 2 (sizeof int)))))
    (set FS_BASE brk)
    ;; tcbhead_t structure
    (memory-allocate brk tcb-size 0)
    (write-word ptr_t brk brk) ; tcb
    (write-word ptr_t (+ brk 0x28) 0xDEADBEEFBEAFDEAD) ; stack_guard
    ;; ptmalloc structure
    (memory-allocate (- brk 0x28) (sizeof ptr_t) 0) ; arena
    (memory-allocate (- brk 0x38) (sizeof ptr_t) 0) ; freelist
    ;; misc
    (memory-allocate (- brk 0x40) (sizeof int) 0) ; errno
    (+= brk tcb-size)))

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main")
           (context (target "amd64")
                    (abi "sysv")))
  (setup-thread-local-storage)
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
