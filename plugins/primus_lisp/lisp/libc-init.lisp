;; for the invoke-procedure we need a type of the invoked function
;; so, the function must be present in the static representation of a program.

(require pointers)
(require memory)

(defun init (main argc argv auxv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main"))
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


;; Although CRT statically adds its stuff to each binary we will stub it,
;; since CRT uses segmented memory model to access TLS data, and the this
;; model is not currently supported by our lifter. Until we add a support
;; at least partial, we need to bypass this function.


(defun init (argc argv ubpev auxvec fini stinfo stack_on_entry)
  ""
  (declare (external "__libc_start_main")
           (context (abi "ppc32")))
  (set R2 (+ stack_on_entry 0x7008))
  (let ((argc (read-word int32_t stack_on_entry))
        (argv (ptr+1 int32_t stack_on_entry)))
    (invoke-subroutine (read-word int32_t (+ stinfo 4)) argc argv)))
