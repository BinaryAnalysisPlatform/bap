;; for the invoke-procedure we need a type of the invoked function
;; so, the function must be present in the static representation of a program.

(require pointers)
(require memory)

(defun init (main argc argv)
  "GNU libc initialization stub"
  (declare (external "__libc_start_main"))
  (exit-with (invoke-subroutine main argc argv)))


;; in bionic init get only argv, so we need to
;; compute the number of arguments manually.
(defun init (args on-exit main)
  "bionic initialization function"
  (declare (external "__libc_init"))
  (let ((argc 0) (p args))
    (while (/= (points-to-null p))
      (incr argc)
      (ptr+1 p))
    (invoke-subroutine main argc args)))
