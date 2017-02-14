;; for the invoke-procedure we need a type of the invoked function
;; so, the function must be present in the static representation of a program.

(defun libc-start-main (main argc argv)
  (declare (external "__libc_start_main"))
  (exit-with (invoke-subroutine main argc argv)))
