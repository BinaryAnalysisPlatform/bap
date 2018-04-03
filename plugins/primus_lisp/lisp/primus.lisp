(defun handle-unresolved-names ()
  "(handle-unresolved-names) emits a diagnostic message when called"
  (declare (external "__primus_linker_unresolved_call"))
  (msg "skipping a jump to an unknown destination at $0" (get-current-program-counter)))
