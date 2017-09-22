(defun handle-unresolved-names ()
  (declare (external "__primus_linker_unresolved_call"))
  (msg "skipping a jump to an unknown destination at $(get-current-program-counter)"))
