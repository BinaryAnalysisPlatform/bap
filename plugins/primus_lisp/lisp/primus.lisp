(defun handle-unresolved-names ()
  (declare (external "__primus_linker_unresolved_call"))
  (msg "skipping unknown function $(get-current-program-counter)"))
