(require posix)
(require types)

(declare (static errno-location))

(defmethod init ()
  (set errno-location brk)
  (+= brk (sizeof int)))

(defun errno-location ()
  (declare (external "__errno_location"))
  errno-location)
