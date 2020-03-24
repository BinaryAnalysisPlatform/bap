(require posix)
(require types)

(defparameter *errno-location* nil)

(defmethod init ()
  (set *errno-location* (malloc (sizeof int))))

(defun errno-location ()
  (declare (external "__errno_location"))
  *errno-location*)
