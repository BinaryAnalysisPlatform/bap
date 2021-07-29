(in-package posix)

(defun bindtextdomain (_ dir)
  (declare (external "bindtextdomain"))
  dir)

(defun textdomain (dom)
  (declare (external "textdomain"))
  dom)
