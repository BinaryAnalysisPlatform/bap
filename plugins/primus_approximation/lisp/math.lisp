(defun sin (x)
  (declare (external "sin"))
  (approximate 'sin 64 x))

(defun cos (x)
  (declare (external "cos"))
  (approximate 'cos 64 x))

(defun log (x)
  (declare (external "log"))
  (approximate 'log 64 x))
