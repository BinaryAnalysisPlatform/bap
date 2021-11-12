(in-package bap)

(defun funcstart (addr attrs)
  (promise-function-start addr))

(defun possiblefuncstart (addr attrs)
  (promise-function-start addr))

(defun setcontext (addr attrs)
  (let ((name (patterns-attribute attrs :name))
        (mode (patterns-attribute attrs :value)))
    (when (and name mode (= name 'TMode))
      (let ((lang (if (= mode '1) :T32 :A32)))
        (arm-set-encoding addr lang)))))
