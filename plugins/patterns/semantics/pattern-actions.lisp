(in-package bap)

(defmethod bap:patterns-action (action addr attrs)
  (when (is-in action 'funcstart 'possiblefuncstart)
    (promise-function-start addr)))

(defmethod bap:patterns-action (action addr attrs)
  (when (= action 'setcontext)
    (let ((name (patterns-attribute attrs :name))
          (mode (patterns-attribute attrs :value)))
      (when (and name mode (= name 'TMode))
        (let ((lang (if (= mode '1) :T32 :A32)))
          (arm-set-encoding addr lang))))))
