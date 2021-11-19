(in-package bap)

(defmethod bap:patterns-action (action addr attrs)
  (when (is-in action 'funcstart 'possiblefuncstart)
    (promise-function-start addr)))
