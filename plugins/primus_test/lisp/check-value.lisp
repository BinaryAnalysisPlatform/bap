;;; - CWE-476 (NULL pointer dereference)
;;; - CWE-690 (Unchecked Return Value to NULL Pointer Dereference)
;;; - CWE-252 (Unchecked Return Value)
;;;
;;; The analysis relies on the Taint Analysis Framework and ensures
;;; that a value, tracked by the module taints a condition of some
;;; jump. When a pointer is tracked, then the check must occur before
;;; the pointer is dereferenced.
;;;
;;; Interface
;;; =========
;;;
;;; - (check-value VALUE)
;;; - (check-value-before-dereference VALUE)
;;;
;;; Auxiliary Interface
;;; -------------------
;;; - (check-value-clear VALUE)


(require taint)

(defun check-value (v)
  (let ((pc (incident-location))
        (tid (taint-introduce-directly 'check-value v)))
    (dict-add 'check-value/required tid pc)))

(defun check-value-clear (v)
  (let ((taint (taint-get-direct 'check-value v))
        (pc (dict-get 'check-value/required taint)))
    (when taint
      (taint-sanitize-direct 'check-value v)
      (when pc
        (check-value/occured pc)
        (dict-del 'check-value/required taint)))))

;; Observed Signals

(defmethod loaded (addr value)
  (check-value/dereferenced addr value))

(defmethod stored (addr value)
  (check-value/dereferenced addr value))

(defmethod taint-finalize (taint live)
  (let ((pc (dict-get 'check-value/required taint)))
    (when pc
      (check-value/unchecked pc)
      (dict-del 'check-value/required taint))))

(defmethod enter-jmp (cnd dst)
  (check-value-clear cnd))


;; Private

(defun check-value/dereferenced (addr value)
  (let ((taint (taint-get-direct 'check-value addr))
        (pc (dict-get 'check-value/required taint)))
    (when pc
      (dict-del 'check-value/required taint)
      (taint-sanitize-indirect 'check-value addr)
      (taint-sanitize-direct 'check-value value)
      (check-value/unchecked-access pc))))


(defun check-value/occured (created)
  (incident-report 'value-was-checked created (incident-location)))

(defun check-value/unchecked (created)
  (incident-report 'value-was-not-checked-or-used created))

(defun check-value/unchecked-access (created)
  (incident-report 'value-was-used-before-check created (incident-location)))
