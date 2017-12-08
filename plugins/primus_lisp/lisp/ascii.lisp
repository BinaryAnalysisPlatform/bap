(defun ascii-special (s) (< s 32))
(defun ascii-whitespace (s:8)
  (or (= s 10)
      (= s 13)
      (= s 32)))

(defun ascii-sign (s:8) (if (= s '+') 1 -1))
(defun ascii-digit (s:8) (< (- s '0') 10))
