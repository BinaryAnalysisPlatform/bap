(defun ascii-special (s) (< s 32))
(defun ascii-whitespace (s) (= s 10 13 32))
(defun ascii-sign (s) (if (= s ?+) 1 -1))
(defun ascii-digit (s) (< (- s ?0) 10))
