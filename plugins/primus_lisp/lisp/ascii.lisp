(defun ascii-special (s)
  "(ascii-special S) is true if S is an ascii special character"
  (< s 32))
(defun ascii-whitespace (s:8)
  "(ascii-whitespace S)is true if S is a whitespace"
  (or (= s 10)
      (= s 13)
      (= s 32)))

(defun ascii-sign (s:8)
  "(ascii-sign S) is 1 if S is + and -1 otherwise"
  (if (= s ?+) 1 -1))

(defun ascii-digit (s:8)
  "(ascii-digit s) is true if S is an ascii representation of decimal digit"
  (< (- s ?0) 10))
