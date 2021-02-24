(defun ascii-is-special (s)
  "(ascii-special S) is true if S is an ascii special character"
  (< s 32))

(defun ascii-is-whitespace (s)
  "(ascii-is-whitespace S) is true if S is \t, \n, \r, or SPACE"
  (or (= s 9)
      (= s 10)
      (= s 13)
      (= s 32)))

(defun ascii-sign (s)
  "(ascii-sign S) is 1 if S is +, -1 if it -, or 0 otherwise"
  (case s
    ?- -1
    ?+  1
    0))

(defun ascii-is-digit (s)
  "(ascii-is-digit s) is true if S is an ascii representation of decimal digit"
  (declare (external "isdigit"))
  (< (- s ?0) 10))

(defun ascii-is-alphanum (c)
  (declare (external "isalnum"))
  (or (ascii-is-alpha c)
      (ascii-is-digit c)))

(defun ascii-is-alpha (c)
  (declare (external "isalpha"))
  (< (- (logor c 32) ?a) 26))

(defun ascii-is-upper (c)
  (declare (external "isupper"))
  (< (- c ?A) 26))

(defun ascii-is-lower (c)
  (declare (external "islower"))
  (< (- c ?a) 26))

(defun ascii-to-lower (c)
  (declare (external "tolower"))
  (if (ascii-is-upper c) (logor c 32) c))


(defun ascii-to-upper (c)
  (declare (external "toupper"))
  (if (ascii-is-lower c) (logand c 0x5f) c))
