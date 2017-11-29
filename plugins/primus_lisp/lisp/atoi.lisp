(require ascii)

(defmacro skip-all (pred s)
  (while (pred (memory-read s)) (incr s)))

(defun atoi-prefix (s)
  (or (ascii-special s) (ascii-whitespace s)))

(defun atoi-read-digit (s)
  (coerce 0 (word-size) (- (memory-read s) '0')))

(defun read-ascii-word (s)
  (skip-all atoi-prefix s)
  (let ((v 0)
        (sign (ascii-sign (memory-read s))))
    (while (ascii-digit (memory-read s))
      (set v (+ (* v 10) (atoi-read-digit s)))
      (incr s))
    (* sign v)))

(defun atoi  (s) (c-int (read-ascii-word s)))
(defun atol  (s) (c-long (read-ascii-word s)))
(defun atoll (s) (c-long-long (read-ascii-word s)))
