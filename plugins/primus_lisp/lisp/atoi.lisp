(require types)
(require ascii)

(defmacro skip-all (pred s)
  (while (pred (memory-read s)) (incr s)))

(defun atoi-prefix (s)
  (or (ascii-is-special s) (ascii-is-whitespace s)))

(defun atoi-read-digit (s)
  (cast ptr_t (- (memory-read s) ?0)))

(defun read-ascii-word (s)
  (skip-all atoi-prefix s)
  (let ((v 0)
        (sign (ascii-sign (memory-read s))))
    (while (ascii-is-digit (memory-read s))
      (set v (+ (* v 10) (atoi-read-digit s)))
      (incr s))
    (* sign v)))

(defmacro make-converter (type s)
  (cast type (read-ascii-word s)))

(defun atoi  (s) (make-converter int s))
(defun atol  (s) (make-converter long s))
(defun atoll (s) (make-converter long-long s))
