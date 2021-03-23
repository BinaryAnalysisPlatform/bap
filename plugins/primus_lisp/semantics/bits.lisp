(in-package core)

(defun msb (x)
  "(msb X) is the most significant bit of X."
  (select (- (word-width) 1) x))

(defun lsb (x)
  "(msb X) is the least significant bit of X."
  (select 0 x))

(defun carry (rd rn rm)
  "(carry RD RN RM) is true if the sum RD = RN + RM causes
   the carry out of the msb bit."
  (logor (logand (msb rn) (msb rm))
         (logand (msb rm) (lnot (msb rd)))
         (logand (msb rn) (lnot (msb rd)))))


(defun overflow (rd rn rm)
  "(overflow RD RN RM) is true if the sum RD = RN + RM results
   in two's complement overflow."
  (logor (logand (msb rn) (msb rm) (lnot (msb rd)))
         (logand (lnot (msb rn)) (lnot (msb rm)) (msb rd))))
