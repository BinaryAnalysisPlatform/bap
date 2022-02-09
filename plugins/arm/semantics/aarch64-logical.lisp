(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; LOGICAL/BITFIELD OPERATIONS

;; Logical

(defmacro ORN*rs (set rd rn rm is)
  (set rd (logor rn (lnot (lshift rm is)))))

(defun ORNWrs (rd rn rm is) (ORN*rs setw rd rn rm is))
(defun ORNXrs (rd rn rm is) (ORN*rs set$ rd rn rm is))

(defmacro log*rs (set op rd rn rm is)
  "(log*rs set op rd rn is) implements the logical operation (shift) instruction
   accepting either a W or X register. op is the binary logical operation."
  (set rd (op rn (shifted rm is))))

(defun ORRWrs (rd rn rm is) (log*rs setw logor  rd rn rm is))
(defun EORWrs (rd rn rm is) (log*rs setw logxor rd rn rm is))
(defun ANDWrs (rd rn rm is) (log*rs setw logand rd rn rm is))
(defun ORRXrs (rd rn rm is) (log*rs set$ logor  rd rn rm is))
(defun EORXrs (rd rn rm is) (log*rs set$ logxor rd rn rm is))
(defun ANDXrs (rd rn rm is) (log*rs set$ logand rd rn rm is))

(defmacro log*ri (set op rd rn imm)
  "(log*ri set op rd rn imm) implements the logical operation (immediate) instruction
   accepting either a W or X register. op is the binary logical operation."
  (set rd (op rn (immediate-from-bitmask imm))))

(defun ANDWri (rd rn imm) (log*ri setw logand rd rn imm))
(defun ANDXri (rd rn imm) (log*ri set$ logand rd rn imm))
(defun EORWri (rd rn imm) (log*ri setw logxor rd rn imm))
(defun EORXri (rd rn imm) (log*ri set$ logxor rd rn imm))
(defun ORRWri (rd rn imm) (log*ri setw logor rd rn imm))
(defun ORRXri (rd rn imm) (log*ri set$ logor rd rn imm))

;; UBFM and SBFM
;; (bitfield moves)

(defmacro make-BFM (set cast xd xr ir is)
  (let ((rs (word)))
    (if (< is ir)
        (if (and (/= is (- rs 1)) (= (+ is 1) ir))
            (set xd (lshift xr (- rs ir)))
            (set xd (lshift
                      (cast rs (extract is 0 xr))
                      (- rs ir))))
        (if (= is (- rs 1))
            (set xd (rshift xr ir))
            (set xd (cast rs (extract is ir xr)))))))

(defun UBFMXri (xd xr ir is)
  (make-BFM set$ cast-unsigned xd xr ir is))

(defun UBFMWri (xd xr ir is)
  (make-BFM setw cast-unsigned xd xr ir is))

(defun SBFMXri (xd xr ir is)
  (make-BFM set$ cast-signed xd xr ir is))

(defun SBFMWri (xd xr ir is)
  (make-BFM setw cast-signed xd xr ir is))