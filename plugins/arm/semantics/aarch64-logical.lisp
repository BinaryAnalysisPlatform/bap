(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; LOGICAL/BITFIELD OPERATIONS

;; Logical

(defmacro ORN*rs (set rd rn rm is)
  "(ORN*rs set rd rn rm is) implements the OR NOT instruction
   accepting either a W or X register."
  (set rd (logor rn (lnot (lshift rm is)))))

(defun ORNWrs (rd rn rm is) (ORN*rs setw rd rn rm is))
(defun ORNXrs (rd rn rm is) (ORN*rs set$ rd rn rm is))

(defmacro log*rs (set op rd rn rm is)
  "(log*rs set op rd rn is) implements the logical operation (shift) instruction
   accepting either a W or X register. op is the binary logical operation."
  (set rd (op rn (shift-encoded rm is))))

(defun ORRWrs (rd rn rm is) (log*rs setw logor  rd rn rm is))
(defun EORWrs (rd rn rm is) (log*rs setw logxor rd rn rm is))
(defun ANDWrs (rd rn rm is) (log*rs setw logand rd rn rm is))
(defun ORRXrs (rd rn rm is) (log*rs set$ logor  rd rn rm is))
(defun EORXrs (rd rn rm is) (log*rs set$ logxor rd rn rm is))
(defun ANDXrs (rd rn rm is) (log*rs set$ logand rd rn rm is))

(defmacro log*ri (set op rd rn imm register-width)
  "(log*ri set op rd rn imm register-width) implements the logical operation (immediate) instruction
   accepting either a W or X register. op is the binary logical operation, and register-width
   is the width of the given registers."
  (set rd (op rn (immediate-from-bitmask imm register-width))))

(defun ANDWri (rd rn imm) (log*ri setw logand rd rn imm 32))
(defun ANDXri (rd rn imm) (log*ri set$ logand rd rn imm 64))
(defun EORWri (rd rn imm) (log*ri setw logxor rd rn imm 32))
(defun EORXri (rd rn imm) (log*ri set$ logxor rd rn imm 64))
(defun ORRWri (rd rn imm) (log*ri setw logor  rd rn imm 32))
(defun ORRXri (rd rn imm) (log*ri set$ logor  rd rn imm 64))

;; Logical ANDS (flags set)

(defmacro ANDS*r* (setf rd rn immOp)
  "(ANDS*r* set rd rn immOp) implements the logical AND operation on either an X or W register with immediate/shifted immediate and sets the N, V, Z, C flags based on the result."
  (let ((result (logand rn immOp)))
    (set NF (msb result))
    (set VF 0)
    (set ZF (is-zero result))
    (set CF 0)
  (setf rd result)))

(defmacro ANDS*ri (setf rd rn imm)
  "(ANDS*ri set rd rn imm) implements the logical AND operation on either an X or W register with immediate and sets the N, V, Z, C flags based on the result."
  (let ((immOp (immediate-from-bitmask imm)))
    (ANDS*r* setf rd rn immOp)))

(defun ANDSWri (rd rn imm) (ANDS*ri setw rd rn imm))
(defun ANDSXri (rd rn imm) (ANDS*ri set$ rd rn imm))

(defmacro ANDS*rs (setf rd rn rm is)
  "(ANDS*rs set rd rn imm) implements the logical AND operation on either an X or W register with shifted immediate and sets the N, V, Z, C flags based on the result."
  (let ((immOp (shifted rm is)))
    (ANDS*r* setf rd rn immOp)))

(defun ANDSWrs (rd rn rm is) (ANDS*rs setw rd rn rm is))
(defun ANDSXrs (rd rn rm is) (ANDS*rs set$ rd rn rm is))

;; ASRV
;; (bitfield moves)

(defmacro ASRV*r (setr datasize rd rn rm)
  (let ((shift (mod rm datasize)))
    (setr rd (arshift rn shift))))

(defun ASRVWr (rd rn rm) (ASRV*r setw 32 rd rn rm))
(defun ASRVXr (rd rn rm) (ASRV*r set$ 64 rd rn rm))

;; BIC

;; assumes immediate always provided... must fix...
(defmacro BIC*r (setr rd rn rm is)
  (let* ((shift (shifted rm is))
	 (comp (lnot shift)))
    (setr rd (logand rn comp))))

(defun BICWr (rd rn rm is) (BIC*r setw rd rn rm is))
(defun BICXr (rd rn rm is) (BIC*r set$ rd rn rm is))

;; UBFM and SBFM
;; (bitfield moves)

(defmacro make-BFM (set cast xd xr ir is)
  "(make-BFM set cast xd xr ir is) implements bitfield move instructions
   accepting either a W or X register, with cast being an unsigned or signed cast."
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
