(declare (context (target arm-family)
                  (bits 64)))

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
  "(ANDS*r* set rd rn immOp) implements the logical AND operation on either an X or W register
   with immediate/shifted immediate and sets the N, V, Z, C flags based on the result."
  (let ((result (logand rn immOp)))
    (set-nzcv-after-logic-op result)
  (setf rd result)))

(defmacro ANDS*ri (setf size rd rn imm)
  "(ANDS*ri set rd rn imm) implements the logical AND operation on either an X or W register
   with immediate and sets the N, V, Z, C flags based on the result."
  (let ((immOp (immediate-from-bitmask imm size)))
    (ANDS*r* setf rd rn immOp)))

(defun ANDSWri (rd rn imm) (ANDS*ri setw 32 rd rn imm))
(defun ANDSXri (rd rn imm) (ANDS*ri set$ 64 rd rn imm))

(defmacro ANDS*rs (setf rd rn rm is)
  "(ANDS*rs set rd rn imm) implements the logical AND operation on either an X or W register
   with shifted immediate and sets the N, V, Z, C flags based on the result."
  (let ((immOp (shift-encoded rm is)))
    (ANDS*r* setf rd rn immOp)))

(defun ANDSWrs (rd rn rm is) (ANDS*rs setw rd rn rm is))
(defun ANDSXrs (rd rn rm is) (ANDS*rs set$ rd rn rm is))


;; BIC

(defmacro BIC*rs (setr rd rn rm is)
  "(BIC*r setr rd rn rm) stores the result of a logical and of rn with the complement of
   the contents of optionally shifted rm in rd"
  (let ((shift (shift-encoded rm is)) 
        (comp (lnot shift)))
    (setr rd (logand rn comp))))

(defun BICWrs (rd rn rm is) (BIC*rs setw rd rn rm is))
(defun BICXrs (rd rn rm is) (BIC*rs set$ rd rn rm is))

(defmacro BICS*rs (setr rd rn rm is)
  "(BICS*r setr rd rn rm) sets appropriate flags and stores the result of a logical and of rn
   with the complement of the contents of optionally shifted rm in rd"
  (let ((shift (shift-encoded rm is)) 
        (comp (lnot shift)) 
        (result (logand rn comp)))
    (set-nzcv-after-logic-op result)
    (setr rd result)))

(defun BICSWrs (rd rn rm is) (BICS*rs setw rd rn rm is))
(defun BICSXrs (rd rn rm is) (BICS*rs set$ rd rn rm is))

;; REV...

(defmacro REVn*r (setr container-size rd rn)
  "(REVn*r setr container-size rd rn) implements the non-vector REV#
   instructions with the given container-size."
  (setr rd (reverse-in-containers container-size 8 rn)))

(defun REVWr   (rd rn) (REVn*r setw 32 rd rn))
(defun REVXr   (rd rn) (REVn*r set$ 64 rd rn))
(defun REV16Xr (rd rn) (REVn*r setw 16 rd rn))
(defun REV16Wr (rd rn) (REVn*r set$ 16 rd rn))
(defun REV32Xr (rd rn) (REVn*r setw 32 rd rn))

;; UBFM and SBFM
;; (bitfield moves)

(defmacro make-eBFM (set cast xd xr ir is)
  "(make-BFM  cast xd xr ir is) implements bitfield move instructions
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
  (make-eBFM set$ cast-unsigned xd xr ir is))

(defun UBFMWri (xd xr ir is)
  (make-eBFM setw cast-unsigned xd xr ir is))

(defun SBFMXri (xd xr ir is)
  (make-eBFM set$ cast-signed xd xr ir is))

(defun SBFMWri (xd xr ir is)
  (make-eBFM setw cast-signed xd xr ir is))

(defmacro BFM (set rd rn r s)
  (if (>= s r)
      (set rd (concat (cast-high (+1 (- s r)) rd)
                      (if (= r 0)
                          (cast-low (+1 s) rn)
                        (extract s r rn))))
    (set rd (concat
             (cast-low (+1 s) rn)
             (cast-low r rd)))))

(defun BFMXri (_ xd xr ir is)
  (BFM set$ xd xr ir is))

(defun BFMWri (_ xd xr ir is)
  (BFM setw xd xr ir is))

;; Shifts

(defmacro SHIFT*r (setr shift datasize rd rn rm)
  "(ASRV*r setr datasize rd rn rm) does an arithmetic shift right and stores it 
  in the destination register rd"
    (setr rd (cast-low datasize (shift rn  (mod rm datasize)))))

(defun ASRVXr (rd rn rm) (SHIFT*r set$ arshift 64 rd rn rm))
(defun ASRVWr (rd rn rm) (SHIFT*r setw arshift 32 rd rn rm))

(defun LSRVXr (rd rn rm) (SHIFT*r set$ rshift 64 rd rn rm))
(defun LSRVWr (rd rn rm) (SHIFT*r setw rshift 32 rd rn rm))
(defun LSLVXr (rd rn rm) (SHIFT*r set$ lshift 64 rd rn rm))
(defun LSLVWr (rd rn rm) (SHIFT*r setw lshift 32 rd rn rm))

(defun RORVXr (rd rn rm) (SHIFT*r set$ rotate-right 64 rd rn rm))
(defun RORVWr (rd rn rm) (SHIFT*r setw rotate-right 32 rd rn rm))

(defun RBITXr (rd rn) (set$ rd (reverse-bits rn)))
(defun RBITWr (rd rn) (setw rd (reverse-bits rn)))