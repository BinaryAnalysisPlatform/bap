(declare (context (target arm armv8-a+le)))

(in-package aarch64)

;;; INTEGER ARITHMETIC OPERATIONS

(defmacro ADD*r* (set shift-function rd rn imm-or-rm off)
  "Implements ADD*ri and ADD*rs by specifying the shift function."
  (set rd (+ rn (shift-function imm-or-rm off))))

;; ADD*ri only uses lshift since the shift arg only zero-extends
;; and doesn't actually change from lshift
(defun ADDWri (rd rn imm off) (ADD*r* setw lshift rd rn imm off))
(defun ADDXri (rd rn imm off) (ADD*r* set$ lshift rd rn imm off))
;; shift-encoded decodes the shift type and shifts
(defun ADDWrs (rd rn rm off) (ADD*r* setw shift-encoded rd rn rm off))
(defun ADDXrs (rd rn rm off) (ADD*r* set$ shift-encoded rd rn rm off))

; adds immediate
(defun ADDSXri (rd rn imm off) 
  (add-with-carry set$ rd rn (lshift imm off) 0))

(defun ADDSWri (rd rn imm off) 
  (add-with-carry setw rd rn (lshift imm off) 0))

; adds shifted
(defun ADDSXrs (rd rn rm shift) 
  (add-with-carry set$ rd rn (shift-encoded rm shift) 0))

(defun ADDSWrs (rd rn rm shift) 
  (add-with-carry set$ rd rn (shift-encoded rm shift) 0))

; add extended
(defun ADDXrx (rd rn rm shift) 
  (set$ rd (+ rn (extended rm shift))))

(defun ADDWrx (rd rn rm shift) 
  (setw rd (+ rn (extended rm shift))))

; add extend SXRX|UXTX
(defun ADDXrx64 (rd rn rm shift) 
  (set$ rd (+ rn (extended rm shift))))

; endTODO 


(defun ADRP (dst imm)
  (set$ dst (+
             (logand (get-program-counter) (lshift -1 12))
             (cast-signed (word) (lshift imm 12)))))

(defmacro SUB*r* (set shift-function rd rn imm-or-rm off)
  "Implements SUB*ri and SUB*rs by specifying the shift function."
  (set rd (cast-low (word-width rd) (- rn (shift-function imm-or-rm off)))))

;; see ADD*ri vs ADD*rs
(defun SUBWri (rd rn rm off) (SUB*r* setw lshift rd rn rm off))
(defun SUBXri (rd rn rm off) (SUB*r* set$ lshift rd rn rm off))
(defun SUBWrs (rd rn rm off) (SUB*r* setw shift-encoded rd rn rm off))
(defun SUBXrs (rd rn rm off) (SUB*r* set$ shift-encoded rd rn rm off))

(defun SUBXrx (rd rn rm off)
  (set$ rd (- rn (extended rm off))))

(defun SUBXrx64 (rd rn rm off)
  (set$ rd (- rn (extended rm off))))

(defun SUBXrw (rd rn rm off)
  (setw rd (- rn (extended rm off))))

(defun SUBSWrs (rd rn rm off)
  (add-with-carry/clear-base rd rn (lnot (shift-encoded rm off)) 1))

(defun SUBSXrx (rd rn rm off)
  (add-with-carry set$ rd rn (lnot (shift-encoded rm off)) 1))

(defun SUBSXrx64 (rd rn rm off)
  (add-with-carry set$ rd rn (lnot (shift-encoded rm off)) 1))

(defun SUBSXrs (rd rn rm off)
  (add-with-carry set$ rd rn (lnot (shift-encoded rm off)) 1))

(defun SUBSWri (rd rn imm off)
  (add-with-carry/clear-base rd rn (lnot (lshift imm off)) 1))

(defun SUBSXri (rd rn imm off)
  (add-with-carry set$ rd rn (lnot (lshift imm off)) 1))

(defmacro Mop*rrr (set op rd rn rm ra)
  "(Mop*rrr set op rd rn rm ra) implements multiply-add, multiply-subtract
   etc with W or X registers. op is the binary operation used after *."
  (set rd (op ra (* rn rm))))

(defun MADDWrrr (rd rn rm ra) (Mop*rrr setw + rd rn rm ra))
(defun MADDXrrr (rd rn rm ra) (Mop*rrr set$ + rd rn rm ra))
(defun MSUBWrrr (rd rn rm ra) (Mop*rrr setw - rd rn rm ra))
(defun MSUBXrrr (rd rn rm ra) (Mop*rrr set$ - rd rn rm ra))

(defmacro *DIV*r (set div rd rn rm)
  "(*DIV*r set div rd rn rm) implements the SDIV or UDIV instructions
   on W or X registers, with div set to s/ or / respectively."
  (if (= rm 0)
    (set rd 0)
    (set rd (div rn rm))))

(defun SDIVWr (rd rn rm) (*DIV*r setw s/ rd rn rm))
(defun SDIVXr (rd rn rm) (*DIV*r set$ s/ rd rn rm))
(defun UDIVWr (rd rn rm) (*DIV*r setw /  rd rn rm))
(defun UDIVXr (rd rn rm) (*DIV*r set$ /  rd rn rm))

(defun ADR (rd label) 
  (store-word rd (+ (get-program-counter) (cast-signed 64 label))))
