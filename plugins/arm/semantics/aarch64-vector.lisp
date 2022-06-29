(declare (context (target armv8-a+le)))

(in-package aarch64)

;;; INS

(defun INSvi32gpr (vd redundant index gpr)
  "(INSvi32gpr vd ts index gpr) inserts an element in the general purpose register gpr into vecter register vd at index. NOTE: does not encode Security state & Exception level"
	(insert-element-into-vector vd index gpr 32))

(defun INSvi32lane (vd redundant index vn index2)
  "NOTE: does not encode Security state & Exception level"
 	(let ((element (get-vector-S-element index2 vn)))
		(insert-element-into-vector vd index element 32)))

;;; LDs..

;; opcde = 1000, rpt = 1, selem = 2
;; L = 1, MEMOP_LOAD
;; T = 16B, imm = #32, Q = 1, size = 00
;; datasize = 128, esize = 8
;; elements = 16
;; address = xn
;; offs = Zeroes(64)
;; pseudocode:

;; for r = 0 to r = 0
;; for e = 0 to e = 15
;; tt = (UInt(Rt) + 0) MOD 32 = a;   --- this is getting the vector reg from the instruction Rt field, which in this case will just be a
;; for s = 0 to s = 1
;; rval = V(tt) = _Z[tt]<127:0> = qa
;; Elem[rval, e, 8] = rval<(e+1)*size-1:e*size> = Mem[address+offs, 1, AccType_VEC] = load-byte (+ xn imm)
;; V[tt] = rval
;; offs = offs + 1
;; tt = (tt + 1) MOD 32 = (a + 1) MOD 32 = b;
;; if xn given, offs = X[n]
;; set if address/X reg given
(defun LD2Twov16b_POST (redundant qa_qb xn imm) 
  "(LD2Twov16b_POST redundant qa_qb xn imm) loads multiple 2-element structures from memory at address xn with offset imm and stores it in qa and qb with de-interleaving. NOTE: does not encode Security state & Exception level"
  (msg "$0" qa_qb))

(defun LDPQi (qn qm base imm)
	""
	(let ((off (lshift (cast-signed 128 imm) 4))
				(dbytes (/ 128 8)))
		(set$ qn (load-dword (+ base off)))
		(set$ qm (load-dword (+ base off dbytes)))))

(defun LDPSi (qn qm base imm)
	""
	(let ((off (lshift (cast-signed 32 imm) 4))
				(dbytes (/ 32 8)))
		(set$ qn (load-hword (+ base off)))
		(set$ qm (load-hword (+ base off dbytes)))))

(defun LDRDui (dt base imm)
	""
	(let ((off (lshift (cast-unsigned 64 imm) 3)))
		(set$ dt (load-word (+ base off)))))

(defun LDRQui (qt base imm)
	""
	(let ((off (lshift (cast-unsigned 64 imm) 4)))
		(set$ qt (load-dword (+ base off)))))

(defun LDRSui (st base imm)
	""
	(let ((off (lshift (cast-unsigned 64 imm) 2)))
		(set$ st (load-hword (+ base off)))))

(defun LDRQroX (qt base index signed shift)
	""
	(if (= signed 1)
			(let ((off (cast-signed 64 (lshift index shift))))
				(set$ qt (load-bits 16 (+ base off))))
		(let ((off (cast-signed 64 (lshift index shift))))
			(set$ qt (load-bits 16 (+ base off))))))

