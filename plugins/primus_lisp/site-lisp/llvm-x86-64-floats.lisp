(require posix)
(require types)

(defmacro lower-all (r) (set r false))
(defmacro lower-all (r rs) (prog (set r false) (lower-all rs)))

(defun ymm_t () 256)
(defun xmm_t () 128)

(defmacro sse-unpack (t n r)
  "(sse-unpack T N R) unpacks the Nth word of type T from the register R"
  (extract (-1 (* (+1 n) (t))) (* n (t)) r))

(defmacro sse-pack-low (tr tv r v)
  (concat (extract (-1 (tr)) (tv) r) (sse-unpack tv 0 v)))

(defun reg-val (reg)
  (case (reg-name reg)
    'RIP (+ (get-current-program-counter) 8)
    'RSP RSP
    'RBP RBP
    (error "unknown register")))

(defmacro read-xmm (t op)
  (case (reg-name op)
    'XMM0 (sse-unpack t 0 YMM0)
    'XMM1 (sse-unpack t 0 YMM1)
    'XMM2 (sse-unpack t 0 YMM2)
    'XMM3 (sse-unpack t 0 YMM3)
    'XMM4 (sse-unpack t 0 YMM4)
    'XMM5 (sse-unpack t 0 YMM5)
    'XMM6 (sse-unpack t 0 YMM6)
    'XMM7 (sse-unpack t 0 YMM7)
    'XMM8 (sse-unpack t 0 YMM8)
    'XMM9 (sse-unpack t 0 YMM9)
    'XMM10 (sse-unpack t 0 YMM10)
    'XMM11 (sse-unpack t 0 YMM11)
    'XMM12 (sse-unpack t 0 YMM12)
    'XMM13 (sse-unpack t 0 YMM13)
    'XMM14 (sse-unpack t 0 YMM14)
    'XMM15 (sse-unpack t 0 YMM15)
    (error "unknown xmm register")))

(defun xmm-parent (reg)
  (case (reg-name reg)
    'XMM0 'YMM0
    'XMM1 'YMM1
    'XMM2 'YMM2
    'XMM3 'YMM3
    'XMM4 'YMM4
    'XMM5 'YMM5
    'XMM6 'YMM6
    'XMM7 'YMM7
    'XMM8 'YMM8
    'XMM9 'YMM9
    'XMM10 'YMM10
    'XMM11 'YMM11
    'XMM12 'YMM12
    'XMM13 'YMM13
    'XMM14 'YMM14
    'XMM15 'YMM15))

(defmacro write-xmm (type dst val)
  (let ((p (xmm-parent dst)))
    (set-symbol-value p (sse-pack-low ymm_t type p val))))


(defmacro fadd-scalar/rm (type)
  (let ((dest insn:op1)
        (src1 (read-xmm type insn:op2))
        (src2 (read-word type (+ (reg-val insn:op3) insn:op6))))
    (write-xmm type dest (ieee754-add (type) src1 src2))
    (exec-addr insn:next_address)))

(defmacro fdiv-scalar/rr (type)
  (let ((dest insn:op1)
        (src1 (read-xmm type insn:op2))
        (src2 (read-xmm type insn:op3)))
    (write-xmm type dest (ieee754-div (type) src1 src2))
    (exec-addr insn:next_address)))

(defmacro fcmp-scalar/rm (type)
  (let ((x (read-xmm type insn:op1))
        (y (read-word type (+ (reg-val insn:op2) insn:op5))))
    (lower-all SF AF OF ZF CF PF)
    (set ZF (ieee754-eq (type) x y))
    (set CF (ieee754-lt (type) x y))
    (exec-addr insn:next_address)))

(defun fadd-scalar-double/rm ()
  (declare (external llvm-x86_64:ADDSDrm llvm-x86_64:ADDSDrm_Int))
  (fadd-scalar/rm double))

(defun fdiv-scalar-double/rr ()
  (declare (external llvm-x86_64:DIVSDrr llvm-x86_64:DIVSDrr_Int))
  (fdiv-scalar/rr double))

(defun fcmp-scalar-double/rm ()
  (declare (external llvm-x86_64:UCOMISDrm))
  (fcmp-scalar/rm double))
