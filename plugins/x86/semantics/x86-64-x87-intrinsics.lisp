(declare (context (target amd64) (bits 64)
                  (x86-floating-points intrinsic-semantics)))
(require x86-common)
(require x86-64)

(in-package x86-64)

(defun st-index (r)
  (case (symbol r)
    'ST0 0
    'ST1 1
    'ST2 2
    'ST3 3
    'ST4 4
    'ST5 5
    'ST6 6
    'ST7 7))

(defun x87-push (name x)
  (intrinsic (symbol-concat 'x87_push name :sep '_) x))

(defun LD_F80m (ptr sc ix off _) (x87-push 'f80 (load-bits 80 (ea ptr sc ix off))))
(defun LD_F64m (ptr sc ix off _) (x87-push 'f64 (load-bits 64 (ea ptr sc ix off))))
(defun LD_F32m (ptr sc ix off _) (x87-push 'f32 (load-bits 32 (ea ptr sc ix off))))
(defun ILD_F64m (ptr sc ix off _) (x87-push 'i64 (load-bits 64 (ea ptr sc ix off))))
(defun ILD_F32m (ptr sc ix off _) (x87-push 'i32 (load-bits 32 (ea ptr sc ix off))))
(defun ILD_F16m (ptr sc ix off _) (x87-push 'i16 (load-bits 16 (ea ptr sc ix off))))
(defun LD_F0 () (intrinsic 'x87_push_zero))
(defun LD_F1 () (intrinsic 'x87_push_one))
(defun LD_Frr (r) (intrinsic 'x87_push_st (st-index r)))

(defun x87-store (name sz ptr sc ix off)
  (intrinsic name :stores (ea ptr sc ix off) sz))

(defun x87-store-cw (name sz ptr sc ix off)
  (intrinsic name FPU_CONTROL :stores (ea ptr sc ix off) sz))

(defun ST_FP80m (ptr sc ix off _) (x87-store 'x87_pop_f80 80 ptr sc ix off))
(defun ST_FP64m (ptr sc ix off _) (x87-store 'x87_pop_f64 64 ptr sc ix off))
(defun ST_FP32m (ptr sc ix off _) (x87-store 'x87_pop_f32 32 ptr sc ix off))
(defun ST_F64m (ptr sc ix off _) (x87-store 'x87_store_f64 64 ptr sc ix off))
(defun ST_F32m (ptr sc ix off _) (x87-store 'x87_store_f32 32 ptr sc ix off))
(defun IST_FP64m (ptr sc ix off _) (x87-store-cw 'x87_pop_i64 64 ptr sc ix off))
(defun IST_FP32m (ptr sc ix off _) (x87-store-cw 'x87_pop_i32 32 ptr sc ix off))
(defun IST_FP16m (ptr sc ix off _) (x87-store-cw 'x87_pop_i16 16 ptr sc ix off))
(defun IST_F32m (ptr sc ix off _) (x87-store-cw 'x87_store_i32 32 ptr sc ix off))
(defun IST_F16m (ptr sc ix off _) (x87-store-cw 'x87_store_i16 16 ptr sc ix off))
(defun ISTT_FP64m (ptr sc ix off _) (x87-store 'x87_pop_trunc_i64 64 ptr sc ix off))
(defun ISTT_FP32m (ptr sc ix off _) (x87-store 'x87_pop_trunc_i32 32 ptr sc ix off))
(defun ISTT_FP16m (ptr sc ix off _) (x87-store 'x87_pop_trunc_i16 16 ptr sc ix off))
(defun ST_FPrr (r) (intrinsic 'x87_pop_st (st-index r)))
(defun ST_Frr (r) (intrinsic 'x87_store_st (st-index r)))

(defun x87-binary (op form r)
  (intrinsic (symbol-concat 'x87 op form :sep '_) (st-index r)))

(defun x87-binary-mem (op ty sz ptr sc ix off)
  (intrinsic (symbol-concat 'x87 op ty :sep '_)
             (load-bits sz (ea ptr sc ix off))))

(defun ADD_FST0r (r) (x87-binary 'fadd 'st0_sti r))
(defun ADD_FrST0 (r) (x87-binary 'fadd 'sti_st0 r))
(defun ADD_FPrST0 (r) (x87-binary 'faddp 'sti_st0 r))
(defun SUB_FST0r (r) (x87-binary 'fsub 'st0_sti r))
(defun SUB_FrST0 (r) (x87-binary 'fsub 'sti_st0 r))
(defun SUB_FPrST0 (r) (x87-binary 'fsubp 'sti_st0 r))
(defun SUBR_FST0r (r) (x87-binary 'fsubr 'st0_sti r))
(defun SUBR_FrST0 (r) (x87-binary 'fsubr 'sti_st0 r))
(defun SUBR_FPrST0 (r) (x87-binary 'fsubrp 'sti_st0 r))
(defun MUL_FST0r (r) (x87-binary 'fmul 'st0_sti r))
(defun MUL_FrST0 (r) (x87-binary 'fmul 'sti_st0 r))
(defun MUL_FPrST0 (r) (x87-binary 'fmulp 'sti_st0 r))
(defun DIV_FST0r (r) (x87-binary 'fdiv 'st0_sti r))
(defun DIV_FrST0 (r) (x87-binary 'fdiv 'sti_st0 r))
(defun DIV_FPrST0 (r) (x87-binary 'fdivp 'sti_st0 r))
(defun DIVR_FST0r (r) (x87-binary 'fdivr 'st0_sti r))
(defun DIVR_FrST0 (r) (x87-binary 'fdivr 'sti_st0 r))
(defun DIVR_FPrST0 (r) (x87-binary 'fdivrp 'sti_st0 r))

(defun ADD_F64m (ptr sc ix off _) (x87-binary-mem 'fadd 'f64 64 ptr sc ix off))
(defun ADD_F32m (ptr sc ix off _) (x87-binary-mem 'fadd 'f32 32 ptr sc ix off))
(defun ADD_FI32m (ptr sc ix off _) (x87-binary-mem 'fadd 'i32 32 ptr sc ix off))
(defun ADD_FI16m (ptr sc ix off _) (x87-binary-mem 'fadd 'i16 16 ptr sc ix off))
(defun SUB_F64m (ptr sc ix off _) (x87-binary-mem 'fsub 'f64 64 ptr sc ix off))
(defun SUB_F32m (ptr sc ix off _) (x87-binary-mem 'fsub 'f32 32 ptr sc ix off))
(defun SUB_FI32m (ptr sc ix off _) (x87-binary-mem 'fsub 'i32 32 ptr sc ix off))
(defun SUB_FI16m (ptr sc ix off _) (x87-binary-mem 'fsub 'i16 16 ptr sc ix off))
(defun SUBR_F64m (ptr sc ix off _) (x87-binary-mem 'fsubr 'f64 64 ptr sc ix off))
(defun SUBR_F32m (ptr sc ix off _) (x87-binary-mem 'fsubr 'f32 32 ptr sc ix off))
(defun SUBR_FI32m (ptr sc ix off _) (x87-binary-mem 'fsubr 'i32 32 ptr sc ix off))
(defun SUBR_FI16m (ptr sc ix off _) (x87-binary-mem 'fsubr 'i16 16 ptr sc ix off))
(defun MUL_F64m (ptr sc ix off _) (x87-binary-mem 'fmul 'f64 64 ptr sc ix off))
(defun MUL_F32m (ptr sc ix off _) (x87-binary-mem 'fmul 'f32 32 ptr sc ix off))
(defun MUL_FI32m (ptr sc ix off _) (x87-binary-mem 'fmul 'i32 32 ptr sc ix off))
(defun MUL_FI16m (ptr sc ix off _) (x87-binary-mem 'fmul 'i16 16 ptr sc ix off))
(defun DIV_F64m (ptr sc ix off _) (x87-binary-mem 'fdiv 'f64 64 ptr sc ix off))
(defun DIV_F32m (ptr sc ix off _) (x87-binary-mem 'fdiv 'f32 32 ptr sc ix off))
(defun DIV_FI32m (ptr sc ix off _) (x87-binary-mem 'fdiv 'i32 32 ptr sc ix off))
(defun DIV_FI16m (ptr sc ix off _) (x87-binary-mem 'fdiv 'i16 16 ptr sc ix off))
(defun DIVR_F64m (ptr sc ix off _) (x87-binary-mem 'fdivr 'f64 64 ptr sc ix off))
(defun DIVR_F32m (ptr sc ix off _) (x87-binary-mem 'fdivr 'f32 32 ptr sc ix off))
(defun DIVR_FI32m (ptr sc ix off _) (x87-binary-mem 'fdivr 'i32 32 ptr sc ix off))
(defun DIVR_FI16m (ptr sc ix off _) (x87-binary-mem 'fdivr 'i16 16 ptr sc ix off))

(defun CHS_F () (intrinsic 'x87_fchs))
(defun ABS_F () (intrinsic 'x87_fabs))
(defun SQRT_F () (intrinsic 'x87_fsqrt))
(defun FRNDINT () (intrinsic 'x87_frndint FPU_CONTROL))
(defun XCH_F (r) (intrinsic 'x87_fxch (st-index r)))

(defun x87-compare (name r)
  (intrinsic name (st-index r) :writes 'ZF :writes 'PF :writes 'CF)
  (set OF 0)
  (set SF 0)
  (set AF 0))

(defun COM_FIr (r) (x87-compare 'x87_fcomi r))
(defun COM_FIPr (r) (x87-compare 'x87_fcomip r))
(defun UCOM_FIr (r) (x87-compare 'x87_fucomi r))
(defun UCOM_FIPr (r) (x87-compare 'x87_fucomip r))

(defun FSTENVm (ptr sc ix off _)
  (intrinsic 'x87_fnstenv FPU_CONTROL
             :writes 'FPU_CONTROL
             :stores (ea ptr sc ix off) 224))

(defun FLDENVm (ptr sc ix off _)
  (intrinsic 'x87_fldenv (load-bits 224 (ea ptr sc ix off))
             :writes 'FPU_CONTROL))

(defun WAIT () (empty))
