//
//  format.h
//

#ifndef rv_format_h
#define rv_format_h

/* Instruction Formats */

extern "C" {
	extern const char* rv_fmt_none;
	extern const char* rv_fmt_rs1;
	extern const char* rv_fmt_offset;
	extern const char* rv_fmt_pred_succ;
	extern const char* rv_fmt_rs1_rs2;
	extern const char* rv_fmt_rd_imm;
	extern const char* rv_fmt_rd_offset;
	extern const char* rv_fmt_rd_rs1_rs2;
	extern const char* rv_fmt_frd_rs1;
	extern const char* rv_fmt_rd_frs1;
	extern const char* rv_fmt_rd_frs1_frs2;
	extern const char* rv_fmt_frd_frs1_frs2;
	extern const char* rv_fmt_rm_frd_frs1;
	extern const char* rv_fmt_rm_frd_rs1;
	extern const char* rv_fmt_rm_rd_frs1;
	extern const char* rv_fmt_rm_frd_frs1_frs2;
	extern const char* rv_fmt_rm_frd_frs1_frs2_frs3;
	extern const char* rv_fmt_rd_rs1_imm;
	extern const char* rv_fmt_rd_rs1_offset;
	extern const char* rv_fmt_rd_offset_rs1;
	extern const char* rv_fmt_frd_offset_rs1;
	extern const char* rv_fmt_rd_csr_rs1;
	extern const char* rv_fmt_rd_csr_zimm;
	extern const char* rv_fmt_rs2_offset_rs1;
	extern const char* rv_fmt_frs2_offset_rs1;
	extern const char* rv_fmt_rs1_rs2_offset;
	extern const char* rv_fmt_rs2_rs1_offset;
	extern const char* rv_fmt_aqrl_rd_rs2_rs1;
	extern const char* rv_fmt_aqrl_rd_rs1;
	extern const char* rv_fmt_rd;
	extern const char* rv_fmt_rd_zimm;
	extern const char* rv_fmt_rd_rs1;
	extern const char* rv_fmt_rd_rs2;
	extern const char* rv_fmt_rs1_offset;
	extern const char* rv_fmt_rs2_offset;
}

#endif
