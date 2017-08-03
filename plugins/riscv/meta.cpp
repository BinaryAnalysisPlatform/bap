//
//  meta.cc
//
//  DANGER - This is machine generated code
//

#include "types.h"
#include "format.h"
#include "meta.h"

const rv_primitive_data rv_type_primitives[] = {
	{ "none", "none" },
	{ "sx", "%ld" , "%lx" },
	{ "ux", "%lu" , "%lx" },
	{ "s8", "%hhd" , "%hhx" },
	{ "u8", "%hhu" , "%hhx" },
	{ "s16", "%hd" , "%hx" },
	{ "u16", "%hu" , "%hx" },
	{ "s32", "%d" , "%x" },
	{ "u32", "%u" , "%x" },
	{ "s64", "%lld" , "%llx" },
	{ "u64", "%llu" , "%llx" },
	{ "s128", "none" , "none" },
	{ "u128", "none" , "none" },
	{ "f32", "%.9e" , "%.9e" },
	{ "f64", "%.17e" , "%.17e" },
	{ "f128", "none" , "none" },
};

const rvc_constraint rvcc_c_addi4spn[] = {
	rvc_imm_10,
	rvc_imm_x4,
	rvc_imm_nz,
	rvc_rd_b3,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_fld[] = {
	rvc_imm_8,
	rvc_imm_x8,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_lw[] = {
	rvc_imm_7,
	rvc_imm_x4,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_flw[] = {
	rvc_imm_7,
	rvc_imm_x4,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_fsd[] = {
	rvc_imm_8,
	rvc_imm_x8,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_sw[] = {
	rvc_imm_7,
	rvc_imm_x4,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_fsw[] = {
	rvc_imm_7,
	rvc_imm_x4,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_nop[] = {
	rvc_rd_eq_x0,
	rvc_rs1_eq_x0,
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_c_addi[] = {
	rvc_simm_6,
	rvc_rd_ne_x0,
	rvc_rd_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_c_jal[] = {
	rvc_imm_12,
	rvc_imm_x2,
	rvc_rd_eq_ra,
	rvc_end
};

const rvc_constraint rvcc_c_li[] = {
	rvc_imm_6,
	rvc_rd_ne_x0,
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_c_addi16sp[] = {
	rvc_imm_10,
	rvc_imm_x4,
	rvc_imm_nz,
	rvc_rd_eq_sp,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_lui[] = {
	rvc_imm_18,
	rvc_imm_nz,
	rvc_rd_ne_x0_x2,
	rvc_end
};

const rvc_constraint rvcc_c_srli_rv32c[] = {
	rvc_imm_nz,
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_srai_rv32c[] = {
	rvc_imm_nz,
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_andi[] = {
	rvc_imm_nz,
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_sub[] = {
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_xor[] = {
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_or[] = {
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_and[] = {
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_subw[] = {
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_addw[] = {
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_j[] = {
	rvc_imm_12,
	rvc_imm_x2,
	rvc_rd_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_c_beqz[] = {
	rvc_imm_9,
	rvc_imm_x2,
	rvc_rs1_b3,
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_c_bnez[] = {
	rvc_imm_9,
	rvc_imm_x2,
	rvc_rs1_b3,
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_c_slli_rv32c[] = {
	rvc_imm_nz,
	rvc_rd_ne_x0,
	rvc_rd_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_c_fldsp[] = {
	rvc_imm_9,
	rvc_imm_x8,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_lwsp[] = {
	rvc_imm_8,
	rvc_imm_x4,
	rvc_rd_ne_x0,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_flwsp[] = {
	rvc_imm_8,
	rvc_imm_x4,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_jr[] = {
	rvc_rd_eq_x0,
	rvc_rs1_ne_x0,
	rvc_end
};

const rvc_constraint rvcc_c_mv[] = {
	rvc_rs1_eq_x0,
	rvc_rd_ne_x0,
	rvc_rs2_ne_x0,
	rvc_end
};

const rvc_constraint rvcc_c_ebreak[] = {
	rvc_end
};

const rvc_constraint rvcc_c_jalr[] = {
	rvc_rd_eq_ra,
	rvc_rs1_ne_x0,
	rvc_end
};

const rvc_constraint rvcc_c_add[] = {
	rvc_rd_eq_rs1,
	rvc_rd_ne_x0,
	rvc_rs2_ne_x0,
	rvc_end
};

const rvc_constraint rvcc_c_fsdsp[] = {
	rvc_imm_9,
	rvc_imm_x8,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_swsp[] = {
	rvc_imm_8,
	rvc_imm_x4,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_fswsp[] = {
	rvc_imm_8,
	rvc_imm_x4,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_ld[] = {
	rvc_imm_8,
	rvc_imm_x8,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_sd[] = {
	rvc_imm_8,
	rvc_imm_x8,
	rvc_rs1_b3,
	rvc_rs2_b3,
	rvc_end
};

const rvc_constraint rvcc_c_addiw[] = {
	rvc_imm_6,
	rvc_rd_ne_x0,
	rvc_rd_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_c_srli_rv64c[] = {
	rvc_imm_nz,
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_srai_rv64c[] = {
	rvc_imm_nz,
	rvc_rd_eq_rs1,
	rvc_rd_b3,
	rvc_rs1_b3,
	rvc_end
};

const rvc_constraint rvcc_c_slli_rv64c[] = {
	rvc_imm_nz,
	rvc_rd_ne_x0,
	rvc_rd_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_c_ldsp[] = {
	rvc_imm_9,
	rvc_imm_x8,
	rvc_rd_ne_x0,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_sdsp[] = {
	rvc_imm_9,
	rvc_imm_x8,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_lq[] = {
	rvc_imm_9,
	rvc_imm_x16,
	rvc_end
};

const rvc_constraint rvcc_c_sq[] = {
	rvc_imm_9,
	rvc_imm_x16,
	rvc_end
};

const rvc_constraint rvcc_c_lqsp[] = {
	rvc_imm_10,
	rvc_imm_x16,
	rvc_rs1_eq_sp,
	rvc_end
};

const rvc_constraint rvcc_c_sqsp[] = {
	rvc_imm_10,
	rvc_imm_x16,
	rvc_rs1_eq_sp,
	rvc_end
};


const rv_comp_data rvcd_rv32_lui[] = {
	{ rv_op_c_lui, rvcc_c_lui },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_jal[] = {
	{ rv_op_c_jal, rvcc_c_jal },
	{ rv_op_c_j, rvcc_c_j },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_jalr[] = {
	{ rv_op_c_jr, rvcc_c_jr },
	{ rv_op_c_jalr, rvcc_c_jalr },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_beq[] = {
	{ rv_op_c_beqz, rvcc_c_beqz },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_bne[] = {
	{ rv_op_c_bnez, rvcc_c_bnez },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_lw[] = {
	{ rv_op_c_lw, rvcc_c_lw },
	{ rv_op_c_lwsp, rvcc_c_lwsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_sw[] = {
	{ rv_op_c_sw, rvcc_c_sw },
	{ rv_op_c_swsp, rvcc_c_swsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_addi[] = {
	{ rv_op_c_addi4spn, rvcc_c_addi4spn },
	{ rv_op_c_nop, rvcc_c_nop },
	{ rv_op_c_addi, rvcc_c_addi },
	{ rv_op_c_li, rvcc_c_li },
	{ rv_op_c_addi16sp, rvcc_c_addi16sp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_andi[] = {
	{ rv_op_c_andi, rvcc_c_andi },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_slli[] = {
	{ rv_op_c_slli, rvcc_c_slli_rv32c },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_srli[] = {
	{ rv_op_c_srli, rvcc_c_srli_rv32c },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_srai[] = {
	{ rv_op_c_srai, rvcc_c_srai_rv32c },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_add[] = {
	{ rv_op_c_mv, rvcc_c_mv },
	{ rv_op_c_add, rvcc_c_add },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_sub[] = {
	{ rv_op_c_sub, rvcc_c_sub },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_xor[] = {
	{ rv_op_c_xor, rvcc_c_xor },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_or[] = {
	{ rv_op_c_or, rvcc_c_or },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_and[] = {
	{ rv_op_c_and, rvcc_c_and },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_ebreak[] = {
	{ rv_op_c_ebreak, rvcc_c_ebreak },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_flw[] = {
	{ rv_op_c_flw, rvcc_c_flw },
	{ rv_op_c_flwsp, rvcc_c_flwsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_fsw[] = {
	{ rv_op_c_fsw, rvcc_c_fsw },
	{ rv_op_c_fswsp, rvcc_c_fswsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_fld[] = {
	{ rv_op_c_fld, rvcc_c_fld },
	{ rv_op_c_fldsp, rvcc_c_fldsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv32_fsd[] = {
	{ rv_op_c_fsd, rvcc_c_fsd },
	{ rv_op_c_fsdsp, rvcc_c_fsdsp },
	{ rv_op_illegal, nullptr }
};


const rv_comp_data rvcd_rv64_lui[] = {
	{ rv_op_c_lui, rvcc_c_lui },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_jal[] = {
	{ rv_op_c_j, rvcc_c_j },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_jalr[] = {
	{ rv_op_c_jr, rvcc_c_jr },
	{ rv_op_c_jalr, rvcc_c_jalr },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_beq[] = {
	{ rv_op_c_beqz, rvcc_c_beqz },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_bne[] = {
	{ rv_op_c_bnez, rvcc_c_bnez },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_lw[] = {
	{ rv_op_c_lw, rvcc_c_lw },
	{ rv_op_c_lwsp, rvcc_c_lwsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_sw[] = {
	{ rv_op_c_sw, rvcc_c_sw },
	{ rv_op_c_swsp, rvcc_c_swsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_addi[] = {
	{ rv_op_c_addi4spn, rvcc_c_addi4spn },
	{ rv_op_c_nop, rvcc_c_nop },
	{ rv_op_c_addi, rvcc_c_addi },
	{ rv_op_c_li, rvcc_c_li },
	{ rv_op_c_addi16sp, rvcc_c_addi16sp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_andi[] = {
	{ rv_op_c_andi, rvcc_c_andi },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_add[] = {
	{ rv_op_c_mv, rvcc_c_mv },
	{ rv_op_c_add, rvcc_c_add },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_sub[] = {
	{ rv_op_c_sub, rvcc_c_sub },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_xor[] = {
	{ rv_op_c_xor, rvcc_c_xor },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_or[] = {
	{ rv_op_c_or, rvcc_c_or },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_and[] = {
	{ rv_op_c_and, rvcc_c_and },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_ld[] = {
	{ rv_op_c_ld, rvcc_c_ld },
	{ rv_op_c_ldsp, rvcc_c_ldsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_sd[] = {
	{ rv_op_c_sd, rvcc_c_sd },
	{ rv_op_c_sdsp, rvcc_c_sdsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_slli[] = {
	{ rv_op_c_slli, rvcc_c_slli_rv64c },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_srli[] = {
	{ rv_op_c_srli, rvcc_c_srli_rv64c },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_srai[] = {
	{ rv_op_c_srai, rvcc_c_srai_rv64c },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_addiw[] = {
	{ rv_op_c_addiw, rvcc_c_addiw },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_addw[] = {
	{ rv_op_c_addw, rvcc_c_addw },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_subw[] = {
	{ rv_op_c_subw, rvcc_c_subw },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_ebreak[] = {
	{ rv_op_c_ebreak, rvcc_c_ebreak },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_fld[] = {
	{ rv_op_c_fld, rvcc_c_fld },
	{ rv_op_c_fldsp, rvcc_c_fldsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv64_fsd[] = {
	{ rv_op_c_fsd, rvcc_c_fsd },
	{ rv_op_c_fsdsp, rvcc_c_fsdsp },
	{ rv_op_illegal, nullptr }
};


const rv_comp_data rvcd_rv128_lq[] = {
	{ rv_op_c_lq, rvcc_c_lq },
	{ rv_op_c_lqsp, rvcc_c_lqsp },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcd_rv128_sq[] = {
	{ rv_op_c_sq, rvcc_c_sq },
	{ rv_op_c_sqsp, rvcc_c_sqsp },
	{ rv_op_illegal, nullptr }
};


const rv_operand_data rv_operands_T_cfrdq_T_crs1q_T_cimmd[] = {
	{ rv_operand_name_cfrdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmd, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_cfrdq_T_crs1q_T_cimmw[] = {
	{ rv_operand_name_cfrdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmw, rv_operand_type_uimm7, rv_primitive_none, rv_type_uimm, 7 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_cimmj[] = {
	{ rv_operand_name_cimmj, rv_operand_type_simm12, rv_primitive_none, rv_type_simm, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crd0_sx_crs1[] = {
	{ rv_operand_name_crd0, rv_operand_type_creg1, rv_primitive_none, rv_type_creg, 1 },
	{ rv_operand_name_crs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crdq_T_cimm4spn[] = {
	{ rv_operand_name_crdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimm4spn, rv_operand_type_uimm10, rv_primitive_none, rv_type_uimm, 10 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crdq_T_crs1q_T_cimmd[] = {
	{ rv_operand_name_crdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmd, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crdq_T_crs1q_T_cimmq[] = {
	{ rv_operand_name_crdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmq, rv_operand_type_uimm9, rv_primitive_none, rv_type_uimm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crdq_T_crs1q_T_cimmw[] = {
	{ rv_operand_name_crdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmw, rv_operand_type_uimm7, rv_primitive_none, rv_type_uimm, 7 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1q_T_cfrs2q_T_cimmd[] = {
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cfrs2q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmd, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1q_T_cfrs2q_T_cimmw[] = {
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cfrs2q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmw, rv_operand_type_uimm7, rv_primitive_none, rv_type_uimm, 7 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1q_T_cimmb[] = {
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmb, rv_operand_type_simm9, rv_primitive_none, rv_type_simm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1q_T_crs2q_T_cimmd[] = {
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs2q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmd, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1q_T_crs2q_T_cimmq[] = {
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs2q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmq, rv_operand_type_uimm9, rv_primitive_none, rv_type_uimm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1q_T_crs2q_T_cimmw[] = {
	{ rv_operand_name_crs1q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs2q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmw, rv_operand_type_uimm7, rv_primitive_none, rv_type_uimm, 7 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1rdq_T_cimmsh6[] = {
	{ rv_operand_name_crs1rdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cimmsh6, rv_operand_type_uimm6, rv_primitive_none, rv_type_uimm, 6 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1rdq_T_cnzimmi[] = {
	{ rv_operand_name_crs1rdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_cnzimmi, rv_operand_type_simm6, rv_primitive_none, rv_type_simm, 6 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_crs1rdq_T_crs2q[] = {
	{ rv_operand_name_crs1rdq, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_crs2q, rv_operand_type_creg3, rv_primitive_none, rv_type_creg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_oimm20[] = {
	{ rv_operand_name_oimm20, rv_operand_type_offset32, rv_primitive_none, rv_type_offset, 32 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_T_pred_T_succ[] = {
	{ rv_operand_name_pred, rv_operand_type_arg4, rv_primitive_none, rv_type_arg, 4 },
	{ rv_operand_name_succ, rv_operand_type_arg4, rv_primitive_none, rv_type_arg, 4 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_f128_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_f128_frs1_f128_frs2[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_f128_frs1_f128_frs2_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_f128_frs1_f128_frs2_f128_frs3_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs3, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_f32_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_f64_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_s32_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_s32, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_s64_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_s64, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_sx_rs1[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_sx_rs1_T_oimm12[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_u32_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_u32, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f128_frd_u64_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_u64, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_cfrd_T_cimmldsp[] = {
	{ rv_operand_name_cfrd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_cimmldsp, rv_operand_type_uimm9, rv_primitive_none, rv_type_uimm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_cfrd_T_cimmlwsp[] = {
	{ rv_operand_name_cfrd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_cimmlwsp, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_cfrs2_T_cimmsdsp[] = {
	{ rv_operand_name_cfrs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_cimmsdsp, rv_operand_type_uimm9, rv_primitive_none, rv_type_uimm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_cfrs2_T_cimmswsp[] = {
	{ rv_operand_name_cfrs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_cimmswsp, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_f128_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_f32_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_f32_frs1_f32_frs2[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_f32_frs1_f32_frs2_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_f32_frs1_f32_frs2_f32_frs3_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs3, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_f64_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_s32_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_s32, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_s64_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_s64, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_sx_rs1[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_sx_rs1_T_oimm12[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_u32_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_u32, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f32_frd_u64_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_u64, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_f128_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_f32_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_f64_frs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_f64_frs1_f64_frs2[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_f64_frs1_f64_frs2_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_f64_frs1_f64_frs2_f64_frs3_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs3, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_s32_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_s32, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_s64_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_s64, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_sx_rs1[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_sx_rs1_T_oimm12[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_u32_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_u32, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_f64_frd_u64_rs1_T_rm[] = {
	{ rv_operand_name_frd, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_u64, rv_type_ireg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_none[] = {
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_s32_rd_f128_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_s32, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_s32_rd_f32_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_s32, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_s32_rd_f64_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_s32, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_s64_rd_f128_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_s64, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_s64_rd_f32_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_s64, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_s64_rd_f64_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_s64, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crd_T_cimmldsp[] = {
	{ rv_operand_name_crd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmldsp, rv_operand_type_uimm9, rv_primitive_none, rv_type_uimm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crd_T_cimmlqsp[] = {
	{ rv_operand_name_crd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmlqsp, rv_operand_type_uimm10, rv_primitive_none, rv_type_uimm, 10 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crd_T_cimmlwsp[] = {
	{ rv_operand_name_crd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmlwsp, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crd_T_cimmui[] = {
	{ rv_operand_name_crd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmui, rv_operand_type_simm18, rv_primitive_none, rv_type_simm, 18 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crd_sx_crs2[] = {
	{ rv_operand_name_crd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_crs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs1rd_T_cimm16sp[] = {
	{ rv_operand_name_crs1rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimm16sp, rv_operand_type_simm10, rv_primitive_none, rv_type_simm, 10 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs1rd_T_cimmi[] = {
	{ rv_operand_name_crs1rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmi, rv_operand_type_simm6, rv_primitive_none, rv_type_simm, 6 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs1rd_T_cimmsh6[] = {
	{ rv_operand_name_crs1rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmsh6, rv_operand_type_uimm6, rv_primitive_none, rv_type_uimm, 6 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs1rd_T_cnzimmi[] = {
	{ rv_operand_name_crs1rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cnzimmi, rv_operand_type_simm6, rv_primitive_none, rv_type_simm, 6 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs1rd_sx_crs2[] = {
	{ rv_operand_name_crs1rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_crs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs2_T_cimmsdsp[] = {
	{ rv_operand_name_crs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmsdsp, rv_operand_type_uimm9, rv_primitive_none, rv_type_uimm, 9 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs2_T_cimmsqsp[] = {
	{ rv_operand_name_crs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmsqsp, rv_operand_type_uimm10, rv_primitive_none, rv_type_uimm, 10 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_crs2_T_cimmswsp[] = {
	{ rv_operand_name_crs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_cimmswsp, rv_operand_type_uimm8, rv_primitive_none, rv_type_uimm, 8 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_T_imm20[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_imm20, rv_operand_type_simm32, rv_primitive_none, rv_type_simm, 32 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_T_jimm20[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_jimm20, rv_operand_type_offset21, rv_primitive_none, rv_type_offset, 21 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_T_oimm20[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm20, rv_operand_type_offset32, rv_primitive_none, rv_type_offset, 32 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_T_zimm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_zimm, rv_operand_type_uimm5, rv_primitive_none, rv_type_uimm, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_T_zimm_T_csr12[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_zimm, rv_operand_type_uimm5, rv_primitive_none, rv_type_uimm, 5 },
	{ rv_operand_name_csr12, rv_operand_type_uimm12, rv_primitive_none, rv_type_uimm, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_f128_frs1[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_f128_frs1_f128_frs2[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_f32_frs1[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_f32_frs1_f32_frs2[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_f64_frs1[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_f64_frs1_f64_frs2[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_aq_T_rl[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_aq, rv_operand_type_arg1, rv_primitive_none, rv_type_arg, 1 },
	{ rv_operand_name_rl, rv_operand_type_arg1, rv_primitive_none, rv_type_arg, 1 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_csr12[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_csr12, rv_operand_type_uimm12, rv_primitive_none, rv_type_uimm, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_imm12[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_imm12, rv_operand_type_simm12, rv_primitive_none, rv_type_simm, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_oimm12[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_shamt5[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_shamt5, rv_operand_type_uimm5, rv_primitive_none, rv_type_uimm, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_shamt6[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_shamt6, rv_operand_type_uimm6, rv_primitive_none, rv_type_uimm, 6 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_T_shamt7[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_shamt7, rv_operand_type_uimm7, rv_primitive_none, rv_type_uimm, 7 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_sx_rs2[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_aq, rv_operand_type_arg1, rv_primitive_none, rv_type_arg, 1 },
	{ rv_operand_name_rl, rv_operand_type_arg1, rv_primitive_none, rv_type_arg, 1 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rd_sx_rs2[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1_T_oimm20[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm20, rv_operand_type_offset32, rv_primitive_none, rv_type_offset, 32 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1_f128_frs2_T_simm12[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_simm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1_f32_frs2_T_simm12[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_simm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1_f64_frs2_T_simm12[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_frs2, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_simm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1_sx_rs2_T_sbimm12[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_sbimm12, rv_operand_type_offset13, rv_primitive_none, rv_type_offset, 13 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs1_sx_rs2_T_simm12[] = {
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_simm12, rv_operand_type_offset12, rv_primitive_none, rv_type_offset, 12 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs2_T_oimm20[] = {
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm20, rv_operand_type_offset32, rv_primitive_none, rv_type_offset, 32 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_sx_rs2_sx_rs1_T_oimm20[] = {
	{ rv_operand_name_rs2, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_rs1, rv_operand_type_ireg5, rv_primitive_sx, rv_type_ireg, 5 },
	{ rv_operand_name_oimm20, rv_operand_type_offset32, rv_primitive_none, rv_type_offset, 32 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_u32_rd_f128_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_u32, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_u32_rd_f32_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_u32, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_u32_rd_f64_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_u32, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_u64_rd_f128_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_u64, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f128, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_u64_rd_f32_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_u64, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f32, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_operand_data rv_operands_u64_rd_f64_frs1_T_rm[] = {
	{ rv_operand_name_rd, rv_operand_type_ireg5, rv_primitive_u64, rv_type_ireg, 5 },
	{ rv_operand_name_frs1, rv_operand_type_freg5, rv_primitive_f64, rv_type_freg, 5 },
	{ rv_operand_name_rm, rv_operand_type_arg3, rv_primitive_none, rv_type_arg, 3 },
	{ rv_operand_name_none, rv_operand_type_none, rv_primitive_none, rv_type_none, 0 }
};

const rv_codec rv_inst_codec[] = {
	/*              unknown */ rv_codec_illegal,
	/*                  lui */ rv_codec_u,
	/*                auipc */ rv_codec_u,
	/*                  jal */ rv_codec_uj,
	/*                 jalr */ rv_codec_i,
	/*                  beq */ rv_codec_sb,
	/*                  bne */ rv_codec_sb,
	/*                  blt */ rv_codec_sb,
	/*                  bge */ rv_codec_sb,
	/*                 bltu */ rv_codec_sb,
	/*                 bgeu */ rv_codec_sb,
	/*                   lb */ rv_codec_i,
	/*                   lh */ rv_codec_i,
	/*                   lw */ rv_codec_i,
	/*                  lbu */ rv_codec_i,
	/*                  lhu */ rv_codec_i,
	/*                   sb */ rv_codec_s,
	/*                   sh */ rv_codec_s,
	/*                   sw */ rv_codec_s,
	/*                 addi */ rv_codec_i,
	/*                 slti */ rv_codec_i,
	/*                sltiu */ rv_codec_i,
	/*                 xori */ rv_codec_i,
	/*                  ori */ rv_codec_i,
	/*                 andi */ rv_codec_i,
	/*                 slli */ rv_codec_i_sh7,
	/*                 srli */ rv_codec_i_sh7,
	/*                 srai */ rv_codec_i_sh7,
	/*                  add */ rv_codec_r,
	/*                  sub */ rv_codec_r,
	/*                  sll */ rv_codec_r,
	/*                  slt */ rv_codec_r,
	/*                 sltu */ rv_codec_r,
	/*                  xor */ rv_codec_r,
	/*                  srl */ rv_codec_r,
	/*                  sra */ rv_codec_r,
	/*                   or */ rv_codec_r,
	/*                  and */ rv_codec_r,
	/*                fence */ rv_codec_r_f,
	/*              fence.i */ rv_codec_none,
	/*                  lwu */ rv_codec_i,
	/*                   ld */ rv_codec_i,
	/*                   sd */ rv_codec_s,
	/*                addiw */ rv_codec_i,
	/*                slliw */ rv_codec_i_sh5,
	/*                srliw */ rv_codec_i_sh5,
	/*                sraiw */ rv_codec_i_sh5,
	/*                 addw */ rv_codec_r,
	/*                 subw */ rv_codec_r,
	/*                 sllw */ rv_codec_r,
	/*                 srlw */ rv_codec_r,
	/*                 sraw */ rv_codec_r,
	/*                  ldu */ rv_codec_i,
	/*                   lq */ rv_codec_i,
	/*                   sq */ rv_codec_s,
	/*                addid */ rv_codec_i,
	/*                sllid */ rv_codec_i_sh6,
	/*                srlid */ rv_codec_i_sh6,
	/*                sraid */ rv_codec_i_sh6,
	/*                 addd */ rv_codec_r,
	/*                 subd */ rv_codec_r,
	/*                 slld */ rv_codec_r,
	/*                 srld */ rv_codec_r,
	/*                 srad */ rv_codec_r,
	/*                  mul */ rv_codec_r,
	/*                 mulh */ rv_codec_r,
	/*               mulhsu */ rv_codec_r,
	/*                mulhu */ rv_codec_r,
	/*                  div */ rv_codec_r,
	/*                 divu */ rv_codec_r,
	/*                  rem */ rv_codec_r,
	/*                 remu */ rv_codec_r,
	/*                 mulw */ rv_codec_r,
	/*                 divw */ rv_codec_r,
	/*                divuw */ rv_codec_r,
	/*                 remw */ rv_codec_r,
	/*                remuw */ rv_codec_r,
	/*                 muld */ rv_codec_r,
	/*                 divd */ rv_codec_r,
	/*                divud */ rv_codec_r,
	/*                 remd */ rv_codec_r,
	/*                remud */ rv_codec_r,
	/*                 lr.w */ rv_codec_r_l,
	/*                 sc.w */ rv_codec_r_a,
	/*            amoswap.w */ rv_codec_r_a,
	/*             amoadd.w */ rv_codec_r_a,
	/*             amoxor.w */ rv_codec_r_a,
	/*              amoor.w */ rv_codec_r_a,
	/*             amoand.w */ rv_codec_r_a,
	/*             amomin.w */ rv_codec_r_a,
	/*             amomax.w */ rv_codec_r_a,
	/*            amominu.w */ rv_codec_r_a,
	/*            amomaxu.w */ rv_codec_r_a,
	/*                 lr.d */ rv_codec_r_l,
	/*                 sc.d */ rv_codec_r_a,
	/*            amoswap.d */ rv_codec_r_a,
	/*             amoadd.d */ rv_codec_r_a,
	/*             amoxor.d */ rv_codec_r_a,
	/*              amoor.d */ rv_codec_r_a,
	/*             amoand.d */ rv_codec_r_a,
	/*             amomin.d */ rv_codec_r_a,
	/*             amomax.d */ rv_codec_r_a,
	/*            amominu.d */ rv_codec_r_a,
	/*            amomaxu.d */ rv_codec_r_a,
	/*                 lr.q */ rv_codec_r_l,
	/*                 sc.q */ rv_codec_r_a,
	/*            amoswap.q */ rv_codec_r_a,
	/*             amoadd.q */ rv_codec_r_a,
	/*             amoxor.q */ rv_codec_r_a,
	/*              amoor.q */ rv_codec_r_a,
	/*             amoand.q */ rv_codec_r_a,
	/*             amomin.q */ rv_codec_r_a,
	/*             amomax.q */ rv_codec_r_a,
	/*            amominu.q */ rv_codec_r_a,
	/*            amomaxu.q */ rv_codec_r_a,
	/*                ecall */ rv_codec_none,
	/*               ebreak */ rv_codec_none,
	/*                 uret */ rv_codec_none,
	/*                 sret */ rv_codec_none,
	/*                 hret */ rv_codec_none,
	/*                 mret */ rv_codec_none,
	/*                 dret */ rv_codec_none,
	/*            sfence.vm */ rv_codec_r,
	/*                  wfi */ rv_codec_none,
	/*                csrrw */ rv_codec_i_csr,
	/*                csrrs */ rv_codec_i_csr,
	/*                csrrc */ rv_codec_i_csr,
	/*               csrrwi */ rv_codec_i_csr,
	/*               csrrsi */ rv_codec_i_csr,
	/*               csrrci */ rv_codec_i_csr,
	/*                  flw */ rv_codec_i,
	/*                  fsw */ rv_codec_s,
	/*              fmadd.s */ rv_codec_r4_m,
	/*              fmsub.s */ rv_codec_r4_m,
	/*             fnmsub.s */ rv_codec_r4_m,
	/*             fnmadd.s */ rv_codec_r4_m,
	/*               fadd.s */ rv_codec_r_m,
	/*               fsub.s */ rv_codec_r_m,
	/*               fmul.s */ rv_codec_r_m,
	/*               fdiv.s */ rv_codec_r_m,
	/*              fsgnj.s */ rv_codec_r,
	/*             fsgnjn.s */ rv_codec_r,
	/*             fsgnjx.s */ rv_codec_r,
	/*               fmin.s */ rv_codec_r,
	/*               fmax.s */ rv_codec_r,
	/*              fsqrt.s */ rv_codec_r_m,
	/*                fle.s */ rv_codec_r,
	/*                flt.s */ rv_codec_r,
	/*                feq.s */ rv_codec_r,
	/*             fcvt.w.s */ rv_codec_r_m,
	/*            fcvt.wu.s */ rv_codec_r_m,
	/*             fcvt.s.w */ rv_codec_r_m,
	/*            fcvt.s.wu */ rv_codec_r_m,
	/*              fmv.x.s */ rv_codec_r,
	/*             fclass.s */ rv_codec_r,
	/*              fmv.s.x */ rv_codec_r,
	/*             fcvt.l.s */ rv_codec_r_m,
	/*            fcvt.lu.s */ rv_codec_r_m,
	/*             fcvt.s.l */ rv_codec_r_m,
	/*            fcvt.s.lu */ rv_codec_r_m,
	/*                  fld */ rv_codec_i,
	/*                  fsd */ rv_codec_s,
	/*              fmadd.d */ rv_codec_r4_m,
	/*              fmsub.d */ rv_codec_r4_m,
	/*             fnmsub.d */ rv_codec_r4_m,
	/*             fnmadd.d */ rv_codec_r4_m,
	/*               fadd.d */ rv_codec_r_m,
	/*               fsub.d */ rv_codec_r_m,
	/*               fmul.d */ rv_codec_r_m,
	/*               fdiv.d */ rv_codec_r_m,
	/*              fsgnj.d */ rv_codec_r,
	/*             fsgnjn.d */ rv_codec_r,
	/*             fsgnjx.d */ rv_codec_r,
	/*               fmin.d */ rv_codec_r,
	/*               fmax.d */ rv_codec_r,
	/*             fcvt.s.d */ rv_codec_r_m,
	/*             fcvt.d.s */ rv_codec_r_m,
	/*              fsqrt.d */ rv_codec_r_m,
	/*                fle.d */ rv_codec_r,
	/*                flt.d */ rv_codec_r,
	/*                feq.d */ rv_codec_r,
	/*             fcvt.w.d */ rv_codec_r_m,
	/*            fcvt.wu.d */ rv_codec_r_m,
	/*             fcvt.d.w */ rv_codec_r_m,
	/*            fcvt.d.wu */ rv_codec_r_m,
	/*             fclass.d */ rv_codec_r,
	/*             fcvt.l.d */ rv_codec_r_m,
	/*            fcvt.lu.d */ rv_codec_r_m,
	/*              fmv.x.d */ rv_codec_r,
	/*             fcvt.d.l */ rv_codec_r_m,
	/*            fcvt.d.lu */ rv_codec_r_m,
	/*              fmv.d.x */ rv_codec_r,
	/*                  flq */ rv_codec_i,
	/*                  fsq */ rv_codec_s,
	/*              fmadd.q */ rv_codec_r4_m,
	/*              fmsub.q */ rv_codec_r4_m,
	/*             fnmsub.q */ rv_codec_r4_m,
	/*             fnmadd.q */ rv_codec_r4_m,
	/*               fadd.q */ rv_codec_r_m,
	/*               fsub.q */ rv_codec_r_m,
	/*               fmul.q */ rv_codec_r_m,
	/*               fdiv.q */ rv_codec_r_m,
	/*              fsgnj.q */ rv_codec_r,
	/*             fsgnjn.q */ rv_codec_r,
	/*             fsgnjx.q */ rv_codec_r,
	/*               fmin.q */ rv_codec_r,
	/*               fmax.q */ rv_codec_r,
	/*             fcvt.s.q */ rv_codec_r_m,
	/*             fcvt.q.s */ rv_codec_r_m,
	/*             fcvt.d.q */ rv_codec_r_m,
	/*             fcvt.q.d */ rv_codec_r_m,
	/*              fsqrt.q */ rv_codec_r_m,
	/*                fle.q */ rv_codec_r,
	/*                flt.q */ rv_codec_r,
	/*                feq.q */ rv_codec_r,
	/*             fcvt.w.q */ rv_codec_r_m,
	/*            fcvt.wu.q */ rv_codec_r_m,
	/*             fcvt.q.w */ rv_codec_r_m,
	/*            fcvt.q.wu */ rv_codec_r_m,
	/*             fclass.q */ rv_codec_r,
	/*             fcvt.l.q */ rv_codec_r_m,
	/*            fcvt.lu.q */ rv_codec_r_m,
	/*             fcvt.q.l */ rv_codec_r_m,
	/*            fcvt.q.lu */ rv_codec_r_m,
	/*              fmv.x.q */ rv_codec_r,
	/*              fmv.q.x */ rv_codec_r,
	/*           c.addi4spn */ rv_codec_ciw_4spn,
	/*                c.fld */ rv_codec_cl_ld,
	/*                 c.lw */ rv_codec_cl_lw,
	/*                c.flw */ rv_codec_cl_lw,
	/*                c.fsd */ rv_codec_cs_sd,
	/*                 c.sw */ rv_codec_cs_sw,
	/*                c.fsw */ rv_codec_cs_sw,
	/*                c.nop */ rv_codec_ci_none,
	/*               c.addi */ rv_codec_ci,
	/*                c.jal */ rv_codec_cj_jal,
	/*                 c.li */ rv_codec_ci_li,
	/*           c.addi16sp */ rv_codec_ci_16sp,
	/*                c.lui */ rv_codec_ci_lui,
	/*               c.srli */ rv_codec_cb_sh6,
	/*               c.srai */ rv_codec_cb_sh6,
	/*               c.andi */ rv_codec_cb_imm,
	/*                c.sub */ rv_codec_cs,
	/*                c.xor */ rv_codec_cs,
	/*                 c.or */ rv_codec_cs,
	/*                c.and */ rv_codec_cs,
	/*               c.subw */ rv_codec_cs,
	/*               c.addw */ rv_codec_cs,
	/*                  c.j */ rv_codec_cj,
	/*               c.beqz */ rv_codec_cb,
	/*               c.bnez */ rv_codec_cb,
	/*               c.slli */ rv_codec_ci_sh6,
	/*              c.fldsp */ rv_codec_ci_ldsp,
	/*               c.lwsp */ rv_codec_ci_lwsp,
	/*              c.flwsp */ rv_codec_ci_lwsp,
	/*                 c.jr */ rv_codec_cr_jr,
	/*                 c.mv */ rv_codec_cr_mv,
	/*             c.ebreak */ rv_codec_ci_none,
	/*               c.jalr */ rv_codec_cr_jalr,
	/*                c.add */ rv_codec_cr,
	/*              c.fsdsp */ rv_codec_css_sdsp,
	/*               c.swsp */ rv_codec_css_swsp,
	/*              c.fswsp */ rv_codec_css_swsp,
	/*                 c.ld */ rv_codec_cl_ld,
	/*                 c.sd */ rv_codec_cs_sd,
	/*              c.addiw */ rv_codec_ci,
	/*               c.ldsp */ rv_codec_ci_ldsp,
	/*               c.sdsp */ rv_codec_css_sdsp,
	/*                 c.lq */ rv_codec_cl_lq,
	/*                 c.sq */ rv_codec_cs_sq,
	/*               c.lqsp */ rv_codec_ci_lqsp,
	/*               c.sqsp */ rv_codec_css_sqsp,
	/*                  nop */ rv_codec_i,
	/*                   mv */ rv_codec_i,
	/*                  not */ rv_codec_i,
	/*                  neg */ rv_codec_r,
	/*                 negw */ rv_codec_r,
	/*               sext.w */ rv_codec_i,
	/*                 seqz */ rv_codec_i,
	/*                 snez */ rv_codec_r,
	/*                 sltz */ rv_codec_r,
	/*                 sgtz */ rv_codec_r,
	/*                fmv.s */ rv_codec_r,
	/*               fabs.s */ rv_codec_r,
	/*               fneg.s */ rv_codec_r,
	/*                fmv.d */ rv_codec_r,
	/*               fabs.d */ rv_codec_r,
	/*               fneg.d */ rv_codec_r,
	/*                fmv.q */ rv_codec_r,
	/*               fabs.q */ rv_codec_r,
	/*               fneg.q */ rv_codec_r,
	/*                 beqz */ rv_codec_sb,
	/*                 bnez */ rv_codec_sb,
	/*                 blez */ rv_codec_sb,
	/*                 bgez */ rv_codec_sb,
	/*                 bltz */ rv_codec_sb,
	/*                 bgtz */ rv_codec_sb,
	/*                  ble */ rv_codec_sb,
	/*                 bleu */ rv_codec_sb,
	/*                  bgt */ rv_codec_sb,
	/*                 bgtu */ rv_codec_sb,
	/*                    j */ rv_codec_uj,
	/*                  ret */ rv_codec_i,
	/*                   jr */ rv_codec_i,
	/*              rdcycle */ rv_codec_i_csr,
	/*               rdtime */ rv_codec_i_csr,
	/*            rdinstret */ rv_codec_i_csr,
	/*             rdcycleh */ rv_codec_i_csr,
	/*              rdtimeh */ rv_codec_i_csr,
	/*           rdinstreth */ rv_codec_i_csr,
	/*                frcsr */ rv_codec_i_csr,
	/*                 frrm */ rv_codec_i_csr,
	/*              frflags */ rv_codec_i_csr,
	/*                fscsr */ rv_codec_i_csr,
	/*                 fsrm */ rv_codec_i_csr,
	/*              fsflags */ rv_codec_i_csr,
	/*                fsrmi */ rv_codec_i_csr,
	/*             fsflagsi */ rv_codec_i_csr,
};

const char* rv_inst_format[] = {
	/*              unknown */ rv_fmt_none,
	/*                  lui */ rv_fmt_rd_imm,
	/*                auipc */ rv_fmt_rd_offset,
	/*                  jal */ rv_fmt_rd_offset,
	/*                 jalr */ rv_fmt_rd_rs1_offset,
	/*                  beq */ rv_fmt_rs1_rs2_offset,
	/*                  bne */ rv_fmt_rs1_rs2_offset,
	/*                  blt */ rv_fmt_rs1_rs2_offset,
	/*                  bge */ rv_fmt_rs1_rs2_offset,
	/*                 bltu */ rv_fmt_rs1_rs2_offset,
	/*                 bgeu */ rv_fmt_rs1_rs2_offset,
	/*                   lb */ rv_fmt_rd_offset_rs1,
	/*                   lh */ rv_fmt_rd_offset_rs1,
	/*                   lw */ rv_fmt_rd_offset_rs1,
	/*                  lbu */ rv_fmt_rd_offset_rs1,
	/*                  lhu */ rv_fmt_rd_offset_rs1,
	/*                   sb */ rv_fmt_rs2_offset_rs1,
	/*                   sh */ rv_fmt_rs2_offset_rs1,
	/*                   sw */ rv_fmt_rs2_offset_rs1,
	/*                 addi */ rv_fmt_rd_rs1_imm,
	/*                 slti */ rv_fmt_rd_rs1_imm,
	/*                sltiu */ rv_fmt_rd_rs1_imm,
	/*                 xori */ rv_fmt_rd_rs1_imm,
	/*                  ori */ rv_fmt_rd_rs1_imm,
	/*                 andi */ rv_fmt_rd_rs1_imm,
	/*                 slli */ rv_fmt_rd_rs1_imm,
	/*                 srli */ rv_fmt_rd_rs1_imm,
	/*                 srai */ rv_fmt_rd_rs1_imm,
	/*                  add */ rv_fmt_rd_rs1_rs2,
	/*                  sub */ rv_fmt_rd_rs1_rs2,
	/*                  sll */ rv_fmt_rd_rs1_rs2,
	/*                  slt */ rv_fmt_rd_rs1_rs2,
	/*                 sltu */ rv_fmt_rd_rs1_rs2,
	/*                  xor */ rv_fmt_rd_rs1_rs2,
	/*                  srl */ rv_fmt_rd_rs1_rs2,
	/*                  sra */ rv_fmt_rd_rs1_rs2,
	/*                   or */ rv_fmt_rd_rs1_rs2,
	/*                  and */ rv_fmt_rd_rs1_rs2,
	/*                fence */ rv_fmt_pred_succ,
	/*              fence.i */ rv_fmt_none,
	/*                  lwu */ rv_fmt_rd_offset_rs1,
	/*                   ld */ rv_fmt_rd_offset_rs1,
	/*                   sd */ rv_fmt_rs2_offset_rs1,
	/*                addiw */ rv_fmt_rd_rs1_imm,
	/*                slliw */ rv_fmt_rd_rs1_imm,
	/*                srliw */ rv_fmt_rd_rs1_imm,
	/*                sraiw */ rv_fmt_rd_rs1_imm,
	/*                 addw */ rv_fmt_rd_rs1_rs2,
	/*                 subw */ rv_fmt_rd_rs1_rs2,
	/*                 sllw */ rv_fmt_rd_rs1_rs2,
	/*                 srlw */ rv_fmt_rd_rs1_rs2,
	/*                 sraw */ rv_fmt_rd_rs1_rs2,
	/*                  ldu */ rv_fmt_rd_offset_rs1,
	/*                   lq */ rv_fmt_rd_offset_rs1,
	/*                   sq */ rv_fmt_rs2_offset_rs1,
	/*                addid */ rv_fmt_rd_rs1_imm,
	/*                sllid */ rv_fmt_rd_rs1_imm,
	/*                srlid */ rv_fmt_rd_rs1_imm,
	/*                sraid */ rv_fmt_rd_rs1_imm,
	/*                 addd */ rv_fmt_rd_rs1_rs2,
	/*                 subd */ rv_fmt_rd_rs1_rs2,
	/*                 slld */ rv_fmt_rd_rs1_rs2,
	/*                 srld */ rv_fmt_rd_rs1_rs2,
	/*                 srad */ rv_fmt_rd_rs1_rs2,
	/*                  mul */ rv_fmt_rd_rs1_rs2,
	/*                 mulh */ rv_fmt_rd_rs1_rs2,
	/*               mulhsu */ rv_fmt_rd_rs1_rs2,
	/*                mulhu */ rv_fmt_rd_rs1_rs2,
	/*                  div */ rv_fmt_rd_rs1_rs2,
	/*                 divu */ rv_fmt_rd_rs1_rs2,
	/*                  rem */ rv_fmt_rd_rs1_rs2,
	/*                 remu */ rv_fmt_rd_rs1_rs2,
	/*                 mulw */ rv_fmt_rd_rs1_rs2,
	/*                 divw */ rv_fmt_rd_rs1_rs2,
	/*                divuw */ rv_fmt_rd_rs1_rs2,
	/*                 remw */ rv_fmt_rd_rs1_rs2,
	/*                remuw */ rv_fmt_rd_rs1_rs2,
	/*                 muld */ rv_fmt_rd_rs1_rs2,
	/*                 divd */ rv_fmt_rd_rs1_rs2,
	/*                divud */ rv_fmt_rd_rs1_rs2,
	/*                 remd */ rv_fmt_rd_rs1_rs2,
	/*                remud */ rv_fmt_rd_rs1_rs2,
	/*                 lr.w */ rv_fmt_aqrl_rd_rs1,
	/*                 sc.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amoswap.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoadd.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoxor.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*              amoor.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoand.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amomin.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amomax.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amominu.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amomaxu.w */ rv_fmt_aqrl_rd_rs2_rs1,
	/*                 lr.d */ rv_fmt_aqrl_rd_rs1,
	/*                 sc.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amoswap.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoadd.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoxor.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*              amoor.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoand.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amomin.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amomax.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amominu.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amomaxu.d */ rv_fmt_aqrl_rd_rs2_rs1,
	/*                 lr.q */ rv_fmt_aqrl_rd_rs1,
	/*                 sc.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amoswap.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoadd.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoxor.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*              amoor.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amoand.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amomin.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*             amomax.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amominu.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*            amomaxu.q */ rv_fmt_aqrl_rd_rs2_rs1,
	/*                ecall */ rv_fmt_none,
	/*               ebreak */ rv_fmt_none,
	/*                 uret */ rv_fmt_none,
	/*                 sret */ rv_fmt_none,
	/*                 hret */ rv_fmt_none,
	/*                 mret */ rv_fmt_none,
	/*                 dret */ rv_fmt_none,
	/*            sfence.vm */ rv_fmt_rs1,
	/*                  wfi */ rv_fmt_none,
	/*                csrrw */ rv_fmt_rd_csr_rs1,
	/*                csrrs */ rv_fmt_rd_csr_rs1,
	/*                csrrc */ rv_fmt_rd_csr_rs1,
	/*               csrrwi */ rv_fmt_rd_csr_zimm,
	/*               csrrsi */ rv_fmt_rd_csr_zimm,
	/*               csrrci */ rv_fmt_rd_csr_zimm,
	/*                  flw */ rv_fmt_frd_offset_rs1,
	/*                  fsw */ rv_fmt_frs2_offset_rs1,
	/*              fmadd.s */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*              fmsub.s */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*             fnmsub.s */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*             fnmadd.s */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*               fadd.s */ rv_fmt_rm_frd_frs1_frs2,
	/*               fsub.s */ rv_fmt_rm_frd_frs1_frs2,
	/*               fmul.s */ rv_fmt_rm_frd_frs1_frs2,
	/*               fdiv.s */ rv_fmt_rm_frd_frs1_frs2,
	/*              fsgnj.s */ rv_fmt_frd_frs1_frs2,
	/*             fsgnjn.s */ rv_fmt_frd_frs1_frs2,
	/*             fsgnjx.s */ rv_fmt_frd_frs1_frs2,
	/*               fmin.s */ rv_fmt_frd_frs1_frs2,
	/*               fmax.s */ rv_fmt_frd_frs1_frs2,
	/*              fsqrt.s */ rv_fmt_rm_frd_frs1,
	/*                fle.s */ rv_fmt_rd_frs1_frs2,
	/*                flt.s */ rv_fmt_rd_frs1_frs2,
	/*                feq.s */ rv_fmt_rd_frs1_frs2,
	/*             fcvt.w.s */ rv_fmt_rm_rd_frs1,
	/*            fcvt.wu.s */ rv_fmt_rm_rd_frs1,
	/*             fcvt.s.w */ rv_fmt_rm_frd_rs1,
	/*            fcvt.s.wu */ rv_fmt_rm_frd_rs1,
	/*              fmv.x.s */ rv_fmt_rd_frs1,
	/*             fclass.s */ rv_fmt_rd_frs1,
	/*              fmv.s.x */ rv_fmt_frd_rs1,
	/*             fcvt.l.s */ rv_fmt_rm_rd_frs1,
	/*            fcvt.lu.s */ rv_fmt_rm_rd_frs1,
	/*             fcvt.s.l */ rv_fmt_rm_frd_rs1,
	/*            fcvt.s.lu */ rv_fmt_rm_frd_rs1,
	/*                  fld */ rv_fmt_frd_offset_rs1,
	/*                  fsd */ rv_fmt_frs2_offset_rs1,
	/*              fmadd.d */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*              fmsub.d */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*             fnmsub.d */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*             fnmadd.d */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*               fadd.d */ rv_fmt_rm_frd_frs1_frs2,
	/*               fsub.d */ rv_fmt_rm_frd_frs1_frs2,
	/*               fmul.d */ rv_fmt_rm_frd_frs1_frs2,
	/*               fdiv.d */ rv_fmt_rm_frd_frs1_frs2,
	/*              fsgnj.d */ rv_fmt_frd_frs1_frs2,
	/*             fsgnjn.d */ rv_fmt_frd_frs1_frs2,
	/*             fsgnjx.d */ rv_fmt_frd_frs1_frs2,
	/*               fmin.d */ rv_fmt_frd_frs1_frs2,
	/*               fmax.d */ rv_fmt_frd_frs1_frs2,
	/*             fcvt.s.d */ rv_fmt_rm_frd_frs1,
	/*             fcvt.d.s */ rv_fmt_rm_frd_frs1,
	/*              fsqrt.d */ rv_fmt_rm_frd_frs1,
	/*                fle.d */ rv_fmt_rd_frs1_frs2,
	/*                flt.d */ rv_fmt_rd_frs1_frs2,
	/*                feq.d */ rv_fmt_rd_frs1_frs2,
	/*             fcvt.w.d */ rv_fmt_rm_rd_frs1,
	/*            fcvt.wu.d */ rv_fmt_rm_rd_frs1,
	/*             fcvt.d.w */ rv_fmt_rm_frd_rs1,
	/*            fcvt.d.wu */ rv_fmt_rm_frd_rs1,
	/*             fclass.d */ rv_fmt_rd_frs1,
	/*             fcvt.l.d */ rv_fmt_rm_rd_frs1,
	/*            fcvt.lu.d */ rv_fmt_rm_rd_frs1,
	/*              fmv.x.d */ rv_fmt_rd_frs1,
	/*             fcvt.d.l */ rv_fmt_rm_frd_rs1,
	/*            fcvt.d.lu */ rv_fmt_rm_frd_rs1,
	/*              fmv.d.x */ rv_fmt_frd_rs1,
	/*                  flq */ rv_fmt_frd_offset_rs1,
	/*                  fsq */ rv_fmt_frs2_offset_rs1,
	/*              fmadd.q */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*              fmsub.q */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*             fnmsub.q */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*             fnmadd.q */ rv_fmt_rm_frd_frs1_frs2_frs3,
	/*               fadd.q */ rv_fmt_rm_frd_frs1_frs2,
	/*               fsub.q */ rv_fmt_rm_frd_frs1_frs2,
	/*               fmul.q */ rv_fmt_rm_frd_frs1_frs2,
	/*               fdiv.q */ rv_fmt_rm_frd_frs1_frs2,
	/*              fsgnj.q */ rv_fmt_frd_frs1_frs2,
	/*             fsgnjn.q */ rv_fmt_frd_frs1_frs2,
	/*             fsgnjx.q */ rv_fmt_frd_frs1_frs2,
	/*               fmin.q */ rv_fmt_frd_frs1_frs2,
	/*               fmax.q */ rv_fmt_frd_frs1_frs2,
	/*             fcvt.s.q */ rv_fmt_rm_frd_frs1,
	/*             fcvt.q.s */ rv_fmt_rm_frd_frs1,
	/*             fcvt.d.q */ rv_fmt_rm_frd_frs1,
	/*             fcvt.q.d */ rv_fmt_rm_frd_frs1,
	/*              fsqrt.q */ rv_fmt_rm_frd_frs1,
	/*                fle.q */ rv_fmt_rd_frs1_frs2,
	/*                flt.q */ rv_fmt_rd_frs1_frs2,
	/*                feq.q */ rv_fmt_rd_frs1_frs2,
	/*             fcvt.w.q */ rv_fmt_rm_rd_frs1,
	/*            fcvt.wu.q */ rv_fmt_rm_rd_frs1,
	/*             fcvt.q.w */ rv_fmt_rm_frd_rs1,
	/*            fcvt.q.wu */ rv_fmt_rm_frd_rs1,
	/*             fclass.q */ rv_fmt_rd_frs1,
	/*             fcvt.l.q */ rv_fmt_rm_rd_frs1,
	/*            fcvt.lu.q */ rv_fmt_rm_rd_frs1,
	/*             fcvt.q.l */ rv_fmt_rm_frd_rs1,
	/*            fcvt.q.lu */ rv_fmt_rm_frd_rs1,
	/*              fmv.x.q */ rv_fmt_rd_frs1,
	/*              fmv.q.x */ rv_fmt_frd_rs1,
	/*           c.addi4spn */ rv_fmt_rd_rs1_imm,
	/*                c.fld */ rv_fmt_frd_offset_rs1,
	/*                 c.lw */ rv_fmt_rd_offset_rs1,
	/*                c.flw */ rv_fmt_frd_offset_rs1,
	/*                c.fsd */ rv_fmt_frs2_offset_rs1,
	/*                 c.sw */ rv_fmt_rs2_offset_rs1,
	/*                c.fsw */ rv_fmt_frs2_offset_rs1,
	/*                c.nop */ rv_fmt_none,
	/*               c.addi */ rv_fmt_rd_rs1_imm,
	/*                c.jal */ rv_fmt_rd_offset,
	/*                 c.li */ rv_fmt_rd_rs1_imm,
	/*           c.addi16sp */ rv_fmt_rd_rs1_imm,
	/*                c.lui */ rv_fmt_rd_imm,
	/*               c.srli */ rv_fmt_rd_rs1_imm,
	/*               c.srai */ rv_fmt_rd_rs1_imm,
	/*               c.andi */ rv_fmt_rd_rs1_imm,
	/*                c.sub */ rv_fmt_rd_rs1_rs2,
	/*                c.xor */ rv_fmt_rd_rs1_rs2,
	/*                 c.or */ rv_fmt_rd_rs1_rs2,
	/*                c.and */ rv_fmt_rd_rs1_rs2,
	/*               c.subw */ rv_fmt_rd_rs1_rs2,
	/*               c.addw */ rv_fmt_rd_rs1_rs2,
	/*                  c.j */ rv_fmt_rd_offset,
	/*               c.beqz */ rv_fmt_rs1_rs2_offset,
	/*               c.bnez */ rv_fmt_rs1_rs2_offset,
	/*               c.slli */ rv_fmt_rd_rs1_imm,
	/*              c.fldsp */ rv_fmt_frd_offset_rs1,
	/*               c.lwsp */ rv_fmt_rd_offset_rs1,
	/*              c.flwsp */ rv_fmt_frd_offset_rs1,
	/*                 c.jr */ rv_fmt_rd_rs1_offset,
	/*                 c.mv */ rv_fmt_rd_rs1_rs2,
	/*             c.ebreak */ rv_fmt_none,
	/*               c.jalr */ rv_fmt_rd_rs1_offset,
	/*                c.add */ rv_fmt_rd_rs1_rs2,
	/*              c.fsdsp */ rv_fmt_frs2_offset_rs1,
	/*               c.swsp */ rv_fmt_rs2_offset_rs1,
	/*              c.fswsp */ rv_fmt_frs2_offset_rs1,
	/*                 c.ld */ rv_fmt_rd_offset_rs1,
	/*                 c.sd */ rv_fmt_rs2_offset_rs1,
	/*              c.addiw */ rv_fmt_rd_rs1_imm,
	/*               c.ldsp */ rv_fmt_rd_offset_rs1,
	/*               c.sdsp */ rv_fmt_rs2_offset_rs1,
	/*                 c.lq */ rv_fmt_rd_offset_rs1,
	/*                 c.sq */ rv_fmt_rs2_offset_rs1,
	/*               c.lqsp */ rv_fmt_rd_offset_rs1,
	/*               c.sqsp */ rv_fmt_rs2_offset_rs1,
	/*                  nop */ rv_fmt_none,
	/*                   mv */ rv_fmt_rd_rs1,
	/*                  not */ rv_fmt_rd_rs1,
	/*                  neg */ rv_fmt_rd_rs2,
	/*                 negw */ rv_fmt_rd_rs2,
	/*               sext.w */ rv_fmt_rd_rs1,
	/*                 seqz */ rv_fmt_rd_rs1,
	/*                 snez */ rv_fmt_rd_rs2,
	/*                 sltz */ rv_fmt_rd_rs1,
	/*                 sgtz */ rv_fmt_rd_rs2,
	/*                fmv.s */ rv_fmt_rd_rs1,
	/*               fabs.s */ rv_fmt_rd_rs1,
	/*               fneg.s */ rv_fmt_rd_rs1,
	/*                fmv.d */ rv_fmt_rd_rs1,
	/*               fabs.d */ rv_fmt_rd_rs1,
	/*               fneg.d */ rv_fmt_rd_rs1,
	/*                fmv.q */ rv_fmt_rd_rs1,
	/*               fabs.q */ rv_fmt_rd_rs1,
	/*               fneg.q */ rv_fmt_rd_rs1,
	/*                 beqz */ rv_fmt_rs1_offset,
	/*                 bnez */ rv_fmt_rs1_offset,
	/*                 blez */ rv_fmt_rs2_offset,
	/*                 bgez */ rv_fmt_rs1_offset,
	/*                 bltz */ rv_fmt_rs1_offset,
	/*                 bgtz */ rv_fmt_rs2_offset,
	/*                  ble */ rv_fmt_rs2_rs1_offset,
	/*                 bleu */ rv_fmt_rs2_rs1_offset,
	/*                  bgt */ rv_fmt_rs2_rs1_offset,
	/*                 bgtu */ rv_fmt_rs2_rs1_offset,
	/*                    j */ rv_fmt_offset,
	/*                  ret */ rv_fmt_none,
	/*                   jr */ rv_fmt_rs1,
	/*              rdcycle */ rv_fmt_rd,
	/*               rdtime */ rv_fmt_rd,
	/*            rdinstret */ rv_fmt_rd,
	/*             rdcycleh */ rv_fmt_rd,
	/*              rdtimeh */ rv_fmt_rd,
	/*           rdinstreth */ rv_fmt_rd,
	/*                frcsr */ rv_fmt_rd,
	/*                 frrm */ rv_fmt_rd,
	/*              frflags */ rv_fmt_rd,
	/*                fscsr */ rv_fmt_rd_rs1,
	/*                 fsrm */ rv_fmt_rd_rs1,
	/*              fsflags */ rv_fmt_rd_rs1,
	/*                fsrmi */ rv_fmt_rd_zimm,
	/*             fsflagsi */ rv_fmt_rd_zimm,
};

const rv_operand_data* rv_inst_operand_data[] = {
	/*              unknown */ rv_operands_none,
	/*                  lui */ rv_operands_sx_rd_T_imm20,
	/*                auipc */ rv_operands_sx_rd_T_oimm20,
	/*                  jal */ rv_operands_sx_rd_T_jimm20,
	/*                 jalr */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                  beq */ rv_operands_sx_rs1_sx_rs2_T_sbimm12,
	/*                  bne */ rv_operands_sx_rs1_sx_rs2_T_sbimm12,
	/*                  blt */ rv_operands_sx_rs1_sx_rs2_T_sbimm12,
	/*                  bge */ rv_operands_sx_rs1_sx_rs2_T_sbimm12,
	/*                 bltu */ rv_operands_sx_rs1_sx_rs2_T_sbimm12,
	/*                 bgeu */ rv_operands_sx_rs1_sx_rs2_T_sbimm12,
	/*                   lb */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   lh */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   lw */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                  lbu */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                  lhu */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   sb */ rv_operands_sx_rs1_sx_rs2_T_simm12,
	/*                   sh */ rv_operands_sx_rs1_sx_rs2_T_simm12,
	/*                   sw */ rv_operands_sx_rs1_sx_rs2_T_simm12,
	/*                 addi */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                 slti */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                sltiu */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                 xori */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                  ori */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                 andi */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                 slli */ rv_operands_sx_rd_sx_rs1_T_shamt7,
	/*                 srli */ rv_operands_sx_rd_sx_rs1_T_shamt7,
	/*                 srai */ rv_operands_sx_rd_sx_rs1_T_shamt7,
	/*                  add */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  sub */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  sll */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  slt */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 sltu */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  xor */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  srl */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  sra */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                   or */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  and */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                fence */ rv_operands_T_pred_T_succ,
	/*              fence.i */ rv_operands_none,
	/*                  lwu */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   ld */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   sd */ rv_operands_sx_rs1_sx_rs2_T_simm12,
	/*                addiw */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                slliw */ rv_operands_sx_rd_sx_rs1_T_shamt5,
	/*                srliw */ rv_operands_sx_rd_sx_rs1_T_shamt5,
	/*                sraiw */ rv_operands_sx_rd_sx_rs1_T_shamt5,
	/*                 addw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 subw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 sllw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 srlw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 sraw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  ldu */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   lq */ rv_operands_sx_rd_sx_rs1_T_oimm12,
	/*                   sq */ rv_operands_sx_rs1_sx_rs2_T_simm12,
	/*                addid */ rv_operands_sx_rd_sx_rs1_T_imm12,
	/*                sllid */ rv_operands_sx_rd_sx_rs1_T_shamt6,
	/*                srlid */ rv_operands_sx_rd_sx_rs1_T_shamt6,
	/*                sraid */ rv_operands_sx_rd_sx_rs1_T_shamt6,
	/*                 addd */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 subd */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 slld */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 srld */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 srad */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  mul */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 mulh */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*               mulhsu */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                mulhu */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  div */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 divu */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                  rem */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 remu */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 mulw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 divw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                divuw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 remw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                remuw */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 muld */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 divd */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                divud */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 remd */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                remud */ rv_operands_sx_rd_sx_rs1_sx_rs2,
	/*                 lr.w */ rv_operands_sx_rd_sx_rs1_T_aq_T_rl,
	/*                 sc.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amoswap.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoadd.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoxor.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*              amoor.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoand.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amomin.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amomax.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amominu.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amomaxu.w */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*                 lr.d */ rv_operands_sx_rd_sx_rs1_T_aq_T_rl,
	/*                 sc.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amoswap.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoadd.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoxor.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*              amoor.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoand.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amomin.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amomax.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amominu.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amomaxu.d */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*                 lr.q */ rv_operands_sx_rd_sx_rs1_T_aq_T_rl,
	/*                 sc.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amoswap.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoadd.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoxor.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*              amoor.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amoand.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amomin.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*             amomax.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amominu.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*            amomaxu.q */ rv_operands_sx_rd_sx_rs1_sx_rs2_T_aq_T_rl,
	/*                ecall */ rv_operands_none,
	/*               ebreak */ rv_operands_none,
	/*                 uret */ rv_operands_none,
	/*                 sret */ rv_operands_none,
	/*                 hret */ rv_operands_none,
	/*                 mret */ rv_operands_none,
	/*                 dret */ rv_operands_none,
	/*            sfence.vm */ rv_operands_sx_rs1,
	/*                  wfi */ rv_operands_none,
	/*                csrrw */ rv_operands_sx_rd_sx_rs1_T_csr12,
	/*                csrrs */ rv_operands_sx_rd_sx_rs1_T_csr12,
	/*                csrrc */ rv_operands_sx_rd_sx_rs1_T_csr12,
	/*               csrrwi */ rv_operands_sx_rd_T_zimm_T_csr12,
	/*               csrrsi */ rv_operands_sx_rd_T_zimm_T_csr12,
	/*               csrrci */ rv_operands_sx_rd_T_zimm_T_csr12,
	/*                  flw */ rv_operands_f32_frd_sx_rs1_T_oimm12,
	/*                  fsw */ rv_operands_sx_rs1_f32_frs2_T_simm12,
	/*              fmadd.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_f32_frs3_T_rm,
	/*              fmsub.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_f32_frs3_T_rm,
	/*             fnmsub.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_f32_frs3_T_rm,
	/*             fnmadd.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_f32_frs3_T_rm,
	/*               fadd.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_T_rm,
	/*               fsub.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_T_rm,
	/*               fmul.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_T_rm,
	/*               fdiv.s */ rv_operands_f32_frd_f32_frs1_f32_frs2_T_rm,
	/*              fsgnj.s */ rv_operands_f32_frd_f32_frs1_f32_frs2,
	/*             fsgnjn.s */ rv_operands_f32_frd_f32_frs1_f32_frs2,
	/*             fsgnjx.s */ rv_operands_f32_frd_f32_frs1_f32_frs2,
	/*               fmin.s */ rv_operands_f32_frd_f32_frs1_f32_frs2,
	/*               fmax.s */ rv_operands_f32_frd_f32_frs1_f32_frs2,
	/*              fsqrt.s */ rv_operands_f32_frd_f32_frs1_T_rm,
	/*                fle.s */ rv_operands_sx_rd_f32_frs1_f32_frs2,
	/*                flt.s */ rv_operands_sx_rd_f32_frs1_f32_frs2,
	/*                feq.s */ rv_operands_sx_rd_f32_frs1_f32_frs2,
	/*             fcvt.w.s */ rv_operands_s32_rd_f32_frs1_T_rm,
	/*            fcvt.wu.s */ rv_operands_u32_rd_f32_frs1_T_rm,
	/*             fcvt.s.w */ rv_operands_f32_frd_s32_rs1_T_rm,
	/*            fcvt.s.wu */ rv_operands_f32_frd_u32_rs1_T_rm,
	/*              fmv.x.s */ rv_operands_sx_rd_f32_frs1,
	/*             fclass.s */ rv_operands_sx_rd_f32_frs1,
	/*              fmv.s.x */ rv_operands_f32_frd_sx_rs1,
	/*             fcvt.l.s */ rv_operands_s64_rd_f32_frs1_T_rm,
	/*            fcvt.lu.s */ rv_operands_u64_rd_f32_frs1_T_rm,
	/*             fcvt.s.l */ rv_operands_f32_frd_s64_rs1_T_rm,
	/*            fcvt.s.lu */ rv_operands_f32_frd_u64_rs1_T_rm,
	/*                  fld */ rv_operands_f64_frd_sx_rs1_T_oimm12,
	/*                  fsd */ rv_operands_sx_rs1_f64_frs2_T_simm12,
	/*              fmadd.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_f64_frs3_T_rm,
	/*              fmsub.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_f64_frs3_T_rm,
	/*             fnmsub.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_f64_frs3_T_rm,
	/*             fnmadd.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_f64_frs3_T_rm,
	/*               fadd.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_T_rm,
	/*               fsub.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_T_rm,
	/*               fmul.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_T_rm,
	/*               fdiv.d */ rv_operands_f64_frd_f64_frs1_f64_frs2_T_rm,
	/*              fsgnj.d */ rv_operands_f64_frd_f64_frs1_f64_frs2,
	/*             fsgnjn.d */ rv_operands_f64_frd_f64_frs1_f64_frs2,
	/*             fsgnjx.d */ rv_operands_f64_frd_f64_frs1_f64_frs2,
	/*               fmin.d */ rv_operands_f64_frd_f64_frs1_f64_frs2,
	/*               fmax.d */ rv_operands_f64_frd_f64_frs1_f64_frs2,
	/*             fcvt.s.d */ rv_operands_f32_frd_f64_frs1_T_rm,
	/*             fcvt.d.s */ rv_operands_f64_frd_f32_frs1_T_rm,
	/*              fsqrt.d */ rv_operands_f64_frd_f64_frs1_T_rm,
	/*                fle.d */ rv_operands_sx_rd_f64_frs1_f64_frs2,
	/*                flt.d */ rv_operands_sx_rd_f64_frs1_f64_frs2,
	/*                feq.d */ rv_operands_sx_rd_f64_frs1_f64_frs2,
	/*             fcvt.w.d */ rv_operands_s32_rd_f64_frs1_T_rm,
	/*            fcvt.wu.d */ rv_operands_u32_rd_f64_frs1_T_rm,
	/*             fcvt.d.w */ rv_operands_f64_frd_s32_rs1_T_rm,
	/*            fcvt.d.wu */ rv_operands_f64_frd_u32_rs1_T_rm,
	/*             fclass.d */ rv_operands_sx_rd_f64_frs1,
	/*             fcvt.l.d */ rv_operands_s64_rd_f64_frs1_T_rm,
	/*            fcvt.lu.d */ rv_operands_u64_rd_f64_frs1_T_rm,
	/*              fmv.x.d */ rv_operands_sx_rd_f64_frs1,
	/*             fcvt.d.l */ rv_operands_f64_frd_s64_rs1_T_rm,
	/*            fcvt.d.lu */ rv_operands_f64_frd_u64_rs1_T_rm,
	/*              fmv.d.x */ rv_operands_f64_frd_sx_rs1,
	/*                  flq */ rv_operands_f128_frd_sx_rs1_T_oimm12,
	/*                  fsq */ rv_operands_sx_rs1_f128_frs2_T_simm12,
	/*              fmadd.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_f128_frs3_T_rm,
	/*              fmsub.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_f128_frs3_T_rm,
	/*             fnmsub.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_f128_frs3_T_rm,
	/*             fnmadd.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_f128_frs3_T_rm,
	/*               fadd.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_T_rm,
	/*               fsub.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_T_rm,
	/*               fmul.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_T_rm,
	/*               fdiv.q */ rv_operands_f128_frd_f128_frs1_f128_frs2_T_rm,
	/*              fsgnj.q */ rv_operands_f128_frd_f128_frs1_f128_frs2,
	/*             fsgnjn.q */ rv_operands_f128_frd_f128_frs1_f128_frs2,
	/*             fsgnjx.q */ rv_operands_f128_frd_f128_frs1_f128_frs2,
	/*               fmin.q */ rv_operands_f128_frd_f128_frs1_f128_frs2,
	/*               fmax.q */ rv_operands_f128_frd_f128_frs1_f128_frs2,
	/*             fcvt.s.q */ rv_operands_f32_frd_f128_frs1_T_rm,
	/*             fcvt.q.s */ rv_operands_f128_frd_f32_frs1_T_rm,
	/*             fcvt.d.q */ rv_operands_f64_frd_f128_frs1_T_rm,
	/*             fcvt.q.d */ rv_operands_f128_frd_f64_frs1_T_rm,
	/*              fsqrt.q */ rv_operands_f128_frd_f128_frs1_T_rm,
	/*                fle.q */ rv_operands_sx_rd_f128_frs1_f128_frs2,
	/*                flt.q */ rv_operands_sx_rd_f128_frs1_f128_frs2,
	/*                feq.q */ rv_operands_sx_rd_f128_frs1_f128_frs2,
	/*             fcvt.w.q */ rv_operands_s32_rd_f128_frs1_T_rm,
	/*            fcvt.wu.q */ rv_operands_u32_rd_f128_frs1_T_rm,
	/*             fcvt.q.w */ rv_operands_f128_frd_s32_rs1_T_rm,
	/*            fcvt.q.wu */ rv_operands_f128_frd_u32_rs1_T_rm,
	/*             fclass.q */ rv_operands_sx_rd_f128_frs1,
	/*             fcvt.l.q */ rv_operands_s64_rd_f128_frs1_T_rm,
	/*            fcvt.lu.q */ rv_operands_u64_rd_f128_frs1_T_rm,
	/*             fcvt.q.l */ rv_operands_f128_frd_s64_rs1_T_rm,
	/*            fcvt.q.lu */ rv_operands_f128_frd_u64_rs1_T_rm,
	/*              fmv.x.q */ rv_operands_sx_rd_f128_frs1,
	/*              fmv.q.x */ rv_operands_f128_frd_sx_rs1,
	/*           c.addi4spn */ rv_operands_T_crdq_T_cimm4spn,
	/*                c.fld */ rv_operands_T_cfrdq_T_crs1q_T_cimmd,
	/*                 c.lw */ rv_operands_T_crdq_T_crs1q_T_cimmw,
	/*                c.flw */ rv_operands_T_cfrdq_T_crs1q_T_cimmw,
	/*                c.fsd */ rv_operands_T_crs1q_T_cfrs2q_T_cimmd,
	/*                 c.sw */ rv_operands_T_crs1q_T_crs2q_T_cimmw,
	/*                c.fsw */ rv_operands_T_crs1q_T_cfrs2q_T_cimmw,
	/*                c.nop */ rv_operands_none,
	/*               c.addi */ rv_operands_sx_crs1rd_T_cnzimmi,
	/*                c.jal */ rv_operands_T_cimmj,
	/*                 c.li */ rv_operands_sx_crs1rd_T_cimmi,
	/*           c.addi16sp */ rv_operands_sx_crs1rd_T_cimm16sp,
	/*                c.lui */ rv_operands_sx_crd_T_cimmui,
	/*               c.srli */ rv_operands_T_crs1rdq_T_cimmsh6,
	/*               c.srai */ rv_operands_T_crs1rdq_T_cimmsh6,
	/*               c.andi */ rv_operands_T_crs1rdq_T_cnzimmi,
	/*                c.sub */ rv_operands_T_crs1rdq_T_crs2q,
	/*                c.xor */ rv_operands_T_crs1rdq_T_crs2q,
	/*                 c.or */ rv_operands_T_crs1rdq_T_crs2q,
	/*                c.and */ rv_operands_T_crs1rdq_T_crs2q,
	/*               c.subw */ rv_operands_T_crs1rdq_T_crs2q,
	/*               c.addw */ rv_operands_T_crs1rdq_T_crs2q,
	/*                  c.j */ rv_operands_T_cimmj,
	/*               c.beqz */ rv_operands_T_crs1q_T_cimmb,
	/*               c.bnez */ rv_operands_T_crs1q_T_cimmb,
	/*               c.slli */ rv_operands_sx_crs1rd_T_cimmsh6,
	/*              c.fldsp */ rv_operands_f32_cfrd_T_cimmldsp,
	/*               c.lwsp */ rv_operands_sx_crd_T_cimmlwsp,
	/*              c.flwsp */ rv_operands_f32_cfrd_T_cimmlwsp,
	/*                 c.jr */ rv_operands_T_crd0_sx_crs1,
	/*                 c.mv */ rv_operands_sx_crd_sx_crs2,
	/*             c.ebreak */ rv_operands_none,
	/*               c.jalr */ rv_operands_T_crd0_sx_crs1,
	/*                c.add */ rv_operands_sx_crs1rd_sx_crs2,
	/*              c.fsdsp */ rv_operands_f32_cfrs2_T_cimmsdsp,
	/*               c.swsp */ rv_operands_sx_crs2_T_cimmswsp,
	/*              c.fswsp */ rv_operands_f32_cfrs2_T_cimmswsp,
	/*                 c.ld */ rv_operands_T_crdq_T_crs1q_T_cimmd,
	/*                 c.sd */ rv_operands_T_crs1q_T_crs2q_T_cimmd,
	/*              c.addiw */ rv_operands_sx_crs1rd_T_cimmi,
	/*               c.ldsp */ rv_operands_sx_crd_T_cimmldsp,
	/*               c.sdsp */ rv_operands_sx_crs2_T_cimmsdsp,
	/*                 c.lq */ rv_operands_T_crdq_T_crs1q_T_cimmq,
	/*                 c.sq */ rv_operands_T_crs1q_T_crs2q_T_cimmq,
	/*               c.lqsp */ rv_operands_sx_crd_T_cimmlqsp,
	/*               c.sqsp */ rv_operands_sx_crs2_T_cimmsqsp,
	/*                  nop */ rv_operands_none,
	/*                   mv */ rv_operands_sx_rd_sx_rs1,
	/*                  not */ rv_operands_sx_rd_sx_rs1,
	/*                  neg */ rv_operands_sx_rd_sx_rs2,
	/*                 negw */ rv_operands_sx_rd_sx_rs2,
	/*               sext.w */ rv_operands_sx_rd_sx_rs1,
	/*                 seqz */ rv_operands_sx_rd_sx_rs1,
	/*                 snez */ rv_operands_sx_rd_sx_rs2,
	/*                 sltz */ rv_operands_sx_rd_sx_rs1,
	/*                 sgtz */ rv_operands_sx_rd_sx_rs2,
	/*                fmv.s */ rv_operands_sx_rd_sx_rs1,
	/*               fabs.s */ rv_operands_sx_rd_sx_rs1,
	/*               fneg.s */ rv_operands_sx_rd_sx_rs1,
	/*                fmv.d */ rv_operands_sx_rd_sx_rs1,
	/*               fabs.d */ rv_operands_sx_rd_sx_rs1,
	/*               fneg.d */ rv_operands_sx_rd_sx_rs1,
	/*                fmv.q */ rv_operands_sx_rd_sx_rs1,
	/*               fabs.q */ rv_operands_sx_rd_sx_rs1,
	/*               fneg.q */ rv_operands_sx_rd_sx_rs1,
	/*                 beqz */ rv_operands_sx_rs1_T_oimm20,
	/*                 bnez */ rv_operands_sx_rs1_T_oimm20,
	/*                 blez */ rv_operands_sx_rs2_T_oimm20,
	/*                 bgez */ rv_operands_sx_rs1_T_oimm20,
	/*                 bltz */ rv_operands_sx_rs1_T_oimm20,
	/*                 bgtz */ rv_operands_sx_rs2_T_oimm20,
	/*                  ble */ rv_operands_sx_rs2_sx_rs1_T_oimm20,
	/*                 bleu */ rv_operands_sx_rs2_sx_rs1_T_oimm20,
	/*                  bgt */ rv_operands_sx_rs2_sx_rs1_T_oimm20,
	/*                 bgtu */ rv_operands_sx_rs2_sx_rs1_T_oimm20,
	/*                    j */ rv_operands_T_oimm20,
	/*                  ret */ rv_operands_none,
	/*                   jr */ rv_operands_sx_rs1,
	/*              rdcycle */ rv_operands_sx_rd,
	/*               rdtime */ rv_operands_sx_rd,
	/*            rdinstret */ rv_operands_sx_rd,
	/*             rdcycleh */ rv_operands_sx_rd,
	/*              rdtimeh */ rv_operands_sx_rd,
	/*           rdinstreth */ rv_operands_sx_rd,
	/*                frcsr */ rv_operands_sx_rd,
	/*                 frrm */ rv_operands_sx_rd,
	/*              frflags */ rv_operands_sx_rd,
	/*                fscsr */ rv_operands_sx_rd_sx_rs1,
	/*                 fsrm */ rv_operands_sx_rd_sx_rs1,
	/*              fsflags */ rv_operands_sx_rd_sx_rs1,
	/*                fsrmi */ rv_operands_sx_rd_T_zimm,
	/*             fsflagsi */ rv_operands_sx_rd_T_zimm,
};

const riscv::inst_t rv_inst_match[] = {
	/*              unknown */ 0x0000000000000000,
	/*                  lui */ 0x0000000000000037,
	/*                auipc */ 0x0000000000000017,
	/*                  jal */ 0x000000000000006f,
	/*                 jalr */ 0x0000000000000067,
	/*                  beq */ 0x0000000000000063,
	/*                  bne */ 0x0000000000001063,
	/*                  blt */ 0x0000000000004063,
	/*                  bge */ 0x0000000000005063,
	/*                 bltu */ 0x0000000000006063,
	/*                 bgeu */ 0x0000000000007063,
	/*                   lb */ 0x0000000000000003,
	/*                   lh */ 0x0000000000001003,
	/*                   lw */ 0x0000000000002003,
	/*                  lbu */ 0x0000000000004003,
	/*                  lhu */ 0x0000000000005003,
	/*                   sb */ 0x0000000000000023,
	/*                   sh */ 0x0000000000001023,
	/*                   sw */ 0x0000000000002023,
	/*                 addi */ 0x0000000000000013,
	/*                 slti */ 0x0000000000002013,
	/*                sltiu */ 0x0000000000003013,
	/*                 xori */ 0x0000000000004013,
	/*                  ori */ 0x0000000000006013,
	/*                 andi */ 0x0000000000007013,
	/*                 slli */ 0x0000000000001013,
	/*                 srli */ 0x0000000000005013,
	/*                 srai */ 0x0000000040005013,
	/*                  add */ 0x0000000000000033,
	/*                  sub */ 0x0000000040000033,
	/*                  sll */ 0x0000000000001033,
	/*                  slt */ 0x0000000000002033,
	/*                 sltu */ 0x0000000000003033,
	/*                  xor */ 0x0000000000004033,
	/*                  srl */ 0x0000000000005033,
	/*                  sra */ 0x0000000040005033,
	/*                   or */ 0x0000000000006033,
	/*                  and */ 0x0000000000007033,
	/*                fence */ 0x000000000000000f,
	/*              fence.i */ 0x000000000000100f,
	/*                  lwu */ 0x0000000000006003,
	/*                   ld */ 0x0000000000003003,
	/*                   sd */ 0x0000000000003023,
	/*                addiw */ 0x000000000000001b,
	/*                slliw */ 0x000000000000101b,
	/*                srliw */ 0x000000000000501b,
	/*                sraiw */ 0x000000004000501b,
	/*                 addw */ 0x000000000000003b,
	/*                 subw */ 0x000000004000003b,
	/*                 sllw */ 0x000000000000103b,
	/*                 srlw */ 0x000000000000503b,
	/*                 sraw */ 0x000000004000503b,
	/*                  ldu */ 0x0000000000007003,
	/*                   lq */ 0x000000000000200f,
	/*                   sq */ 0x0000000000004023,
	/*                addid */ 0x000000000000005b,
	/*                sllid */ 0x000000000000105b,
	/*                srlid */ 0x000000000000505b,
	/*                sraid */ 0x000000004000505b,
	/*                 addd */ 0x000000000000007b,
	/*                 subd */ 0x000000004000007b,
	/*                 slld */ 0x000000000000107b,
	/*                 srld */ 0x000000000000507b,
	/*                 srad */ 0x000000004000507b,
	/*                  mul */ 0x0000000002000033,
	/*                 mulh */ 0x0000000002001033,
	/*               mulhsu */ 0x0000000002002033,
	/*                mulhu */ 0x0000000002003033,
	/*                  div */ 0x0000000002004033,
	/*                 divu */ 0x0000000002005033,
	/*                  rem */ 0x0000000002006033,
	/*                 remu */ 0x0000000002007033,
	/*                 mulw */ 0x000000000200003b,
	/*                 divw */ 0x000000000200403b,
	/*                divuw */ 0x000000000200503b,
	/*                 remw */ 0x000000000200603b,
	/*                remuw */ 0x000000000200703b,
	/*                 muld */ 0x000000000200007b,
	/*                 divd */ 0x000000000200407b,
	/*                divud */ 0x000000000200507b,
	/*                 remd */ 0x000000000200607b,
	/*                remud */ 0x000000000200707b,
	/*                 lr.w */ 0x000000001000202f,
	/*                 sc.w */ 0x000000001800202f,
	/*            amoswap.w */ 0x000000000800202f,
	/*             amoadd.w */ 0x000000000000202f,
	/*             amoxor.w */ 0x000000002000202f,
	/*              amoor.w */ 0x000000004000202f,
	/*             amoand.w */ 0x000000006000202f,
	/*             amomin.w */ 0x000000008000202f,
	/*             amomax.w */ 0x00000000a000202f,
	/*            amominu.w */ 0x00000000c000202f,
	/*            amomaxu.w */ 0x00000000e000202f,
	/*                 lr.d */ 0x000000001000302f,
	/*                 sc.d */ 0x000000001800302f,
	/*            amoswap.d */ 0x000000000800302f,
	/*             amoadd.d */ 0x000000000000302f,
	/*             amoxor.d */ 0x000000002000302f,
	/*              amoor.d */ 0x000000004000302f,
	/*             amoand.d */ 0x000000006000302f,
	/*             amomin.d */ 0x000000008000302f,
	/*             amomax.d */ 0x00000000a000302f,
	/*            amominu.d */ 0x00000000c000302f,
	/*            amomaxu.d */ 0x00000000e000302f,
	/*                 lr.q */ 0x000000001000402f,
	/*                 sc.q */ 0x000000001800402f,
	/*            amoswap.q */ 0x000000000800402f,
	/*             amoadd.q */ 0x000000000000402f,
	/*             amoxor.q */ 0x000000002000402f,
	/*              amoor.q */ 0x000000004000402f,
	/*             amoand.q */ 0x000000006000402f,
	/*             amomin.q */ 0x000000008000402f,
	/*             amomax.q */ 0x00000000a000402f,
	/*            amominu.q */ 0x00000000c000402f,
	/*            amomaxu.q */ 0x00000000e000402f,
	/*                ecall */ 0x0000000000000073,
	/*               ebreak */ 0x0000000000100073,
	/*                 uret */ 0x0000000000200073,
	/*                 sret */ 0x0000000010200073,
	/*                 hret */ 0x0000000020200073,
	/*                 mret */ 0x0000000030200073,
	/*                 dret */ 0x000000007b200073,
	/*            sfence.vm */ 0x0000000010400073,
	/*                  wfi */ 0x0000000010500073,
	/*                csrrw */ 0x0000000000001073,
	/*                csrrs */ 0x0000000000002073,
	/*                csrrc */ 0x0000000000003073,
	/*               csrrwi */ 0x0000000000005073,
	/*               csrrsi */ 0x0000000000006073,
	/*               csrrci */ 0x0000000000007073,
	/*                  flw */ 0x0000000000002007,
	/*                  fsw */ 0x0000000000002027,
	/*              fmadd.s */ 0x0000000000000043,
	/*              fmsub.s */ 0x0000000000000047,
	/*             fnmsub.s */ 0x000000000000004b,
	/*             fnmadd.s */ 0x000000000000004f,
	/*               fadd.s */ 0x0000000000000053,
	/*               fsub.s */ 0x0000000008000053,
	/*               fmul.s */ 0x0000000010000053,
	/*               fdiv.s */ 0x0000000018000053,
	/*              fsgnj.s */ 0x0000000020000053,
	/*             fsgnjn.s */ 0x0000000020001053,
	/*             fsgnjx.s */ 0x0000000020002053,
	/*               fmin.s */ 0x0000000028000053,
	/*               fmax.s */ 0x0000000028001053,
	/*              fsqrt.s */ 0x0000000058000053,
	/*                fle.s */ 0x00000000a0000053,
	/*                flt.s */ 0x00000000a0001053,
	/*                feq.s */ 0x00000000a0002053,
	/*             fcvt.w.s */ 0x00000000c0000053,
	/*            fcvt.wu.s */ 0x00000000c0100053,
	/*             fcvt.s.w */ 0x00000000d0000053,
	/*            fcvt.s.wu */ 0x00000000d0100053,
	/*              fmv.x.s */ 0x00000000e0000053,
	/*             fclass.s */ 0x00000000e0001053,
	/*              fmv.s.x */ 0x00000000f0000053,
	/*             fcvt.l.s */ 0x00000000c0200053,
	/*            fcvt.lu.s */ 0x00000000c0300053,
	/*             fcvt.s.l */ 0x00000000d0200053,
	/*            fcvt.s.lu */ 0x00000000d0300053,
	/*                  fld */ 0x0000000000003007,
	/*                  fsd */ 0x0000000000003027,
	/*              fmadd.d */ 0x0000000002000043,
	/*              fmsub.d */ 0x0000000002000047,
	/*             fnmsub.d */ 0x000000000200004b,
	/*             fnmadd.d */ 0x000000000200004f,
	/*               fadd.d */ 0x0000000002000053,
	/*               fsub.d */ 0x000000000a000053,
	/*               fmul.d */ 0x0000000012000053,
	/*               fdiv.d */ 0x000000001a000053,
	/*              fsgnj.d */ 0x0000000022000053,
	/*             fsgnjn.d */ 0x0000000022001053,
	/*             fsgnjx.d */ 0x0000000022002053,
	/*               fmin.d */ 0x000000002a000053,
	/*               fmax.d */ 0x000000002a001053,
	/*             fcvt.s.d */ 0x0000000040100053,
	/*             fcvt.d.s */ 0x0000000042000053,
	/*              fsqrt.d */ 0x000000005a000053,
	/*                fle.d */ 0x00000000a2000053,
	/*                flt.d */ 0x00000000a2001053,
	/*                feq.d */ 0x00000000a2002053,
	/*             fcvt.w.d */ 0x00000000c2000053,
	/*            fcvt.wu.d */ 0x00000000c2100053,
	/*             fcvt.d.w */ 0x00000000d2000053,
	/*            fcvt.d.wu */ 0x00000000d2100053,
	/*             fclass.d */ 0x00000000e2001053,
	/*             fcvt.l.d */ 0x00000000c2200053,
	/*            fcvt.lu.d */ 0x00000000c2300053,
	/*              fmv.x.d */ 0x00000000e2000053,
	/*             fcvt.d.l */ 0x00000000d2200053,
	/*            fcvt.d.lu */ 0x00000000d2300053,
	/*              fmv.d.x */ 0x00000000f2000053,
	/*                  flq */ 0x0000000000004007,
	/*                  fsq */ 0x0000000000004027,
	/*              fmadd.q */ 0x0000000006000043,
	/*              fmsub.q */ 0x0000000006000047,
	/*             fnmsub.q */ 0x000000000600004b,
	/*             fnmadd.q */ 0x000000000600004f,
	/*               fadd.q */ 0x0000000006000053,
	/*               fsub.q */ 0x000000000e000053,
	/*               fmul.q */ 0x0000000016000053,
	/*               fdiv.q */ 0x000000001e000053,
	/*              fsgnj.q */ 0x0000000026000053,
	/*             fsgnjn.q */ 0x0000000026001053,
	/*             fsgnjx.q */ 0x0000000026002053,
	/*               fmin.q */ 0x000000002e000053,
	/*               fmax.q */ 0x000000002e001053,
	/*             fcvt.s.q */ 0x0000000040300053,
	/*             fcvt.q.s */ 0x0000000046000053,
	/*             fcvt.d.q */ 0x0000000042300053,
	/*             fcvt.q.d */ 0x0000000046100053,
	/*              fsqrt.q */ 0x000000005e000053,
	/*                fle.q */ 0x00000000a6000053,
	/*                flt.q */ 0x00000000a6001053,
	/*                feq.q */ 0x00000000a6002053,
	/*             fcvt.w.q */ 0x00000000c6000053,
	/*            fcvt.wu.q */ 0x00000000c6100053,
	/*             fcvt.q.w */ 0x00000000d6000053,
	/*            fcvt.q.wu */ 0x00000000d6100053,
	/*             fclass.q */ 0x00000000e6001053,
	/*             fcvt.l.q */ 0x00000000c6200053,
	/*            fcvt.lu.q */ 0x00000000c6300053,
	/*             fcvt.q.l */ 0x00000000d6200053,
	/*            fcvt.q.lu */ 0x00000000d6300053,
	/*              fmv.x.q */ 0x00000000e6000053,
	/*              fmv.q.x */ 0x00000000f6000053,
	/*           c.addi4spn */ 0x0000000000000000,
	/*                c.fld */ 0x0000000000002000,
	/*                 c.lw */ 0x0000000000004000,
	/*                c.flw */ 0x0000000000006000,
	/*                c.fsd */ 0x000000000000a000,
	/*                 c.sw */ 0x000000000000c000,
	/*                c.fsw */ 0x000000000000e000,
	/*                c.nop */ 0x0000000000000001,
	/*               c.addi */ 0x0000000000000001,
	/*                c.jal */ 0x0000000000002001,
	/*                 c.li */ 0x0000000000004001,
	/*           c.addi16sp */ 0x0000000000006101,
	/*                c.lui */ 0x0000000000006001,
	/*               c.srli */ 0x0000000000008001,
	/*               c.srai */ 0x0000000000008401,
	/*               c.andi */ 0x0000000000008801,
	/*                c.sub */ 0x0000000000008c01,
	/*                c.xor */ 0x0000000000008c21,
	/*                 c.or */ 0x0000000000008c41,
	/*                c.and */ 0x0000000000008c61,
	/*               c.subw */ 0x0000000000009c01,
	/*               c.addw */ 0x0000000000009c21,
	/*                  c.j */ 0x000000000000a001,
	/*               c.beqz */ 0x000000000000c001,
	/*               c.bnez */ 0x000000000000e001,
	/*               c.slli */ 0x0000000000000002,
	/*              c.fldsp */ 0x0000000000002002,
	/*               c.lwsp */ 0x0000000000004002,
	/*              c.flwsp */ 0x0000000000006002,
	/*                 c.jr */ 0x0000000000008002,
	/*                 c.mv */ 0x0000000000008002,
	/*             c.ebreak */ 0x0000000000009002,
	/*               c.jalr */ 0x0000000000009002,
	/*                c.add */ 0x0000000000009002,
	/*              c.fsdsp */ 0x000000000000a002,
	/*               c.swsp */ 0x000000000000c002,
	/*              c.fswsp */ 0x000000000000e002,
	/*                 c.ld */ 0x0000000000006000,
	/*                 c.sd */ 0x000000000000e000,
	/*              c.addiw */ 0x0000000000002001,
	/*               c.ldsp */ 0x0000000000006002,
	/*               c.sdsp */ 0x000000000000e002,
	/*                 c.lq */ 0x0000000000002000,
	/*                 c.sq */ 0x000000000000a000,
	/*               c.lqsp */ 0x0000000000002002,
	/*               c.sqsp */ 0x000000000000a002,
	/*                  nop */ 0x0000000000000000,
	/*                   mv */ 0x0000000000000000,
	/*                  not */ 0x0000000000000000,
	/*                  neg */ 0x0000000000000000,
	/*                 negw */ 0x0000000000000000,
	/*               sext.w */ 0x0000000000000000,
	/*                 seqz */ 0x0000000000000000,
	/*                 snez */ 0x0000000000000000,
	/*                 sltz */ 0x0000000000000000,
	/*                 sgtz */ 0x0000000000000000,
	/*                fmv.s */ 0x0000000000000000,
	/*               fabs.s */ 0x0000000000000000,
	/*               fneg.s */ 0x0000000000000000,
	/*                fmv.d */ 0x0000000000000000,
	/*               fabs.d */ 0x0000000000000000,
	/*               fneg.d */ 0x0000000000000000,
	/*                fmv.q */ 0x0000000000000000,
	/*               fabs.q */ 0x0000000000000000,
	/*               fneg.q */ 0x0000000000000000,
	/*                 beqz */ 0x0000000000000000,
	/*                 bnez */ 0x0000000000000000,
	/*                 blez */ 0x0000000000000000,
	/*                 bgez */ 0x0000000000000000,
	/*                 bltz */ 0x0000000000000000,
	/*                 bgtz */ 0x0000000000000000,
	/*                  ble */ 0x0000000000000000,
	/*                 bleu */ 0x0000000000000000,
	/*                  bgt */ 0x0000000000000000,
	/*                 bgtu */ 0x0000000000000000,
	/*                    j */ 0x0000000000000000,
	/*                  ret */ 0x0000000000000000,
	/*                   jr */ 0x0000000000000000,
	/*              rdcycle */ 0x0000000000000000,
	/*               rdtime */ 0x0000000000000000,
	/*            rdinstret */ 0x0000000000000000,
	/*             rdcycleh */ 0x0000000000000000,
	/*              rdtimeh */ 0x0000000000000000,
	/*           rdinstreth */ 0x0000000000000000,
	/*                frcsr */ 0x0000000000000000,
	/*                 frrm */ 0x0000000000000000,
	/*              frflags */ 0x0000000000000000,
	/*                fscsr */ 0x0000000000000000,
	/*                 fsrm */ 0x0000000000000000,
	/*              fsflags */ 0x0000000000000000,
	/*                fsrmi */ 0x0000000000000000,
	/*             fsflagsi */ 0x0000000000000000,
};

const riscv::inst_t rv_inst_mask[] = {
	/*              unknown */ 0x0000000000000000,
	/*                  lui */ 0x000000000000007f,
	/*                auipc */ 0x000000000000007f,
	/*                  jal */ 0x000000000000007f,
	/*                 jalr */ 0x000000000000707f,
	/*                  beq */ 0x000000000000707f,
	/*                  bne */ 0x000000000000707f,
	/*                  blt */ 0x000000000000707f,
	/*                  bge */ 0x000000000000707f,
	/*                 bltu */ 0x000000000000707f,
	/*                 bgeu */ 0x000000000000707f,
	/*                   lb */ 0x000000000000707f,
	/*                   lh */ 0x000000000000707f,
	/*                   lw */ 0x000000000000707f,
	/*                  lbu */ 0x000000000000707f,
	/*                  lhu */ 0x000000000000707f,
	/*                   sb */ 0x000000000000707f,
	/*                   sh */ 0x000000000000707f,
	/*                   sw */ 0x000000000000707f,
	/*                 addi */ 0x000000000000707f,
	/*                 slti */ 0x000000000000707f,
	/*                sltiu */ 0x000000000000707f,
	/*                 xori */ 0x000000000000707f,
	/*                  ori */ 0x000000000000707f,
	/*                 andi */ 0x000000000000707f,
	/*                 slli */ 0x00000000f800707f,
	/*                 srli */ 0x00000000f800707f,
	/*                 srai */ 0x00000000f800707f,
	/*                  add */ 0x00000000fe00707f,
	/*                  sub */ 0x00000000fe00707f,
	/*                  sll */ 0x00000000fe00707f,
	/*                  slt */ 0x00000000fe00707f,
	/*                 sltu */ 0x00000000fe00707f,
	/*                  xor */ 0x00000000fe00707f,
	/*                  srl */ 0x00000000fe00707f,
	/*                  sra */ 0x00000000fe00707f,
	/*                   or */ 0x00000000fe00707f,
	/*                  and */ 0x00000000fe00707f,
	/*                fence */ 0x000000000000707f,
	/*              fence.i */ 0x000000000000707f,
	/*                  lwu */ 0x000000000000707f,
	/*                   ld */ 0x000000000000707f,
	/*                   sd */ 0x000000000000707f,
	/*                addiw */ 0x000000000000707f,
	/*                slliw */ 0x00000000fe00707f,
	/*                srliw */ 0x00000000fe00707f,
	/*                sraiw */ 0x00000000fe00707f,
	/*                 addw */ 0x00000000fe00707f,
	/*                 subw */ 0x00000000fe00707f,
	/*                 sllw */ 0x00000000fe00707f,
	/*                 srlw */ 0x00000000fe00707f,
	/*                 sraw */ 0x00000000fe00707f,
	/*                  ldu */ 0x000000000000707f,
	/*                   lq */ 0x000000000000707f,
	/*                   sq */ 0x000000000000707f,
	/*                addid */ 0x000000000000707f,
	/*                sllid */ 0x00000000fc00707f,
	/*                srlid */ 0x00000000fc00707f,
	/*                sraid */ 0x00000000fc00707f,
	/*                 addd */ 0x00000000fe00707f,
	/*                 subd */ 0x00000000fe00707f,
	/*                 slld */ 0x00000000fe00707f,
	/*                 srld */ 0x00000000fe00707f,
	/*                 srad */ 0x00000000fe00707f,
	/*                  mul */ 0x00000000fe00707f,
	/*                 mulh */ 0x00000000fe00707f,
	/*               mulhsu */ 0x00000000fe00707f,
	/*                mulhu */ 0x00000000fe00707f,
	/*                  div */ 0x00000000fe00707f,
	/*                 divu */ 0x00000000fe00707f,
	/*                  rem */ 0x00000000fe00707f,
	/*                 remu */ 0x00000000fe00707f,
	/*                 mulw */ 0x00000000fe00707f,
	/*                 divw */ 0x00000000fe00707f,
	/*                divuw */ 0x00000000fe00707f,
	/*                 remw */ 0x00000000fe00707f,
	/*                remuw */ 0x00000000fe00707f,
	/*                 muld */ 0x00000000fe00707f,
	/*                 divd */ 0x00000000fe00707f,
	/*                divud */ 0x00000000fe00707f,
	/*                 remd */ 0x00000000fe00707f,
	/*                remud */ 0x00000000fe00707f,
	/*                 lr.w */ 0x00000000f9f0707f,
	/*                 sc.w */ 0x00000000f800707f,
	/*            amoswap.w */ 0x00000000f800707f,
	/*             amoadd.w */ 0x00000000f800707f,
	/*             amoxor.w */ 0x00000000f800707f,
	/*              amoor.w */ 0x00000000f800707f,
	/*             amoand.w */ 0x00000000f800707f,
	/*             amomin.w */ 0x00000000f800707f,
	/*             amomax.w */ 0x00000000f800707f,
	/*            amominu.w */ 0x00000000f800707f,
	/*            amomaxu.w */ 0x00000000f800707f,
	/*                 lr.d */ 0x00000000f9f0707f,
	/*                 sc.d */ 0x00000000f800707f,
	/*            amoswap.d */ 0x00000000f800707f,
	/*             amoadd.d */ 0x00000000f800707f,
	/*             amoxor.d */ 0x00000000f800707f,
	/*              amoor.d */ 0x00000000f800707f,
	/*             amoand.d */ 0x00000000f800707f,
	/*             amomin.d */ 0x00000000f800707f,
	/*             amomax.d */ 0x00000000f800707f,
	/*            amominu.d */ 0x00000000f800707f,
	/*            amomaxu.d */ 0x00000000f800707f,
	/*                 lr.q */ 0x00000000f9f0707f,
	/*                 sc.q */ 0x00000000f800707f,
	/*            amoswap.q */ 0x00000000f800707f,
	/*             amoadd.q */ 0x00000000f800707f,
	/*             amoxor.q */ 0x00000000f800707f,
	/*              amoor.q */ 0x00000000f800707f,
	/*             amoand.q */ 0x00000000f800707f,
	/*             amomin.q */ 0x00000000f800707f,
	/*             amomax.q */ 0x00000000f800707f,
	/*            amominu.q */ 0x00000000f800707f,
	/*            amomaxu.q */ 0x00000000f800707f,
	/*                ecall */ 0x00000000ffffffff,
	/*               ebreak */ 0x00000000ffffffff,
	/*                 uret */ 0x00000000ffffffff,
	/*                 sret */ 0x00000000ffffffff,
	/*                 hret */ 0x00000000ffffffff,
	/*                 mret */ 0x00000000ffffffff,
	/*                 dret */ 0x00000000ffffffff,
	/*            sfence.vm */ 0x00000000fff07fff,
	/*                  wfi */ 0x00000000ffffffff,
	/*                csrrw */ 0x000000000000707f,
	/*                csrrs */ 0x000000000000707f,
	/*                csrrc */ 0x000000000000707f,
	/*               csrrwi */ 0x000000000000707f,
	/*               csrrsi */ 0x000000000000707f,
	/*               csrrci */ 0x000000000000707f,
	/*                  flw */ 0x000000000000707f,
	/*                  fsw */ 0x000000000000707f,
	/*              fmadd.s */ 0x000000000600007f,
	/*              fmsub.s */ 0x000000000600007f,
	/*             fnmsub.s */ 0x000000000600007f,
	/*             fnmadd.s */ 0x000000000600007f,
	/*               fadd.s */ 0x00000000fe00007f,
	/*               fsub.s */ 0x00000000fe00007f,
	/*               fmul.s */ 0x00000000fe00007f,
	/*               fdiv.s */ 0x00000000fe00007f,
	/*              fsgnj.s */ 0x00000000fe00707f,
	/*             fsgnjn.s */ 0x00000000fe00707f,
	/*             fsgnjx.s */ 0x00000000fe00707f,
	/*               fmin.s */ 0x00000000fe00707f,
	/*               fmax.s */ 0x00000000fe00707f,
	/*              fsqrt.s */ 0x00000000fff0007f,
	/*                fle.s */ 0x00000000fe00707f,
	/*                flt.s */ 0x00000000fe00707f,
	/*                feq.s */ 0x00000000fe00707f,
	/*             fcvt.w.s */ 0x00000000fff0007f,
	/*            fcvt.wu.s */ 0x00000000fff0007f,
	/*             fcvt.s.w */ 0x00000000fff0007f,
	/*            fcvt.s.wu */ 0x00000000fff0007f,
	/*              fmv.x.s */ 0x00000000fff0707f,
	/*             fclass.s */ 0x00000000fff0707f,
	/*              fmv.s.x */ 0x00000000fff0707f,
	/*             fcvt.l.s */ 0x00000000fff0007f,
	/*            fcvt.lu.s */ 0x00000000fff0007f,
	/*             fcvt.s.l */ 0x00000000fff0007f,
	/*            fcvt.s.lu */ 0x00000000fff0007f,
	/*                  fld */ 0x000000000000707f,
	/*                  fsd */ 0x000000000000707f,
	/*              fmadd.d */ 0x000000000600007f,
	/*              fmsub.d */ 0x000000000600007f,
	/*             fnmsub.d */ 0x000000000600007f,
	/*             fnmadd.d */ 0x000000000600007f,
	/*               fadd.d */ 0x00000000fe00007f,
	/*               fsub.d */ 0x00000000fe00007f,
	/*               fmul.d */ 0x00000000fe00007f,
	/*               fdiv.d */ 0x00000000fe00007f,
	/*              fsgnj.d */ 0x00000000fe00707f,
	/*             fsgnjn.d */ 0x00000000fe00707f,
	/*             fsgnjx.d */ 0x00000000fe00707f,
	/*               fmin.d */ 0x00000000fe00707f,
	/*               fmax.d */ 0x00000000fe00707f,
	/*             fcvt.s.d */ 0x00000000fff0007f,
	/*             fcvt.d.s */ 0x00000000fff0007f,
	/*              fsqrt.d */ 0x00000000fff0007f,
	/*                fle.d */ 0x00000000fe00707f,
	/*                flt.d */ 0x00000000fe00707f,
	/*                feq.d */ 0x00000000fe00707f,
	/*             fcvt.w.d */ 0x00000000fff0007f,
	/*            fcvt.wu.d */ 0x00000000fff0007f,
	/*             fcvt.d.w */ 0x00000000fff0007f,
	/*            fcvt.d.wu */ 0x00000000fff0007f,
	/*             fclass.d */ 0x00000000fff0707f,
	/*             fcvt.l.d */ 0x00000000fff0007f,
	/*            fcvt.lu.d */ 0x00000000fff0007f,
	/*              fmv.x.d */ 0x00000000fff0707f,
	/*             fcvt.d.l */ 0x00000000fff0007f,
	/*            fcvt.d.lu */ 0x00000000fff0007f,
	/*              fmv.d.x */ 0x00000000fff0707f,
	/*                  flq */ 0x000000000000707f,
	/*                  fsq */ 0x000000000000707f,
	/*              fmadd.q */ 0x000000000600007f,
	/*              fmsub.q */ 0x000000000600007f,
	/*             fnmsub.q */ 0x000000000600007f,
	/*             fnmadd.q */ 0x000000000600007f,
	/*               fadd.q */ 0x00000000fe00007f,
	/*               fsub.q */ 0x00000000fe00007f,
	/*               fmul.q */ 0x00000000fe00007f,
	/*               fdiv.q */ 0x00000000fe00007f,
	/*              fsgnj.q */ 0x00000000fe00707f,
	/*             fsgnjn.q */ 0x00000000fe00707f,
	/*             fsgnjx.q */ 0x00000000fe00707f,
	/*               fmin.q */ 0x00000000fe00707f,
	/*               fmax.q */ 0x00000000fe00707f,
	/*             fcvt.s.q */ 0x00000000fff0007f,
	/*             fcvt.q.s */ 0x00000000fff0007f,
	/*             fcvt.d.q */ 0x00000000fff0007f,
	/*             fcvt.q.d */ 0x00000000fff0007f,
	/*              fsqrt.q */ 0x00000000fff0007f,
	/*                fle.q */ 0x00000000fe00707f,
	/*                flt.q */ 0x00000000fe00707f,
	/*                feq.q */ 0x00000000fe00707f,
	/*             fcvt.w.q */ 0x00000000fff0007f,
	/*            fcvt.wu.q */ 0x00000000fff0007f,
	/*             fcvt.q.w */ 0x00000000fff0007f,
	/*            fcvt.q.wu */ 0x00000000fff0007f,
	/*             fclass.q */ 0x00000000fff0707f,
	/*             fcvt.l.q */ 0x00000000fff0007f,
	/*            fcvt.lu.q */ 0x00000000fff0007f,
	/*             fcvt.q.l */ 0x00000000fff0007f,
	/*            fcvt.q.lu */ 0x00000000fff0007f,
	/*              fmv.x.q */ 0x00000000fff0707f,
	/*              fmv.q.x */ 0x00000000fff0707f,
	/*           c.addi4spn */ 0x000000000000e003,
	/*                c.fld */ 0x000000000000e003,
	/*                 c.lw */ 0x000000000000e003,
	/*                c.flw */ 0x000000000000e003,
	/*                c.fsd */ 0x000000000000e003,
	/*                 c.sw */ 0x000000000000e003,
	/*                c.fsw */ 0x000000000000e003,
	/*                c.nop */ 0x000000000000ffff,
	/*               c.addi */ 0x000000000000e003,
	/*                c.jal */ 0x000000000000e003,
	/*                 c.li */ 0x000000000000e003,
	/*           c.addi16sp */ 0x000000000000ef83,
	/*                c.lui */ 0x000000000000e003,
	/*               c.srli */ 0x000000000000ec03,
	/*               c.srai */ 0x000000000000ec03,
	/*               c.andi */ 0x000000000000ec03,
	/*                c.sub */ 0x000000000000fc63,
	/*                c.xor */ 0x000000000000fc63,
	/*                 c.or */ 0x000000000000fc63,
	/*                c.and */ 0x000000000000fc63,
	/*               c.subw */ 0x000000000000fc63,
	/*               c.addw */ 0x000000000000fc63,
	/*                  c.j */ 0x000000000000e003,
	/*               c.beqz */ 0x000000000000e003,
	/*               c.bnez */ 0x000000000000e003,
	/*               c.slli */ 0x000000000000e003,
	/*              c.fldsp */ 0x000000000000e003,
	/*               c.lwsp */ 0x000000000000e003,
	/*              c.flwsp */ 0x000000000000e003,
	/*                 c.jr */ 0x000000000000f07f,
	/*                 c.mv */ 0x000000000000f003,
	/*             c.ebreak */ 0x000000000000ffff,
	/*               c.jalr */ 0x000000000000f07f,
	/*                c.add */ 0x000000000000f003,
	/*              c.fsdsp */ 0x000000000000e003,
	/*               c.swsp */ 0x000000000000e003,
	/*              c.fswsp */ 0x000000000000e003,
	/*                 c.ld */ 0x000000000000e003,
	/*                 c.sd */ 0x000000000000e003,
	/*              c.addiw */ 0x000000000000e003,
	/*               c.ldsp */ 0x000000000000e003,
	/*               c.sdsp */ 0x000000000000e003,
	/*                 c.lq */ 0x000000000000e003,
	/*                 c.sq */ 0x000000000000e003,
	/*               c.lqsp */ 0x000000000000e003,
	/*               c.sqsp */ 0x000000000000e003,
	/*                  nop */ 0x0000000000000000,
	/*                   mv */ 0x0000000000000000,
	/*                  not */ 0x0000000000000000,
	/*                  neg */ 0x0000000000000000,
	/*                 negw */ 0x0000000000000000,
	/*               sext.w */ 0x0000000000000000,
	/*                 seqz */ 0x0000000000000000,
	/*                 snez */ 0x0000000000000000,
	/*                 sltz */ 0x0000000000000000,
	/*                 sgtz */ 0x0000000000000000,
	/*                fmv.s */ 0x0000000000000000,
	/*               fabs.s */ 0x0000000000000000,
	/*               fneg.s */ 0x0000000000000000,
	/*                fmv.d */ 0x0000000000000000,
	/*               fabs.d */ 0x0000000000000000,
	/*               fneg.d */ 0x0000000000000000,
	/*                fmv.q */ 0x0000000000000000,
	/*               fabs.q */ 0x0000000000000000,
	/*               fneg.q */ 0x0000000000000000,
	/*                 beqz */ 0x0000000000000000,
	/*                 bnez */ 0x0000000000000000,
	/*                 blez */ 0x0000000000000000,
	/*                 bgez */ 0x0000000000000000,
	/*                 bltz */ 0x0000000000000000,
	/*                 bgtz */ 0x0000000000000000,
	/*                  ble */ 0x0000000000000000,
	/*                 bleu */ 0x0000000000000000,
	/*                  bgt */ 0x0000000000000000,
	/*                 bgtu */ 0x0000000000000000,
	/*                    j */ 0x0000000000000000,
	/*                  ret */ 0x0000000000000000,
	/*                   jr */ 0x0000000000000000,
	/*              rdcycle */ 0x0000000000000000,
	/*               rdtime */ 0x0000000000000000,
	/*            rdinstret */ 0x0000000000000000,
	/*             rdcycleh */ 0x0000000000000000,
	/*              rdtimeh */ 0x0000000000000000,
	/*           rdinstreth */ 0x0000000000000000,
	/*                frcsr */ 0x0000000000000000,
	/*                 frrm */ 0x0000000000000000,
	/*              frflags */ 0x0000000000000000,
	/*                fscsr */ 0x0000000000000000,
	/*                 fsrm */ 0x0000000000000000,
	/*              fsflags */ 0x0000000000000000,
	/*                fsrmi */ 0x0000000000000000,
	/*             fsflagsi */ 0x0000000000000000,
};

const rvc_constraint rvcc_jal[] = {
	rvc_rd_eq_ra,
	rvc_end
};

const rvc_constraint rvcc_jalr[] = {
	rvc_rd_eq_ra,
	rvc_imm_eq_zero,
	rvc_end
};

const rvc_constraint rvcc_nop[] = {
	rvc_rd_eq_x0,
	rvc_rs1_eq_x0,
	rvc_imm_eq_zero,
	rvc_end
};

const rvc_constraint rvcc_mv[] = {
	rvc_imm_eq_zero,
	rvc_end
};

const rvc_constraint rvcc_not[] = {
	rvc_imm_eq_n1,
	rvc_end
};

const rvc_constraint rvcc_neg[] = {
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_negw[] = {
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_sext_w[] = {
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_seqz[] = {
	rvc_imm_eq_p1,
	rvc_end
};

const rvc_constraint rvcc_snez[] = {
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_sltz[] = {
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_sgtz[] = {
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_fmv_s[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fabs_s[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fneg_s[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fmv_d[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fabs_d[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fneg_d[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fmv_q[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fabs_q[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_fneg_q[] = {
	rvc_rs2_eq_rs1,
	rvc_end
};

const rvc_constraint rvcc_beqz[] = {
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_bnez[] = {
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_blez[] = {
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_bgez[] = {
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_bltz[] = {
	rvc_rs2_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_bgtz[] = {
	rvc_rs1_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_ble[] = {
	rvc_end
};

const rvc_constraint rvcc_bleu[] = {
	rvc_end
};

const rvc_constraint rvcc_bgt[] = {
	rvc_end
};

const rvc_constraint rvcc_bgtu[] = {
	rvc_end
};

const rvc_constraint rvcc_j[] = {
	rvc_rd_eq_x0,
	rvc_end
};

const rvc_constraint rvcc_ret[] = {
	rvc_rd_eq_x0,
	rvc_rs1_eq_ra,
	rvc_end
};

const rvc_constraint rvcc_jr[] = {
	rvc_rd_eq_x0,
	rvc_imm_eq_zero,
	rvc_end
};

const rvc_constraint rvcc_rdcycle[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0xc00,
	rvc_end
};

const rvc_constraint rvcc_rdtime[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0xc01,
	rvc_end
};

const rvc_constraint rvcc_rdinstret[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0xc02,
	rvc_end
};

const rvc_constraint rvcc_rdcycleh[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0xc80,
	rvc_end
};

const rvc_constraint rvcc_rdtimeh[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0xc81,
	rvc_end
};

const rvc_constraint rvcc_rdinstreth[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0xc80,
	rvc_end
};

const rvc_constraint rvcc_frcsr[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0x003,
	rvc_end
};

const rvc_constraint rvcc_frrm[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0x002,
	rvc_end
};

const rvc_constraint rvcc_frflags[] = {
	rvc_rs1_eq_x0,
	rvc_csr_eq_0x001,
	rvc_end
};

const rvc_constraint rvcc_fscsr[] = {
	rvc_csr_eq_0x003,
	rvc_end
};

const rvc_constraint rvcc_fsrm[] = {
	rvc_csr_eq_0x002,
	rvc_end
};

const rvc_constraint rvcc_fsflags[] = {
	rvc_csr_eq_0x001,
	rvc_end
};

const rvc_constraint rvcc_fsrmi[] = {
	rvc_csr_eq_0x002,
	rvc_end
};

const rvc_constraint rvcc_fsflagsi[] = {
	rvc_csr_eq_0x001,
	rvc_end
};


const rv_comp_data rvcp_jal[] = {
	{ rv_op_j, rvcc_j },
	{ rv_op_jal, rvcc_jal },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_jalr[] = {
	{ rv_op_ret, rvcc_ret },
	{ rv_op_jr, rvcc_jr },
	{ rv_op_jalr, rvcc_jalr },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_beq[] = {
	{ rv_op_beqz, rvcc_beqz },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_bne[] = {
	{ rv_op_bnez, rvcc_bnez },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_blt[] = {
	{ rv_op_bltz, rvcc_bltz },
	{ rv_op_bgtz, rvcc_bgtz },
	{ rv_op_bgt, rvcc_bgt },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_bge[] = {
	{ rv_op_blez, rvcc_blez },
	{ rv_op_bgez, rvcc_bgez },
	{ rv_op_ble, rvcc_ble },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_bltu[] = {
	{ rv_op_bgtu, rvcc_bgtu },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_bgeu[] = {
	{ rv_op_bleu, rvcc_bleu },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_addi[] = {
	{ rv_op_nop, rvcc_nop },
	{ rv_op_mv, rvcc_mv },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_sltiu[] = {
	{ rv_op_seqz, rvcc_seqz },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_xori[] = {
	{ rv_op_not, rvcc_not },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_sub[] = {
	{ rv_op_neg, rvcc_neg },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_slt[] = {
	{ rv_op_sltz, rvcc_sltz },
	{ rv_op_sgtz, rvcc_sgtz },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_sltu[] = {
	{ rv_op_snez, rvcc_snez },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_addiw[] = {
	{ rv_op_sext_w, rvcc_sext_w },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_subw[] = {
	{ rv_op_negw, rvcc_negw },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_csrrw[] = {
	{ rv_op_fscsr, rvcc_fscsr },
	{ rv_op_fsrm, rvcc_fsrm },
	{ rv_op_fsflags, rvcc_fsflags },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_csrrs[] = {
	{ rv_op_rdcycle, rvcc_rdcycle },
	{ rv_op_rdtime, rvcc_rdtime },
	{ rv_op_rdinstret, rvcc_rdinstret },
	{ rv_op_rdcycleh, rvcc_rdcycleh },
	{ rv_op_rdtimeh, rvcc_rdtimeh },
	{ rv_op_rdinstreth, rvcc_rdinstreth },
	{ rv_op_frcsr, rvcc_frcsr },
	{ rv_op_frrm, rvcc_frrm },
	{ rv_op_frflags, rvcc_frflags },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_csrrwi[] = {
	{ rv_op_fsrmi, rvcc_fsrmi },
	{ rv_op_fsflagsi, rvcc_fsflagsi },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnj_s[] = {
	{ rv_op_fmv_s, rvcc_fmv_s },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnjn_s[] = {
	{ rv_op_fneg_s, rvcc_fneg_s },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnjx_s[] = {
	{ rv_op_fabs_s, rvcc_fabs_s },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnj_d[] = {
	{ rv_op_fmv_d, rvcc_fmv_d },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnjn_d[] = {
	{ rv_op_fneg_d, rvcc_fneg_d },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnjx_d[] = {
	{ rv_op_fabs_d, rvcc_fabs_d },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnj_q[] = {
	{ rv_op_fmv_q, rvcc_fmv_q },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnjn_q[] = {
	{ rv_op_fneg_q, rvcc_fneg_q },
	{ rv_op_illegal, nullptr }
};

const rv_comp_data rvcp_fsgnjx_q[] = {
	{ rv_op_fabs_q, rvcc_fabs_q },
	{ rv_op_illegal, nullptr }
};


const rv_comp_data* rv_inst_pseudo[] = {
	/*              unknown */ nullptr,
	/*                  lui */ nullptr,
	/*                auipc */ nullptr,
	/*                  jal */ rvcp_jal,
	/*                 jalr */ rvcp_jalr,
	/*                  beq */ rvcp_beq,
	/*                  bne */ rvcp_bne,
	/*                  blt */ rvcp_blt,
	/*                  bge */ rvcp_bge,
	/*                 bltu */ rvcp_bltu,
	/*                 bgeu */ rvcp_bgeu,
	/*                   lb */ nullptr,
	/*                   lh */ nullptr,
	/*                   lw */ nullptr,
	/*                  lbu */ nullptr,
	/*                  lhu */ nullptr,
	/*                   sb */ nullptr,
	/*                   sh */ nullptr,
	/*                   sw */ nullptr,
	/*                 addi */ rvcp_addi,
	/*                 slti */ nullptr,
	/*                sltiu */ rvcp_sltiu,
	/*                 xori */ rvcp_xori,
	/*                  ori */ nullptr,
	/*                 andi */ nullptr,
	/*                 slli */ nullptr,
	/*                 srli */ nullptr,
	/*                 srai */ nullptr,
	/*                  add */ nullptr,
	/*                  sub */ rvcp_sub,
	/*                  sll */ nullptr,
	/*                  slt */ rvcp_slt,
	/*                 sltu */ rvcp_sltu,
	/*                  xor */ nullptr,
	/*                  srl */ nullptr,
	/*                  sra */ nullptr,
	/*                   or */ nullptr,
	/*                  and */ nullptr,
	/*                fence */ nullptr,
	/*              fence.i */ nullptr,
	/*                  lwu */ nullptr,
	/*                   ld */ nullptr,
	/*                   sd */ nullptr,
	/*                addiw */ rvcp_addiw,
	/*                slliw */ nullptr,
	/*                srliw */ nullptr,
	/*                sraiw */ nullptr,
	/*                 addw */ nullptr,
	/*                 subw */ rvcp_subw,
	/*                 sllw */ nullptr,
	/*                 srlw */ nullptr,
	/*                 sraw */ nullptr,
	/*                  ldu */ nullptr,
	/*                   lq */ nullptr,
	/*                   sq */ nullptr,
	/*                addid */ nullptr,
	/*                sllid */ nullptr,
	/*                srlid */ nullptr,
	/*                sraid */ nullptr,
	/*                 addd */ nullptr,
	/*                 subd */ nullptr,
	/*                 slld */ nullptr,
	/*                 srld */ nullptr,
	/*                 srad */ nullptr,
	/*                  mul */ nullptr,
	/*                 mulh */ nullptr,
	/*               mulhsu */ nullptr,
	/*                mulhu */ nullptr,
	/*                  div */ nullptr,
	/*                 divu */ nullptr,
	/*                  rem */ nullptr,
	/*                 remu */ nullptr,
	/*                 mulw */ nullptr,
	/*                 divw */ nullptr,
	/*                divuw */ nullptr,
	/*                 remw */ nullptr,
	/*                remuw */ nullptr,
	/*                 muld */ nullptr,
	/*                 divd */ nullptr,
	/*                divud */ nullptr,
	/*                 remd */ nullptr,
	/*                remud */ nullptr,
	/*                 lr.w */ nullptr,
	/*                 sc.w */ nullptr,
	/*            amoswap.w */ nullptr,
	/*             amoadd.w */ nullptr,
	/*             amoxor.w */ nullptr,
	/*              amoor.w */ nullptr,
	/*             amoand.w */ nullptr,
	/*             amomin.w */ nullptr,
	/*             amomax.w */ nullptr,
	/*            amominu.w */ nullptr,
	/*            amomaxu.w */ nullptr,
	/*                 lr.d */ nullptr,
	/*                 sc.d */ nullptr,
	/*            amoswap.d */ nullptr,
	/*             amoadd.d */ nullptr,
	/*             amoxor.d */ nullptr,
	/*              amoor.d */ nullptr,
	/*             amoand.d */ nullptr,
	/*             amomin.d */ nullptr,
	/*             amomax.d */ nullptr,
	/*            amominu.d */ nullptr,
	/*            amomaxu.d */ nullptr,
	/*                 lr.q */ nullptr,
	/*                 sc.q */ nullptr,
	/*            amoswap.q */ nullptr,
	/*             amoadd.q */ nullptr,
	/*             amoxor.q */ nullptr,
	/*              amoor.q */ nullptr,
	/*             amoand.q */ nullptr,
	/*             amomin.q */ nullptr,
	/*             amomax.q */ nullptr,
	/*            amominu.q */ nullptr,
	/*            amomaxu.q */ nullptr,
	/*                ecall */ nullptr,
	/*               ebreak */ nullptr,
	/*                 uret */ nullptr,
	/*                 sret */ nullptr,
	/*                 hret */ nullptr,
	/*                 mret */ nullptr,
	/*                 dret */ nullptr,
	/*            sfence.vm */ nullptr,
	/*                  wfi */ nullptr,
	/*                csrrw */ rvcp_csrrw,
	/*                csrrs */ rvcp_csrrs,
	/*                csrrc */ nullptr,
	/*               csrrwi */ rvcp_csrrwi,
	/*               csrrsi */ nullptr,
	/*               csrrci */ nullptr,
	/*                  flw */ nullptr,
	/*                  fsw */ nullptr,
	/*              fmadd.s */ nullptr,
	/*              fmsub.s */ nullptr,
	/*             fnmsub.s */ nullptr,
	/*             fnmadd.s */ nullptr,
	/*               fadd.s */ nullptr,
	/*               fsub.s */ nullptr,
	/*               fmul.s */ nullptr,
	/*               fdiv.s */ nullptr,
	/*              fsgnj.s */ rvcp_fsgnj_s,
	/*             fsgnjn.s */ rvcp_fsgnjn_s,
	/*             fsgnjx.s */ rvcp_fsgnjx_s,
	/*               fmin.s */ nullptr,
	/*               fmax.s */ nullptr,
	/*              fsqrt.s */ nullptr,
	/*                fle.s */ nullptr,
	/*                flt.s */ nullptr,
	/*                feq.s */ nullptr,
	/*             fcvt.w.s */ nullptr,
	/*            fcvt.wu.s */ nullptr,
	/*             fcvt.s.w */ nullptr,
	/*            fcvt.s.wu */ nullptr,
	/*              fmv.x.s */ nullptr,
	/*             fclass.s */ nullptr,
	/*              fmv.s.x */ nullptr,
	/*             fcvt.l.s */ nullptr,
	/*            fcvt.lu.s */ nullptr,
	/*             fcvt.s.l */ nullptr,
	/*            fcvt.s.lu */ nullptr,
	/*                  fld */ nullptr,
	/*                  fsd */ nullptr,
	/*              fmadd.d */ nullptr,
	/*              fmsub.d */ nullptr,
	/*             fnmsub.d */ nullptr,
	/*             fnmadd.d */ nullptr,
	/*               fadd.d */ nullptr,
	/*               fsub.d */ nullptr,
	/*               fmul.d */ nullptr,
	/*               fdiv.d */ nullptr,
	/*              fsgnj.d */ rvcp_fsgnj_d,
	/*             fsgnjn.d */ rvcp_fsgnjn_d,
	/*             fsgnjx.d */ rvcp_fsgnjx_d,
	/*               fmin.d */ nullptr,
	/*               fmax.d */ nullptr,
	/*             fcvt.s.d */ nullptr,
	/*             fcvt.d.s */ nullptr,
	/*              fsqrt.d */ nullptr,
	/*                fle.d */ nullptr,
	/*                flt.d */ nullptr,
	/*                feq.d */ nullptr,
	/*             fcvt.w.d */ nullptr,
	/*            fcvt.wu.d */ nullptr,
	/*             fcvt.d.w */ nullptr,
	/*            fcvt.d.wu */ nullptr,
	/*             fclass.d */ nullptr,
	/*             fcvt.l.d */ nullptr,
	/*            fcvt.lu.d */ nullptr,
	/*              fmv.x.d */ nullptr,
	/*             fcvt.d.l */ nullptr,
	/*            fcvt.d.lu */ nullptr,
	/*              fmv.d.x */ nullptr,
	/*                  flq */ nullptr,
	/*                  fsq */ nullptr,
	/*              fmadd.q */ nullptr,
	/*              fmsub.q */ nullptr,
	/*             fnmsub.q */ nullptr,
	/*             fnmadd.q */ nullptr,
	/*               fadd.q */ nullptr,
	/*               fsub.q */ nullptr,
	/*               fmul.q */ nullptr,
	/*               fdiv.q */ nullptr,
	/*              fsgnj.q */ rvcp_fsgnj_q,
	/*             fsgnjn.q */ rvcp_fsgnjn_q,
	/*             fsgnjx.q */ rvcp_fsgnjx_q,
	/*               fmin.q */ nullptr,
	/*               fmax.q */ nullptr,
	/*             fcvt.s.q */ nullptr,
	/*             fcvt.q.s */ nullptr,
	/*             fcvt.d.q */ nullptr,
	/*             fcvt.q.d */ nullptr,
	/*              fsqrt.q */ nullptr,
	/*                fle.q */ nullptr,
	/*                flt.q */ nullptr,
	/*                feq.q */ nullptr,
	/*             fcvt.w.q */ nullptr,
	/*            fcvt.wu.q */ nullptr,
	/*             fcvt.q.w */ nullptr,
	/*            fcvt.q.wu */ nullptr,
	/*             fclass.q */ nullptr,
	/*             fcvt.l.q */ nullptr,
	/*            fcvt.lu.q */ nullptr,
	/*             fcvt.q.l */ nullptr,
	/*            fcvt.q.lu */ nullptr,
	/*              fmv.x.q */ nullptr,
	/*              fmv.q.x */ nullptr,
	/*           c.addi4spn */ nullptr,
	/*                c.fld */ nullptr,
	/*                 c.lw */ nullptr,
	/*                c.flw */ nullptr,
	/*                c.fsd */ nullptr,
	/*                 c.sw */ nullptr,
	/*                c.fsw */ nullptr,
	/*                c.nop */ nullptr,
	/*               c.addi */ nullptr,
	/*                c.jal */ nullptr,
	/*                 c.li */ nullptr,
	/*           c.addi16sp */ nullptr,
	/*                c.lui */ nullptr,
	/*               c.srli */ nullptr,
	/*               c.srai */ nullptr,
	/*               c.andi */ nullptr,
	/*                c.sub */ nullptr,
	/*                c.xor */ nullptr,
	/*                 c.or */ nullptr,
	/*                c.and */ nullptr,
	/*               c.subw */ nullptr,
	/*               c.addw */ nullptr,
	/*                  c.j */ nullptr,
	/*               c.beqz */ nullptr,
	/*               c.bnez */ nullptr,
	/*               c.slli */ nullptr,
	/*              c.fldsp */ nullptr,
	/*               c.lwsp */ nullptr,
	/*              c.flwsp */ nullptr,
	/*                 c.jr */ nullptr,
	/*                 c.mv */ nullptr,
	/*             c.ebreak */ nullptr,
	/*               c.jalr */ nullptr,
	/*                c.add */ nullptr,
	/*              c.fsdsp */ nullptr,
	/*               c.swsp */ nullptr,
	/*              c.fswsp */ nullptr,
	/*                 c.ld */ nullptr,
	/*                 c.sd */ nullptr,
	/*              c.addiw */ nullptr,
	/*               c.ldsp */ nullptr,
	/*               c.sdsp */ nullptr,
	/*                 c.lq */ nullptr,
	/*                 c.sq */ nullptr,
	/*               c.lqsp */ nullptr,
	/*               c.sqsp */ nullptr,
	/*                  nop */ nullptr,
	/*                   mv */ nullptr,
	/*                  not */ nullptr,
	/*                  neg */ nullptr,
	/*                 negw */ nullptr,
	/*               sext.w */ nullptr,
	/*                 seqz */ nullptr,
	/*                 snez */ nullptr,
	/*                 sltz */ nullptr,
	/*                 sgtz */ nullptr,
	/*                fmv.s */ nullptr,
	/*               fabs.s */ nullptr,
	/*               fneg.s */ nullptr,
	/*                fmv.d */ nullptr,
	/*               fabs.d */ nullptr,
	/*               fneg.d */ nullptr,
	/*                fmv.q */ nullptr,
	/*               fabs.q */ nullptr,
	/*               fneg.q */ nullptr,
	/*                 beqz */ nullptr,
	/*                 bnez */ nullptr,
	/*                 blez */ nullptr,
	/*                 bgez */ nullptr,
	/*                 bltz */ nullptr,
	/*                 bgtz */ nullptr,
	/*                  ble */ nullptr,
	/*                 bleu */ nullptr,
	/*                  bgt */ nullptr,
	/*                 bgtu */ nullptr,
	/*                    j */ nullptr,
	/*                  ret */ nullptr,
	/*                   jr */ nullptr,
	/*              rdcycle */ nullptr,
	/*               rdtime */ nullptr,
	/*            rdinstret */ nullptr,
	/*             rdcycleh */ nullptr,
	/*              rdtimeh */ nullptr,
	/*           rdinstreth */ nullptr,
	/*                frcsr */ nullptr,
	/*                 frrm */ nullptr,
	/*              frflags */ nullptr,
	/*                fscsr */ nullptr,
	/*                 fsrm */ nullptr,
	/*              fsflags */ nullptr,
	/*                fsrmi */ nullptr,
	/*             fsflagsi */ nullptr,
};

const rv_comp_data rv_inst_depseudo[] = {
	/*              unknown */ { rv_op_illegal, nullptr },
	/*                  lui */ { rv_op_illegal, nullptr },
	/*                auipc */ { rv_op_illegal, nullptr },
	/*                  jal */ { rv_op_jal, rvcc_jal },
	/*                 jalr */ { rv_op_jalr, rvcc_jalr },
	/*                  beq */ { rv_op_illegal, nullptr },
	/*                  bne */ { rv_op_illegal, nullptr },
	/*                  blt */ { rv_op_illegal, nullptr },
	/*                  bge */ { rv_op_illegal, nullptr },
	/*                 bltu */ { rv_op_illegal, nullptr },
	/*                 bgeu */ { rv_op_illegal, nullptr },
	/*                   lb */ { rv_op_illegal, nullptr },
	/*                   lh */ { rv_op_illegal, nullptr },
	/*                   lw */ { rv_op_illegal, nullptr },
	/*                  lbu */ { rv_op_illegal, nullptr },
	/*                  lhu */ { rv_op_illegal, nullptr },
	/*                   sb */ { rv_op_illegal, nullptr },
	/*                   sh */ { rv_op_illegal, nullptr },
	/*                   sw */ { rv_op_illegal, nullptr },
	/*                 addi */ { rv_op_illegal, nullptr },
	/*                 slti */ { rv_op_illegal, nullptr },
	/*                sltiu */ { rv_op_illegal, nullptr },
	/*                 xori */ { rv_op_illegal, nullptr },
	/*                  ori */ { rv_op_illegal, nullptr },
	/*                 andi */ { rv_op_illegal, nullptr },
	/*                 slli */ { rv_op_illegal, nullptr },
	/*                 srli */ { rv_op_illegal, nullptr },
	/*                 srai */ { rv_op_illegal, nullptr },
	/*                  add */ { rv_op_illegal, nullptr },
	/*                  sub */ { rv_op_illegal, nullptr },
	/*                  sll */ { rv_op_illegal, nullptr },
	/*                  slt */ { rv_op_illegal, nullptr },
	/*                 sltu */ { rv_op_illegal, nullptr },
	/*                  xor */ { rv_op_illegal, nullptr },
	/*                  srl */ { rv_op_illegal, nullptr },
	/*                  sra */ { rv_op_illegal, nullptr },
	/*                   or */ { rv_op_illegal, nullptr },
	/*                  and */ { rv_op_illegal, nullptr },
	/*                fence */ { rv_op_illegal, nullptr },
	/*              fence.i */ { rv_op_illegal, nullptr },
	/*                  lwu */ { rv_op_illegal, nullptr },
	/*                   ld */ { rv_op_illegal, nullptr },
	/*                   sd */ { rv_op_illegal, nullptr },
	/*                addiw */ { rv_op_illegal, nullptr },
	/*                slliw */ { rv_op_illegal, nullptr },
	/*                srliw */ { rv_op_illegal, nullptr },
	/*                sraiw */ { rv_op_illegal, nullptr },
	/*                 addw */ { rv_op_illegal, nullptr },
	/*                 subw */ { rv_op_illegal, nullptr },
	/*                 sllw */ { rv_op_illegal, nullptr },
	/*                 srlw */ { rv_op_illegal, nullptr },
	/*                 sraw */ { rv_op_illegal, nullptr },
	/*                  ldu */ { rv_op_illegal, nullptr },
	/*                   lq */ { rv_op_illegal, nullptr },
	/*                   sq */ { rv_op_illegal, nullptr },
	/*                addid */ { rv_op_illegal, nullptr },
	/*                sllid */ { rv_op_illegal, nullptr },
	/*                srlid */ { rv_op_illegal, nullptr },
	/*                sraid */ { rv_op_illegal, nullptr },
	/*                 addd */ { rv_op_illegal, nullptr },
	/*                 subd */ { rv_op_illegal, nullptr },
	/*                 slld */ { rv_op_illegal, nullptr },
	/*                 srld */ { rv_op_illegal, nullptr },
	/*                 srad */ { rv_op_illegal, nullptr },
	/*                  mul */ { rv_op_illegal, nullptr },
	/*                 mulh */ { rv_op_illegal, nullptr },
	/*               mulhsu */ { rv_op_illegal, nullptr },
	/*                mulhu */ { rv_op_illegal, nullptr },
	/*                  div */ { rv_op_illegal, nullptr },
	/*                 divu */ { rv_op_illegal, nullptr },
	/*                  rem */ { rv_op_illegal, nullptr },
	/*                 remu */ { rv_op_illegal, nullptr },
	/*                 mulw */ { rv_op_illegal, nullptr },
	/*                 divw */ { rv_op_illegal, nullptr },
	/*                divuw */ { rv_op_illegal, nullptr },
	/*                 remw */ { rv_op_illegal, nullptr },
	/*                remuw */ { rv_op_illegal, nullptr },
	/*                 muld */ { rv_op_illegal, nullptr },
	/*                 divd */ { rv_op_illegal, nullptr },
	/*                divud */ { rv_op_illegal, nullptr },
	/*                 remd */ { rv_op_illegal, nullptr },
	/*                remud */ { rv_op_illegal, nullptr },
	/*                 lr.w */ { rv_op_illegal, nullptr },
	/*                 sc.w */ { rv_op_illegal, nullptr },
	/*            amoswap.w */ { rv_op_illegal, nullptr },
	/*             amoadd.w */ { rv_op_illegal, nullptr },
	/*             amoxor.w */ { rv_op_illegal, nullptr },
	/*              amoor.w */ { rv_op_illegal, nullptr },
	/*             amoand.w */ { rv_op_illegal, nullptr },
	/*             amomin.w */ { rv_op_illegal, nullptr },
	/*             amomax.w */ { rv_op_illegal, nullptr },
	/*            amominu.w */ { rv_op_illegal, nullptr },
	/*            amomaxu.w */ { rv_op_illegal, nullptr },
	/*                 lr.d */ { rv_op_illegal, nullptr },
	/*                 sc.d */ { rv_op_illegal, nullptr },
	/*            amoswap.d */ { rv_op_illegal, nullptr },
	/*             amoadd.d */ { rv_op_illegal, nullptr },
	/*             amoxor.d */ { rv_op_illegal, nullptr },
	/*              amoor.d */ { rv_op_illegal, nullptr },
	/*             amoand.d */ { rv_op_illegal, nullptr },
	/*             amomin.d */ { rv_op_illegal, nullptr },
	/*             amomax.d */ { rv_op_illegal, nullptr },
	/*            amominu.d */ { rv_op_illegal, nullptr },
	/*            amomaxu.d */ { rv_op_illegal, nullptr },
	/*                 lr.q */ { rv_op_illegal, nullptr },
	/*                 sc.q */ { rv_op_illegal, nullptr },
	/*            amoswap.q */ { rv_op_illegal, nullptr },
	/*             amoadd.q */ { rv_op_illegal, nullptr },
	/*             amoxor.q */ { rv_op_illegal, nullptr },
	/*              amoor.q */ { rv_op_illegal, nullptr },
	/*             amoand.q */ { rv_op_illegal, nullptr },
	/*             amomin.q */ { rv_op_illegal, nullptr },
	/*             amomax.q */ { rv_op_illegal, nullptr },
	/*            amominu.q */ { rv_op_illegal, nullptr },
	/*            amomaxu.q */ { rv_op_illegal, nullptr },
	/*                ecall */ { rv_op_illegal, nullptr },
	/*               ebreak */ { rv_op_illegal, nullptr },
	/*                 uret */ { rv_op_illegal, nullptr },
	/*                 sret */ { rv_op_illegal, nullptr },
	/*                 hret */ { rv_op_illegal, nullptr },
	/*                 mret */ { rv_op_illegal, nullptr },
	/*                 dret */ { rv_op_illegal, nullptr },
	/*            sfence.vm */ { rv_op_illegal, nullptr },
	/*                  wfi */ { rv_op_illegal, nullptr },
	/*                csrrw */ { rv_op_illegal, nullptr },
	/*                csrrs */ { rv_op_illegal, nullptr },
	/*                csrrc */ { rv_op_illegal, nullptr },
	/*               csrrwi */ { rv_op_illegal, nullptr },
	/*               csrrsi */ { rv_op_illegal, nullptr },
	/*               csrrci */ { rv_op_illegal, nullptr },
	/*                  flw */ { rv_op_illegal, nullptr },
	/*                  fsw */ { rv_op_illegal, nullptr },
	/*              fmadd.s */ { rv_op_illegal, nullptr },
	/*              fmsub.s */ { rv_op_illegal, nullptr },
	/*             fnmsub.s */ { rv_op_illegal, nullptr },
	/*             fnmadd.s */ { rv_op_illegal, nullptr },
	/*               fadd.s */ { rv_op_illegal, nullptr },
	/*               fsub.s */ { rv_op_illegal, nullptr },
	/*               fmul.s */ { rv_op_illegal, nullptr },
	/*               fdiv.s */ { rv_op_illegal, nullptr },
	/*              fsgnj.s */ { rv_op_illegal, nullptr },
	/*             fsgnjn.s */ { rv_op_illegal, nullptr },
	/*             fsgnjx.s */ { rv_op_illegal, nullptr },
	/*               fmin.s */ { rv_op_illegal, nullptr },
	/*               fmax.s */ { rv_op_illegal, nullptr },
	/*              fsqrt.s */ { rv_op_illegal, nullptr },
	/*                fle.s */ { rv_op_illegal, nullptr },
	/*                flt.s */ { rv_op_illegal, nullptr },
	/*                feq.s */ { rv_op_illegal, nullptr },
	/*             fcvt.w.s */ { rv_op_illegal, nullptr },
	/*            fcvt.wu.s */ { rv_op_illegal, nullptr },
	/*             fcvt.s.w */ { rv_op_illegal, nullptr },
	/*            fcvt.s.wu */ { rv_op_illegal, nullptr },
	/*              fmv.x.s */ { rv_op_illegal, nullptr },
	/*             fclass.s */ { rv_op_illegal, nullptr },
	/*              fmv.s.x */ { rv_op_illegal, nullptr },
	/*             fcvt.l.s */ { rv_op_illegal, nullptr },
	/*            fcvt.lu.s */ { rv_op_illegal, nullptr },
	/*             fcvt.s.l */ { rv_op_illegal, nullptr },
	/*            fcvt.s.lu */ { rv_op_illegal, nullptr },
	/*                  fld */ { rv_op_illegal, nullptr },
	/*                  fsd */ { rv_op_illegal, nullptr },
	/*              fmadd.d */ { rv_op_illegal, nullptr },
	/*              fmsub.d */ { rv_op_illegal, nullptr },
	/*             fnmsub.d */ { rv_op_illegal, nullptr },
	/*             fnmadd.d */ { rv_op_illegal, nullptr },
	/*               fadd.d */ { rv_op_illegal, nullptr },
	/*               fsub.d */ { rv_op_illegal, nullptr },
	/*               fmul.d */ { rv_op_illegal, nullptr },
	/*               fdiv.d */ { rv_op_illegal, nullptr },
	/*              fsgnj.d */ { rv_op_illegal, nullptr },
	/*             fsgnjn.d */ { rv_op_illegal, nullptr },
	/*             fsgnjx.d */ { rv_op_illegal, nullptr },
	/*               fmin.d */ { rv_op_illegal, nullptr },
	/*               fmax.d */ { rv_op_illegal, nullptr },
	/*             fcvt.s.d */ { rv_op_illegal, nullptr },
	/*             fcvt.d.s */ { rv_op_illegal, nullptr },
	/*              fsqrt.d */ { rv_op_illegal, nullptr },
	/*                fle.d */ { rv_op_illegal, nullptr },
	/*                flt.d */ { rv_op_illegal, nullptr },
	/*                feq.d */ { rv_op_illegal, nullptr },
	/*             fcvt.w.d */ { rv_op_illegal, nullptr },
	/*            fcvt.wu.d */ { rv_op_illegal, nullptr },
	/*             fcvt.d.w */ { rv_op_illegal, nullptr },
	/*            fcvt.d.wu */ { rv_op_illegal, nullptr },
	/*             fclass.d */ { rv_op_illegal, nullptr },
	/*             fcvt.l.d */ { rv_op_illegal, nullptr },
	/*            fcvt.lu.d */ { rv_op_illegal, nullptr },
	/*              fmv.x.d */ { rv_op_illegal, nullptr },
	/*             fcvt.d.l */ { rv_op_illegal, nullptr },
	/*            fcvt.d.lu */ { rv_op_illegal, nullptr },
	/*              fmv.d.x */ { rv_op_illegal, nullptr },
	/*                  flq */ { rv_op_illegal, nullptr },
	/*                  fsq */ { rv_op_illegal, nullptr },
	/*              fmadd.q */ { rv_op_illegal, nullptr },
	/*              fmsub.q */ { rv_op_illegal, nullptr },
	/*             fnmsub.q */ { rv_op_illegal, nullptr },
	/*             fnmadd.q */ { rv_op_illegal, nullptr },
	/*               fadd.q */ { rv_op_illegal, nullptr },
	/*               fsub.q */ { rv_op_illegal, nullptr },
	/*               fmul.q */ { rv_op_illegal, nullptr },
	/*               fdiv.q */ { rv_op_illegal, nullptr },
	/*              fsgnj.q */ { rv_op_illegal, nullptr },
	/*             fsgnjn.q */ { rv_op_illegal, nullptr },
	/*             fsgnjx.q */ { rv_op_illegal, nullptr },
	/*               fmin.q */ { rv_op_illegal, nullptr },
	/*               fmax.q */ { rv_op_illegal, nullptr },
	/*             fcvt.s.q */ { rv_op_illegal, nullptr },
	/*             fcvt.q.s */ { rv_op_illegal, nullptr },
	/*             fcvt.d.q */ { rv_op_illegal, nullptr },
	/*             fcvt.q.d */ { rv_op_illegal, nullptr },
	/*              fsqrt.q */ { rv_op_illegal, nullptr },
	/*                fle.q */ { rv_op_illegal, nullptr },
	/*                flt.q */ { rv_op_illegal, nullptr },
	/*                feq.q */ { rv_op_illegal, nullptr },
	/*             fcvt.w.q */ { rv_op_illegal, nullptr },
	/*            fcvt.wu.q */ { rv_op_illegal, nullptr },
	/*             fcvt.q.w */ { rv_op_illegal, nullptr },
	/*            fcvt.q.wu */ { rv_op_illegal, nullptr },
	/*             fclass.q */ { rv_op_illegal, nullptr },
	/*             fcvt.l.q */ { rv_op_illegal, nullptr },
	/*            fcvt.lu.q */ { rv_op_illegal, nullptr },
	/*             fcvt.q.l */ { rv_op_illegal, nullptr },
	/*            fcvt.q.lu */ { rv_op_illegal, nullptr },
	/*              fmv.x.q */ { rv_op_illegal, nullptr },
	/*              fmv.q.x */ { rv_op_illegal, nullptr },
	/*           c.addi4spn */ { rv_op_illegal, nullptr },
	/*                c.fld */ { rv_op_illegal, nullptr },
	/*                 c.lw */ { rv_op_illegal, nullptr },
	/*                c.flw */ { rv_op_illegal, nullptr },
	/*                c.fsd */ { rv_op_illegal, nullptr },
	/*                 c.sw */ { rv_op_illegal, nullptr },
	/*                c.fsw */ { rv_op_illegal, nullptr },
	/*                c.nop */ { rv_op_illegal, nullptr },
	/*               c.addi */ { rv_op_illegal, nullptr },
	/*                c.jal */ { rv_op_illegal, nullptr },
	/*                 c.li */ { rv_op_illegal, nullptr },
	/*           c.addi16sp */ { rv_op_illegal, nullptr },
	/*                c.lui */ { rv_op_illegal, nullptr },
	/*               c.srli */ { rv_op_illegal, nullptr },
	/*               c.srai */ { rv_op_illegal, nullptr },
	/*               c.andi */ { rv_op_illegal, nullptr },
	/*                c.sub */ { rv_op_illegal, nullptr },
	/*                c.xor */ { rv_op_illegal, nullptr },
	/*                 c.or */ { rv_op_illegal, nullptr },
	/*                c.and */ { rv_op_illegal, nullptr },
	/*               c.subw */ { rv_op_illegal, nullptr },
	/*               c.addw */ { rv_op_illegal, nullptr },
	/*                  c.j */ { rv_op_illegal, nullptr },
	/*               c.beqz */ { rv_op_illegal, nullptr },
	/*               c.bnez */ { rv_op_illegal, nullptr },
	/*               c.slli */ { rv_op_illegal, nullptr },
	/*              c.fldsp */ { rv_op_illegal, nullptr },
	/*               c.lwsp */ { rv_op_illegal, nullptr },
	/*              c.flwsp */ { rv_op_illegal, nullptr },
	/*                 c.jr */ { rv_op_illegal, nullptr },
	/*                 c.mv */ { rv_op_illegal, nullptr },
	/*             c.ebreak */ { rv_op_illegal, nullptr },
	/*               c.jalr */ { rv_op_illegal, nullptr },
	/*                c.add */ { rv_op_illegal, nullptr },
	/*              c.fsdsp */ { rv_op_illegal, nullptr },
	/*               c.swsp */ { rv_op_illegal, nullptr },
	/*              c.fswsp */ { rv_op_illegal, nullptr },
	/*                 c.ld */ { rv_op_illegal, nullptr },
	/*                 c.sd */ { rv_op_illegal, nullptr },
	/*              c.addiw */ { rv_op_illegal, nullptr },
	/*               c.ldsp */ { rv_op_illegal, nullptr },
	/*               c.sdsp */ { rv_op_illegal, nullptr },
	/*                 c.lq */ { rv_op_illegal, nullptr },
	/*                 c.sq */ { rv_op_illegal, nullptr },
	/*               c.lqsp */ { rv_op_illegal, nullptr },
	/*               c.sqsp */ { rv_op_illegal, nullptr },
	/*                  nop */ { rv_op_addi, rvcc_nop },
	/*                   mv */ { rv_op_addi, rvcc_mv },
	/*                  not */ { rv_op_xori, rvcc_not },
	/*                  neg */ { rv_op_sub, rvcc_neg },
	/*                 negw */ { rv_op_subw, rvcc_negw },
	/*               sext.w */ { rv_op_addiw, rvcc_sext_w },
	/*                 seqz */ { rv_op_sltiu, rvcc_seqz },
	/*                 snez */ { rv_op_sltu, rvcc_snez },
	/*                 sltz */ { rv_op_slt, rvcc_sltz },
	/*                 sgtz */ { rv_op_slt, rvcc_sgtz },
	/*                fmv.s */ { rv_op_fsgnj_s, rvcc_fmv_s },
	/*               fabs.s */ { rv_op_fsgnjx_s, rvcc_fabs_s },
	/*               fneg.s */ { rv_op_fsgnjn_s, rvcc_fneg_s },
	/*                fmv.d */ { rv_op_fsgnj_d, rvcc_fmv_d },
	/*               fabs.d */ { rv_op_fsgnjx_d, rvcc_fabs_d },
	/*               fneg.d */ { rv_op_fsgnjn_d, rvcc_fneg_d },
	/*                fmv.q */ { rv_op_fsgnj_q, rvcc_fmv_q },
	/*               fabs.q */ { rv_op_fsgnjx_q, rvcc_fabs_q },
	/*               fneg.q */ { rv_op_fsgnjn_q, rvcc_fneg_q },
	/*                 beqz */ { rv_op_beq, rvcc_beqz },
	/*                 bnez */ { rv_op_bne, rvcc_bnez },
	/*                 blez */ { rv_op_bge, rvcc_blez },
	/*                 bgez */ { rv_op_bge, rvcc_bgez },
	/*                 bltz */ { rv_op_blt, rvcc_bltz },
	/*                 bgtz */ { rv_op_blt, rvcc_bgtz },
	/*                  ble */ { rv_op_bge, rvcc_ble },
	/*                 bleu */ { rv_op_bgeu, rvcc_bleu },
	/*                  bgt */ { rv_op_blt, rvcc_bgt },
	/*                 bgtu */ { rv_op_bltu, rvcc_bgtu },
	/*                    j */ { rv_op_jal, rvcc_j },
	/*                  ret */ { rv_op_jalr, rvcc_ret },
	/*                   jr */ { rv_op_jalr, rvcc_jr },
	/*              rdcycle */ { rv_op_csrrs, rvcc_rdcycle },
	/*               rdtime */ { rv_op_csrrs, rvcc_rdtime },
	/*            rdinstret */ { rv_op_csrrs, rvcc_rdinstret },
	/*             rdcycleh */ { rv_op_csrrs, rvcc_rdcycleh },
	/*              rdtimeh */ { rv_op_csrrs, rvcc_rdtimeh },
	/*           rdinstreth */ { rv_op_csrrs, rvcc_rdinstreth },
	/*                frcsr */ { rv_op_csrrs, rvcc_frcsr },
	/*                 frrm */ { rv_op_csrrs, rvcc_frrm },
	/*              frflags */ { rv_op_csrrs, rvcc_frflags },
	/*                fscsr */ { rv_op_csrrw, rvcc_fscsr },
	/*                 fsrm */ { rv_op_csrrw, rvcc_fsrm },
	/*              fsflags */ { rv_op_csrrw, rvcc_fsflags },
	/*                fsrmi */ { rv_op_csrrwi, rvcc_fsrmi },
	/*             fsflagsi */ { rv_op_csrrwi, rvcc_fsflagsi },
};

const rv_comp_data* rv_inst_comp_rv32[] = {
	/*              unknown */ nullptr,
	/*                  lui */ rvcd_rv32_lui,
	/*                auipc */ nullptr,
	/*                  jal */ rvcd_rv32_jal,
	/*                 jalr */ rvcd_rv32_jalr,
	/*                  beq */ rvcd_rv32_beq,
	/*                  bne */ rvcd_rv32_bne,
	/*                  blt */ nullptr,
	/*                  bge */ nullptr,
	/*                 bltu */ nullptr,
	/*                 bgeu */ nullptr,
	/*                   lb */ nullptr,
	/*                   lh */ nullptr,
	/*                   lw */ rvcd_rv32_lw,
	/*                  lbu */ nullptr,
	/*                  lhu */ nullptr,
	/*                   sb */ nullptr,
	/*                   sh */ nullptr,
	/*                   sw */ rvcd_rv32_sw,
	/*                 addi */ rvcd_rv32_addi,
	/*                 slti */ nullptr,
	/*                sltiu */ nullptr,
	/*                 xori */ nullptr,
	/*                  ori */ nullptr,
	/*                 andi */ rvcd_rv32_andi,
	/*                 slli */ rvcd_rv32_slli,
	/*                 srli */ rvcd_rv32_srli,
	/*                 srai */ rvcd_rv32_srai,
	/*                  add */ rvcd_rv32_add,
	/*                  sub */ rvcd_rv32_sub,
	/*                  sll */ nullptr,
	/*                  slt */ nullptr,
	/*                 sltu */ nullptr,
	/*                  xor */ rvcd_rv32_xor,
	/*                  srl */ nullptr,
	/*                  sra */ nullptr,
	/*                   or */ rvcd_rv32_or,
	/*                  and */ rvcd_rv32_and,
	/*                fence */ nullptr,
	/*              fence.i */ nullptr,
	/*                  lwu */ nullptr,
	/*                   ld */ nullptr,
	/*                   sd */ nullptr,
	/*                addiw */ nullptr,
	/*                slliw */ nullptr,
	/*                srliw */ nullptr,
	/*                sraiw */ nullptr,
	/*                 addw */ nullptr,
	/*                 subw */ nullptr,
	/*                 sllw */ nullptr,
	/*                 srlw */ nullptr,
	/*                 sraw */ nullptr,
	/*                  ldu */ nullptr,
	/*                   lq */ nullptr,
	/*                   sq */ nullptr,
	/*                addid */ nullptr,
	/*                sllid */ nullptr,
	/*                srlid */ nullptr,
	/*                sraid */ nullptr,
	/*                 addd */ nullptr,
	/*                 subd */ nullptr,
	/*                 slld */ nullptr,
	/*                 srld */ nullptr,
	/*                 srad */ nullptr,
	/*                  mul */ nullptr,
	/*                 mulh */ nullptr,
	/*               mulhsu */ nullptr,
	/*                mulhu */ nullptr,
	/*                  div */ nullptr,
	/*                 divu */ nullptr,
	/*                  rem */ nullptr,
	/*                 remu */ nullptr,
	/*                 mulw */ nullptr,
	/*                 divw */ nullptr,
	/*                divuw */ nullptr,
	/*                 remw */ nullptr,
	/*                remuw */ nullptr,
	/*                 muld */ nullptr,
	/*                 divd */ nullptr,
	/*                divud */ nullptr,
	/*                 remd */ nullptr,
	/*                remud */ nullptr,
	/*                 lr.w */ nullptr,
	/*                 sc.w */ nullptr,
	/*            amoswap.w */ nullptr,
	/*             amoadd.w */ nullptr,
	/*             amoxor.w */ nullptr,
	/*              amoor.w */ nullptr,
	/*             amoand.w */ nullptr,
	/*             amomin.w */ nullptr,
	/*             amomax.w */ nullptr,
	/*            amominu.w */ nullptr,
	/*            amomaxu.w */ nullptr,
	/*                 lr.d */ nullptr,
	/*                 sc.d */ nullptr,
	/*            amoswap.d */ nullptr,
	/*             amoadd.d */ nullptr,
	/*             amoxor.d */ nullptr,
	/*              amoor.d */ nullptr,
	/*             amoand.d */ nullptr,
	/*             amomin.d */ nullptr,
	/*             amomax.d */ nullptr,
	/*            amominu.d */ nullptr,
	/*            amomaxu.d */ nullptr,
	/*                 lr.q */ nullptr,
	/*                 sc.q */ nullptr,
	/*            amoswap.q */ nullptr,
	/*             amoadd.q */ nullptr,
	/*             amoxor.q */ nullptr,
	/*              amoor.q */ nullptr,
	/*             amoand.q */ nullptr,
	/*             amomin.q */ nullptr,
	/*             amomax.q */ nullptr,
	/*            amominu.q */ nullptr,
	/*            amomaxu.q */ nullptr,
	/*                ecall */ nullptr,
	/*               ebreak */ rvcd_rv32_ebreak,
	/*                 uret */ nullptr,
	/*                 sret */ nullptr,
	/*                 hret */ nullptr,
	/*                 mret */ nullptr,
	/*                 dret */ nullptr,
	/*            sfence.vm */ nullptr,
	/*                  wfi */ nullptr,
	/*                csrrw */ nullptr,
	/*                csrrs */ nullptr,
	/*                csrrc */ nullptr,
	/*               csrrwi */ nullptr,
	/*               csrrsi */ nullptr,
	/*               csrrci */ nullptr,
	/*                  flw */ rvcd_rv32_flw,
	/*                  fsw */ rvcd_rv32_fsw,
	/*              fmadd.s */ nullptr,
	/*              fmsub.s */ nullptr,
	/*             fnmsub.s */ nullptr,
	/*             fnmadd.s */ nullptr,
	/*               fadd.s */ nullptr,
	/*               fsub.s */ nullptr,
	/*               fmul.s */ nullptr,
	/*               fdiv.s */ nullptr,
	/*              fsgnj.s */ nullptr,
	/*             fsgnjn.s */ nullptr,
	/*             fsgnjx.s */ nullptr,
	/*               fmin.s */ nullptr,
	/*               fmax.s */ nullptr,
	/*              fsqrt.s */ nullptr,
	/*                fle.s */ nullptr,
	/*                flt.s */ nullptr,
	/*                feq.s */ nullptr,
	/*             fcvt.w.s */ nullptr,
	/*            fcvt.wu.s */ nullptr,
	/*             fcvt.s.w */ nullptr,
	/*            fcvt.s.wu */ nullptr,
	/*              fmv.x.s */ nullptr,
	/*             fclass.s */ nullptr,
	/*              fmv.s.x */ nullptr,
	/*             fcvt.l.s */ nullptr,
	/*            fcvt.lu.s */ nullptr,
	/*             fcvt.s.l */ nullptr,
	/*            fcvt.s.lu */ nullptr,
	/*                  fld */ rvcd_rv32_fld,
	/*                  fsd */ rvcd_rv32_fsd,
	/*              fmadd.d */ nullptr,
	/*              fmsub.d */ nullptr,
	/*             fnmsub.d */ nullptr,
	/*             fnmadd.d */ nullptr,
	/*               fadd.d */ nullptr,
	/*               fsub.d */ nullptr,
	/*               fmul.d */ nullptr,
	/*               fdiv.d */ nullptr,
	/*              fsgnj.d */ nullptr,
	/*             fsgnjn.d */ nullptr,
	/*             fsgnjx.d */ nullptr,
	/*               fmin.d */ nullptr,
	/*               fmax.d */ nullptr,
	/*             fcvt.s.d */ nullptr,
	/*             fcvt.d.s */ nullptr,
	/*              fsqrt.d */ nullptr,
	/*                fle.d */ nullptr,
	/*                flt.d */ nullptr,
	/*                feq.d */ nullptr,
	/*             fcvt.w.d */ nullptr,
	/*            fcvt.wu.d */ nullptr,
	/*             fcvt.d.w */ nullptr,
	/*            fcvt.d.wu */ nullptr,
	/*             fclass.d */ nullptr,
	/*             fcvt.l.d */ nullptr,
	/*            fcvt.lu.d */ nullptr,
	/*              fmv.x.d */ nullptr,
	/*             fcvt.d.l */ nullptr,
	/*            fcvt.d.lu */ nullptr,
	/*              fmv.d.x */ nullptr,
	/*                  flq */ nullptr,
	/*                  fsq */ nullptr,
	/*              fmadd.q */ nullptr,
	/*              fmsub.q */ nullptr,
	/*             fnmsub.q */ nullptr,
	/*             fnmadd.q */ nullptr,
	/*               fadd.q */ nullptr,
	/*               fsub.q */ nullptr,
	/*               fmul.q */ nullptr,
	/*               fdiv.q */ nullptr,
	/*              fsgnj.q */ nullptr,
	/*             fsgnjn.q */ nullptr,
	/*             fsgnjx.q */ nullptr,
	/*               fmin.q */ nullptr,
	/*               fmax.q */ nullptr,
	/*             fcvt.s.q */ nullptr,
	/*             fcvt.q.s */ nullptr,
	/*             fcvt.d.q */ nullptr,
	/*             fcvt.q.d */ nullptr,
	/*              fsqrt.q */ nullptr,
	/*                fle.q */ nullptr,
	/*                flt.q */ nullptr,
	/*                feq.q */ nullptr,
	/*             fcvt.w.q */ nullptr,
	/*            fcvt.wu.q */ nullptr,
	/*             fcvt.q.w */ nullptr,
	/*            fcvt.q.wu */ nullptr,
	/*             fclass.q */ nullptr,
	/*             fcvt.l.q */ nullptr,
	/*            fcvt.lu.q */ nullptr,
	/*             fcvt.q.l */ nullptr,
	/*            fcvt.q.lu */ nullptr,
	/*              fmv.x.q */ nullptr,
	/*              fmv.q.x */ nullptr,
	/*           c.addi4spn */ nullptr,
	/*                c.fld */ nullptr,
	/*                 c.lw */ nullptr,
	/*                c.flw */ nullptr,
	/*                c.fsd */ nullptr,
	/*                 c.sw */ nullptr,
	/*                c.fsw */ nullptr,
	/*                c.nop */ nullptr,
	/*               c.addi */ nullptr,
	/*                c.jal */ nullptr,
	/*                 c.li */ nullptr,
	/*           c.addi16sp */ nullptr,
	/*                c.lui */ nullptr,
	/*               c.srli */ nullptr,
	/*               c.srai */ nullptr,
	/*               c.andi */ nullptr,
	/*                c.sub */ nullptr,
	/*                c.xor */ nullptr,
	/*                 c.or */ nullptr,
	/*                c.and */ nullptr,
	/*               c.subw */ nullptr,
	/*               c.addw */ nullptr,
	/*                  c.j */ nullptr,
	/*               c.beqz */ nullptr,
	/*               c.bnez */ nullptr,
	/*               c.slli */ nullptr,
	/*              c.fldsp */ nullptr,
	/*               c.lwsp */ nullptr,
	/*              c.flwsp */ nullptr,
	/*                 c.jr */ nullptr,
	/*                 c.mv */ nullptr,
	/*             c.ebreak */ nullptr,
	/*               c.jalr */ nullptr,
	/*                c.add */ nullptr,
	/*              c.fsdsp */ nullptr,
	/*               c.swsp */ nullptr,
	/*              c.fswsp */ nullptr,
	/*                 c.ld */ nullptr,
	/*                 c.sd */ nullptr,
	/*              c.addiw */ nullptr,
	/*               c.ldsp */ nullptr,
	/*               c.sdsp */ nullptr,
	/*                 c.lq */ nullptr,
	/*                 c.sq */ nullptr,
	/*               c.lqsp */ nullptr,
	/*               c.sqsp */ nullptr,
	/*                  nop */ nullptr,
	/*                   mv */ nullptr,
	/*                  not */ nullptr,
	/*                  neg */ nullptr,
	/*                 negw */ nullptr,
	/*               sext.w */ nullptr,
	/*                 seqz */ nullptr,
	/*                 snez */ nullptr,
	/*                 sltz */ nullptr,
	/*                 sgtz */ nullptr,
	/*                fmv.s */ nullptr,
	/*               fabs.s */ nullptr,
	/*               fneg.s */ nullptr,
	/*                fmv.d */ nullptr,
	/*               fabs.d */ nullptr,
	/*               fneg.d */ nullptr,
	/*                fmv.q */ nullptr,
	/*               fabs.q */ nullptr,
	/*               fneg.q */ nullptr,
	/*                 beqz */ nullptr,
	/*                 bnez */ nullptr,
	/*                 blez */ nullptr,
	/*                 bgez */ nullptr,
	/*                 bltz */ nullptr,
	/*                 bgtz */ nullptr,
	/*                  ble */ nullptr,
	/*                 bleu */ nullptr,
	/*                  bgt */ nullptr,
	/*                 bgtu */ nullptr,
	/*                    j */ nullptr,
	/*                  ret */ nullptr,
	/*                   jr */ nullptr,
	/*              rdcycle */ nullptr,
	/*               rdtime */ nullptr,
	/*            rdinstret */ nullptr,
	/*             rdcycleh */ nullptr,
	/*              rdtimeh */ nullptr,
	/*           rdinstreth */ nullptr,
	/*                frcsr */ nullptr,
	/*                 frrm */ nullptr,
	/*              frflags */ nullptr,
	/*                fscsr */ nullptr,
	/*                 fsrm */ nullptr,
	/*              fsflags */ nullptr,
	/*                fsrmi */ nullptr,
	/*             fsflagsi */ nullptr,
};

const rv_comp_data* rv_inst_comp_rv64[] = {
	/*              unknown */ nullptr,
	/*                  lui */ rvcd_rv64_lui,
	/*                auipc */ nullptr,
	/*                  jal */ rvcd_rv64_jal,
	/*                 jalr */ rvcd_rv64_jalr,
	/*                  beq */ rvcd_rv64_beq,
	/*                  bne */ rvcd_rv64_bne,
	/*                  blt */ nullptr,
	/*                  bge */ nullptr,
	/*                 bltu */ nullptr,
	/*                 bgeu */ nullptr,
	/*                   lb */ nullptr,
	/*                   lh */ nullptr,
	/*                   lw */ rvcd_rv64_lw,
	/*                  lbu */ nullptr,
	/*                  lhu */ nullptr,
	/*                   sb */ nullptr,
	/*                   sh */ nullptr,
	/*                   sw */ rvcd_rv64_sw,
	/*                 addi */ rvcd_rv64_addi,
	/*                 slti */ nullptr,
	/*                sltiu */ nullptr,
	/*                 xori */ nullptr,
	/*                  ori */ nullptr,
	/*                 andi */ rvcd_rv64_andi,
	/*                 slli */ rvcd_rv64_slli,
	/*                 srli */ rvcd_rv64_srli,
	/*                 srai */ rvcd_rv64_srai,
	/*                  add */ rvcd_rv64_add,
	/*                  sub */ rvcd_rv64_sub,
	/*                  sll */ nullptr,
	/*                  slt */ nullptr,
	/*                 sltu */ nullptr,
	/*                  xor */ rvcd_rv64_xor,
	/*                  srl */ nullptr,
	/*                  sra */ nullptr,
	/*                   or */ rvcd_rv64_or,
	/*                  and */ rvcd_rv64_and,
	/*                fence */ nullptr,
	/*              fence.i */ nullptr,
	/*                  lwu */ nullptr,
	/*                   ld */ rvcd_rv64_ld,
	/*                   sd */ rvcd_rv64_sd,
	/*                addiw */ rvcd_rv64_addiw,
	/*                slliw */ nullptr,
	/*                srliw */ nullptr,
	/*                sraiw */ nullptr,
	/*                 addw */ rvcd_rv64_addw,
	/*                 subw */ rvcd_rv64_subw,
	/*                 sllw */ nullptr,
	/*                 srlw */ nullptr,
	/*                 sraw */ nullptr,
	/*                  ldu */ nullptr,
	/*                   lq */ nullptr,
	/*                   sq */ nullptr,
	/*                addid */ nullptr,
	/*                sllid */ nullptr,
	/*                srlid */ nullptr,
	/*                sraid */ nullptr,
	/*                 addd */ nullptr,
	/*                 subd */ nullptr,
	/*                 slld */ nullptr,
	/*                 srld */ nullptr,
	/*                 srad */ nullptr,
	/*                  mul */ nullptr,
	/*                 mulh */ nullptr,
	/*               mulhsu */ nullptr,
	/*                mulhu */ nullptr,
	/*                  div */ nullptr,
	/*                 divu */ nullptr,
	/*                  rem */ nullptr,
	/*                 remu */ nullptr,
	/*                 mulw */ nullptr,
	/*                 divw */ nullptr,
	/*                divuw */ nullptr,
	/*                 remw */ nullptr,
	/*                remuw */ nullptr,
	/*                 muld */ nullptr,
	/*                 divd */ nullptr,
	/*                divud */ nullptr,
	/*                 remd */ nullptr,
	/*                remud */ nullptr,
	/*                 lr.w */ nullptr,
	/*                 sc.w */ nullptr,
	/*            amoswap.w */ nullptr,
	/*             amoadd.w */ nullptr,
	/*             amoxor.w */ nullptr,
	/*              amoor.w */ nullptr,
	/*             amoand.w */ nullptr,
	/*             amomin.w */ nullptr,
	/*             amomax.w */ nullptr,
	/*            amominu.w */ nullptr,
	/*            amomaxu.w */ nullptr,
	/*                 lr.d */ nullptr,
	/*                 sc.d */ nullptr,
	/*            amoswap.d */ nullptr,
	/*             amoadd.d */ nullptr,
	/*             amoxor.d */ nullptr,
	/*              amoor.d */ nullptr,
	/*             amoand.d */ nullptr,
	/*             amomin.d */ nullptr,
	/*             amomax.d */ nullptr,
	/*            amominu.d */ nullptr,
	/*            amomaxu.d */ nullptr,
	/*                 lr.q */ nullptr,
	/*                 sc.q */ nullptr,
	/*            amoswap.q */ nullptr,
	/*             amoadd.q */ nullptr,
	/*             amoxor.q */ nullptr,
	/*              amoor.q */ nullptr,
	/*             amoand.q */ nullptr,
	/*             amomin.q */ nullptr,
	/*             amomax.q */ nullptr,
	/*            amominu.q */ nullptr,
	/*            amomaxu.q */ nullptr,
	/*                ecall */ nullptr,
	/*               ebreak */ rvcd_rv64_ebreak,
	/*                 uret */ nullptr,
	/*                 sret */ nullptr,
	/*                 hret */ nullptr,
	/*                 mret */ nullptr,
	/*                 dret */ nullptr,
	/*            sfence.vm */ nullptr,
	/*                  wfi */ nullptr,
	/*                csrrw */ nullptr,
	/*                csrrs */ nullptr,
	/*                csrrc */ nullptr,
	/*               csrrwi */ nullptr,
	/*               csrrsi */ nullptr,
	/*               csrrci */ nullptr,
	/*                  flw */ nullptr,
	/*                  fsw */ nullptr,
	/*              fmadd.s */ nullptr,
	/*              fmsub.s */ nullptr,
	/*             fnmsub.s */ nullptr,
	/*             fnmadd.s */ nullptr,
	/*               fadd.s */ nullptr,
	/*               fsub.s */ nullptr,
	/*               fmul.s */ nullptr,
	/*               fdiv.s */ nullptr,
	/*              fsgnj.s */ nullptr,
	/*             fsgnjn.s */ nullptr,
	/*             fsgnjx.s */ nullptr,
	/*               fmin.s */ nullptr,
	/*               fmax.s */ nullptr,
	/*              fsqrt.s */ nullptr,
	/*                fle.s */ nullptr,
	/*                flt.s */ nullptr,
	/*                feq.s */ nullptr,
	/*             fcvt.w.s */ nullptr,
	/*            fcvt.wu.s */ nullptr,
	/*             fcvt.s.w */ nullptr,
	/*            fcvt.s.wu */ nullptr,
	/*              fmv.x.s */ nullptr,
	/*             fclass.s */ nullptr,
	/*              fmv.s.x */ nullptr,
	/*             fcvt.l.s */ nullptr,
	/*            fcvt.lu.s */ nullptr,
	/*             fcvt.s.l */ nullptr,
	/*            fcvt.s.lu */ nullptr,
	/*                  fld */ rvcd_rv64_fld,
	/*                  fsd */ rvcd_rv64_fsd,
	/*              fmadd.d */ nullptr,
	/*              fmsub.d */ nullptr,
	/*             fnmsub.d */ nullptr,
	/*             fnmadd.d */ nullptr,
	/*               fadd.d */ nullptr,
	/*               fsub.d */ nullptr,
	/*               fmul.d */ nullptr,
	/*               fdiv.d */ nullptr,
	/*              fsgnj.d */ nullptr,
	/*             fsgnjn.d */ nullptr,
	/*             fsgnjx.d */ nullptr,
	/*               fmin.d */ nullptr,
	/*               fmax.d */ nullptr,
	/*             fcvt.s.d */ nullptr,
	/*             fcvt.d.s */ nullptr,
	/*              fsqrt.d */ nullptr,
	/*                fle.d */ nullptr,
	/*                flt.d */ nullptr,
	/*                feq.d */ nullptr,
	/*             fcvt.w.d */ nullptr,
	/*            fcvt.wu.d */ nullptr,
	/*             fcvt.d.w */ nullptr,
	/*            fcvt.d.wu */ nullptr,
	/*             fclass.d */ nullptr,
	/*             fcvt.l.d */ nullptr,
	/*            fcvt.lu.d */ nullptr,
	/*              fmv.x.d */ nullptr,
	/*             fcvt.d.l */ nullptr,
	/*            fcvt.d.lu */ nullptr,
	/*              fmv.d.x */ nullptr,
	/*                  flq */ nullptr,
	/*                  fsq */ nullptr,
	/*              fmadd.q */ nullptr,
	/*              fmsub.q */ nullptr,
	/*             fnmsub.q */ nullptr,
	/*             fnmadd.q */ nullptr,
	/*               fadd.q */ nullptr,
	/*               fsub.q */ nullptr,
	/*               fmul.q */ nullptr,
	/*               fdiv.q */ nullptr,
	/*              fsgnj.q */ nullptr,
	/*             fsgnjn.q */ nullptr,
	/*             fsgnjx.q */ nullptr,
	/*               fmin.q */ nullptr,
	/*               fmax.q */ nullptr,
	/*             fcvt.s.q */ nullptr,
	/*             fcvt.q.s */ nullptr,
	/*             fcvt.d.q */ nullptr,
	/*             fcvt.q.d */ nullptr,
	/*              fsqrt.q */ nullptr,
	/*                fle.q */ nullptr,
	/*                flt.q */ nullptr,
	/*                feq.q */ nullptr,
	/*             fcvt.w.q */ nullptr,
	/*            fcvt.wu.q */ nullptr,
	/*             fcvt.q.w */ nullptr,
	/*            fcvt.q.wu */ nullptr,
	/*             fclass.q */ nullptr,
	/*             fcvt.l.q */ nullptr,
	/*            fcvt.lu.q */ nullptr,
	/*             fcvt.q.l */ nullptr,
	/*            fcvt.q.lu */ nullptr,
	/*              fmv.x.q */ nullptr,
	/*              fmv.q.x */ nullptr,
	/*           c.addi4spn */ nullptr,
	/*                c.fld */ nullptr,
	/*                 c.lw */ nullptr,
	/*                c.flw */ nullptr,
	/*                c.fsd */ nullptr,
	/*                 c.sw */ nullptr,
	/*                c.fsw */ nullptr,
	/*                c.nop */ nullptr,
	/*               c.addi */ nullptr,
	/*                c.jal */ nullptr,
	/*                 c.li */ nullptr,
	/*           c.addi16sp */ nullptr,
	/*                c.lui */ nullptr,
	/*               c.srli */ nullptr,
	/*               c.srai */ nullptr,
	/*               c.andi */ nullptr,
	/*                c.sub */ nullptr,
	/*                c.xor */ nullptr,
	/*                 c.or */ nullptr,
	/*                c.and */ nullptr,
	/*               c.subw */ nullptr,
	/*               c.addw */ nullptr,
	/*                  c.j */ nullptr,
	/*               c.beqz */ nullptr,
	/*               c.bnez */ nullptr,
	/*               c.slli */ nullptr,
	/*              c.fldsp */ nullptr,
	/*               c.lwsp */ nullptr,
	/*              c.flwsp */ nullptr,
	/*                 c.jr */ nullptr,
	/*                 c.mv */ nullptr,
	/*             c.ebreak */ nullptr,
	/*               c.jalr */ nullptr,
	/*                c.add */ nullptr,
	/*              c.fsdsp */ nullptr,
	/*               c.swsp */ nullptr,
	/*              c.fswsp */ nullptr,
	/*                 c.ld */ nullptr,
	/*                 c.sd */ nullptr,
	/*              c.addiw */ nullptr,
	/*               c.ldsp */ nullptr,
	/*               c.sdsp */ nullptr,
	/*                 c.lq */ nullptr,
	/*                 c.sq */ nullptr,
	/*               c.lqsp */ nullptr,
	/*               c.sqsp */ nullptr,
	/*                  nop */ nullptr,
	/*                   mv */ nullptr,
	/*                  not */ nullptr,
	/*                  neg */ nullptr,
	/*                 negw */ nullptr,
	/*               sext.w */ nullptr,
	/*                 seqz */ nullptr,
	/*                 snez */ nullptr,
	/*                 sltz */ nullptr,
	/*                 sgtz */ nullptr,
	/*                fmv.s */ nullptr,
	/*               fabs.s */ nullptr,
	/*               fneg.s */ nullptr,
	/*                fmv.d */ nullptr,
	/*               fabs.d */ nullptr,
	/*               fneg.d */ nullptr,
	/*                fmv.q */ nullptr,
	/*               fabs.q */ nullptr,
	/*               fneg.q */ nullptr,
	/*                 beqz */ nullptr,
	/*                 bnez */ nullptr,
	/*                 blez */ nullptr,
	/*                 bgez */ nullptr,
	/*                 bltz */ nullptr,
	/*                 bgtz */ nullptr,
	/*                  ble */ nullptr,
	/*                 bleu */ nullptr,
	/*                  bgt */ nullptr,
	/*                 bgtu */ nullptr,
	/*                    j */ nullptr,
	/*                  ret */ nullptr,
	/*                   jr */ nullptr,
	/*              rdcycle */ nullptr,
	/*               rdtime */ nullptr,
	/*            rdinstret */ nullptr,
	/*             rdcycleh */ nullptr,
	/*              rdtimeh */ nullptr,
	/*           rdinstreth */ nullptr,
	/*                frcsr */ nullptr,
	/*                 frrm */ nullptr,
	/*              frflags */ nullptr,
	/*                fscsr */ nullptr,
	/*                 fsrm */ nullptr,
	/*              fsflags */ nullptr,
	/*                fsrmi */ nullptr,
	/*             fsflagsi */ nullptr,
};

const rv_comp_data* rv_inst_comp_rv128[] = {
	/*              unknown */ nullptr,
	/*                  lui */ nullptr,
	/*                auipc */ nullptr,
	/*                  jal */ nullptr,
	/*                 jalr */ nullptr,
	/*                  beq */ nullptr,
	/*                  bne */ nullptr,
	/*                  blt */ nullptr,
	/*                  bge */ nullptr,
	/*                 bltu */ nullptr,
	/*                 bgeu */ nullptr,
	/*                   lb */ nullptr,
	/*                   lh */ nullptr,
	/*                   lw */ nullptr,
	/*                  lbu */ nullptr,
	/*                  lhu */ nullptr,
	/*                   sb */ nullptr,
	/*                   sh */ nullptr,
	/*                   sw */ nullptr,
	/*                 addi */ nullptr,
	/*                 slti */ nullptr,
	/*                sltiu */ nullptr,
	/*                 xori */ nullptr,
	/*                  ori */ nullptr,
	/*                 andi */ nullptr,
	/*                 slli */ nullptr,
	/*                 srli */ nullptr,
	/*                 srai */ nullptr,
	/*                  add */ nullptr,
	/*                  sub */ nullptr,
	/*                  sll */ nullptr,
	/*                  slt */ nullptr,
	/*                 sltu */ nullptr,
	/*                  xor */ nullptr,
	/*                  srl */ nullptr,
	/*                  sra */ nullptr,
	/*                   or */ nullptr,
	/*                  and */ nullptr,
	/*                fence */ nullptr,
	/*              fence.i */ nullptr,
	/*                  lwu */ nullptr,
	/*                   ld */ nullptr,
	/*                   sd */ nullptr,
	/*                addiw */ nullptr,
	/*                slliw */ nullptr,
	/*                srliw */ nullptr,
	/*                sraiw */ nullptr,
	/*                 addw */ nullptr,
	/*                 subw */ nullptr,
	/*                 sllw */ nullptr,
	/*                 srlw */ nullptr,
	/*                 sraw */ nullptr,
	/*                  ldu */ nullptr,
	/*                   lq */ rvcd_rv128_lq,
	/*                   sq */ rvcd_rv128_sq,
	/*                addid */ nullptr,
	/*                sllid */ nullptr,
	/*                srlid */ nullptr,
	/*                sraid */ nullptr,
	/*                 addd */ nullptr,
	/*                 subd */ nullptr,
	/*                 slld */ nullptr,
	/*                 srld */ nullptr,
	/*                 srad */ nullptr,
	/*                  mul */ nullptr,
	/*                 mulh */ nullptr,
	/*               mulhsu */ nullptr,
	/*                mulhu */ nullptr,
	/*                  div */ nullptr,
	/*                 divu */ nullptr,
	/*                  rem */ nullptr,
	/*                 remu */ nullptr,
	/*                 mulw */ nullptr,
	/*                 divw */ nullptr,
	/*                divuw */ nullptr,
	/*                 remw */ nullptr,
	/*                remuw */ nullptr,
	/*                 muld */ nullptr,
	/*                 divd */ nullptr,
	/*                divud */ nullptr,
	/*                 remd */ nullptr,
	/*                remud */ nullptr,
	/*                 lr.w */ nullptr,
	/*                 sc.w */ nullptr,
	/*            amoswap.w */ nullptr,
	/*             amoadd.w */ nullptr,
	/*             amoxor.w */ nullptr,
	/*              amoor.w */ nullptr,
	/*             amoand.w */ nullptr,
	/*             amomin.w */ nullptr,
	/*             amomax.w */ nullptr,
	/*            amominu.w */ nullptr,
	/*            amomaxu.w */ nullptr,
	/*                 lr.d */ nullptr,
	/*                 sc.d */ nullptr,
	/*            amoswap.d */ nullptr,
	/*             amoadd.d */ nullptr,
	/*             amoxor.d */ nullptr,
	/*              amoor.d */ nullptr,
	/*             amoand.d */ nullptr,
	/*             amomin.d */ nullptr,
	/*             amomax.d */ nullptr,
	/*            amominu.d */ nullptr,
	/*            amomaxu.d */ nullptr,
	/*                 lr.q */ nullptr,
	/*                 sc.q */ nullptr,
	/*            amoswap.q */ nullptr,
	/*             amoadd.q */ nullptr,
	/*             amoxor.q */ nullptr,
	/*              amoor.q */ nullptr,
	/*             amoand.q */ nullptr,
	/*             amomin.q */ nullptr,
	/*             amomax.q */ nullptr,
	/*            amominu.q */ nullptr,
	/*            amomaxu.q */ nullptr,
	/*                ecall */ nullptr,
	/*               ebreak */ nullptr,
	/*                 uret */ nullptr,
	/*                 sret */ nullptr,
	/*                 hret */ nullptr,
	/*                 mret */ nullptr,
	/*                 dret */ nullptr,
	/*            sfence.vm */ nullptr,
	/*                  wfi */ nullptr,
	/*                csrrw */ nullptr,
	/*                csrrs */ nullptr,
	/*                csrrc */ nullptr,
	/*               csrrwi */ nullptr,
	/*               csrrsi */ nullptr,
	/*               csrrci */ nullptr,
	/*                  flw */ nullptr,
	/*                  fsw */ nullptr,
	/*              fmadd.s */ nullptr,
	/*              fmsub.s */ nullptr,
	/*             fnmsub.s */ nullptr,
	/*             fnmadd.s */ nullptr,
	/*               fadd.s */ nullptr,
	/*               fsub.s */ nullptr,
	/*               fmul.s */ nullptr,
	/*               fdiv.s */ nullptr,
	/*              fsgnj.s */ nullptr,
	/*             fsgnjn.s */ nullptr,
	/*             fsgnjx.s */ nullptr,
	/*               fmin.s */ nullptr,
	/*               fmax.s */ nullptr,
	/*              fsqrt.s */ nullptr,
	/*                fle.s */ nullptr,
	/*                flt.s */ nullptr,
	/*                feq.s */ nullptr,
	/*             fcvt.w.s */ nullptr,
	/*            fcvt.wu.s */ nullptr,
	/*             fcvt.s.w */ nullptr,
	/*            fcvt.s.wu */ nullptr,
	/*              fmv.x.s */ nullptr,
	/*             fclass.s */ nullptr,
	/*              fmv.s.x */ nullptr,
	/*             fcvt.l.s */ nullptr,
	/*            fcvt.lu.s */ nullptr,
	/*             fcvt.s.l */ nullptr,
	/*            fcvt.s.lu */ nullptr,
	/*                  fld */ nullptr,
	/*                  fsd */ nullptr,
	/*              fmadd.d */ nullptr,
	/*              fmsub.d */ nullptr,
	/*             fnmsub.d */ nullptr,
	/*             fnmadd.d */ nullptr,
	/*               fadd.d */ nullptr,
	/*               fsub.d */ nullptr,
	/*               fmul.d */ nullptr,
	/*               fdiv.d */ nullptr,
	/*              fsgnj.d */ nullptr,
	/*             fsgnjn.d */ nullptr,
	/*             fsgnjx.d */ nullptr,
	/*               fmin.d */ nullptr,
	/*               fmax.d */ nullptr,
	/*             fcvt.s.d */ nullptr,
	/*             fcvt.d.s */ nullptr,
	/*              fsqrt.d */ nullptr,
	/*                fle.d */ nullptr,
	/*                flt.d */ nullptr,
	/*                feq.d */ nullptr,
	/*             fcvt.w.d */ nullptr,
	/*            fcvt.wu.d */ nullptr,
	/*             fcvt.d.w */ nullptr,
	/*            fcvt.d.wu */ nullptr,
	/*             fclass.d */ nullptr,
	/*             fcvt.l.d */ nullptr,
	/*            fcvt.lu.d */ nullptr,
	/*              fmv.x.d */ nullptr,
	/*             fcvt.d.l */ nullptr,
	/*            fcvt.d.lu */ nullptr,
	/*              fmv.d.x */ nullptr,
	/*                  flq */ nullptr,
	/*                  fsq */ nullptr,
	/*              fmadd.q */ nullptr,
	/*              fmsub.q */ nullptr,
	/*             fnmsub.q */ nullptr,
	/*             fnmadd.q */ nullptr,
	/*               fadd.q */ nullptr,
	/*               fsub.q */ nullptr,
	/*               fmul.q */ nullptr,
	/*               fdiv.q */ nullptr,
	/*              fsgnj.q */ nullptr,
	/*             fsgnjn.q */ nullptr,
	/*             fsgnjx.q */ nullptr,
	/*               fmin.q */ nullptr,
	/*               fmax.q */ nullptr,
	/*             fcvt.s.q */ nullptr,
	/*             fcvt.q.s */ nullptr,
	/*             fcvt.d.q */ nullptr,
	/*             fcvt.q.d */ nullptr,
	/*              fsqrt.q */ nullptr,
	/*                fle.q */ nullptr,
	/*                flt.q */ nullptr,
	/*                feq.q */ nullptr,
	/*             fcvt.w.q */ nullptr,
	/*            fcvt.wu.q */ nullptr,
	/*             fcvt.q.w */ nullptr,
	/*            fcvt.q.wu */ nullptr,
	/*             fclass.q */ nullptr,
	/*             fcvt.l.q */ nullptr,
	/*            fcvt.lu.q */ nullptr,
	/*             fcvt.q.l */ nullptr,
	/*            fcvt.q.lu */ nullptr,
	/*              fmv.x.q */ nullptr,
	/*              fmv.q.x */ nullptr,
	/*           c.addi4spn */ nullptr,
	/*                c.fld */ nullptr,
	/*                 c.lw */ nullptr,
	/*                c.flw */ nullptr,
	/*                c.fsd */ nullptr,
	/*                 c.sw */ nullptr,
	/*                c.fsw */ nullptr,
	/*                c.nop */ nullptr,
	/*               c.addi */ nullptr,
	/*                c.jal */ nullptr,
	/*                 c.li */ nullptr,
	/*           c.addi16sp */ nullptr,
	/*                c.lui */ nullptr,
	/*               c.srli */ nullptr,
	/*               c.srai */ nullptr,
	/*               c.andi */ nullptr,
	/*                c.sub */ nullptr,
	/*                c.xor */ nullptr,
	/*                 c.or */ nullptr,
	/*                c.and */ nullptr,
	/*               c.subw */ nullptr,
	/*               c.addw */ nullptr,
	/*                  c.j */ nullptr,
	/*               c.beqz */ nullptr,
	/*               c.bnez */ nullptr,
	/*               c.slli */ nullptr,
	/*              c.fldsp */ nullptr,
	/*               c.lwsp */ nullptr,
	/*              c.flwsp */ nullptr,
	/*                 c.jr */ nullptr,
	/*                 c.mv */ nullptr,
	/*             c.ebreak */ nullptr,
	/*               c.jalr */ nullptr,
	/*                c.add */ nullptr,
	/*              c.fsdsp */ nullptr,
	/*               c.swsp */ nullptr,
	/*              c.fswsp */ nullptr,
	/*                 c.ld */ nullptr,
	/*                 c.sd */ nullptr,
	/*              c.addiw */ nullptr,
	/*               c.ldsp */ nullptr,
	/*               c.sdsp */ nullptr,
	/*                 c.lq */ nullptr,
	/*                 c.sq */ nullptr,
	/*               c.lqsp */ nullptr,
	/*               c.sqsp */ nullptr,
	/*                  nop */ nullptr,
	/*                   mv */ nullptr,
	/*                  not */ nullptr,
	/*                  neg */ nullptr,
	/*                 negw */ nullptr,
	/*               sext.w */ nullptr,
	/*                 seqz */ nullptr,
	/*                 snez */ nullptr,
	/*                 sltz */ nullptr,
	/*                 sgtz */ nullptr,
	/*                fmv.s */ nullptr,
	/*               fabs.s */ nullptr,
	/*               fneg.s */ nullptr,
	/*                fmv.d */ nullptr,
	/*               fabs.d */ nullptr,
	/*               fneg.d */ nullptr,
	/*                fmv.q */ nullptr,
	/*               fabs.q */ nullptr,
	/*               fneg.q */ nullptr,
	/*                 beqz */ nullptr,
	/*                 bnez */ nullptr,
	/*                 blez */ nullptr,
	/*                 bgez */ nullptr,
	/*                 bltz */ nullptr,
	/*                 bgtz */ nullptr,
	/*                  ble */ nullptr,
	/*                 bleu */ nullptr,
	/*                  bgt */ nullptr,
	/*                 bgtu */ nullptr,
	/*                    j */ nullptr,
	/*                  ret */ nullptr,
	/*                   jr */ nullptr,
	/*              rdcycle */ nullptr,
	/*               rdtime */ nullptr,
	/*            rdinstret */ nullptr,
	/*             rdcycleh */ nullptr,
	/*              rdtimeh */ nullptr,
	/*           rdinstreth */ nullptr,
	/*                frcsr */ nullptr,
	/*                 frrm */ nullptr,
	/*              frflags */ nullptr,
	/*                fscsr */ nullptr,
	/*                 fsrm */ nullptr,
	/*              fsflags */ nullptr,
	/*                fsrmi */ nullptr,
	/*             fsflagsi */ nullptr,
};

const int rv_inst_decomp_rv32[] = {
	/*              unknown */ rv_op_illegal,
	/*                  lui */ rv_op_illegal,
	/*                auipc */ rv_op_illegal,
	/*                  jal */ rv_op_illegal,
	/*                 jalr */ rv_op_illegal,
	/*                  beq */ rv_op_illegal,
	/*                  bne */ rv_op_illegal,
	/*                  blt */ rv_op_illegal,
	/*                  bge */ rv_op_illegal,
	/*                 bltu */ rv_op_illegal,
	/*                 bgeu */ rv_op_illegal,
	/*                   lb */ rv_op_illegal,
	/*                   lh */ rv_op_illegal,
	/*                   lw */ rv_op_illegal,
	/*                  lbu */ rv_op_illegal,
	/*                  lhu */ rv_op_illegal,
	/*                   sb */ rv_op_illegal,
	/*                   sh */ rv_op_illegal,
	/*                   sw */ rv_op_illegal,
	/*                 addi */ rv_op_illegal,
	/*                 slti */ rv_op_illegal,
	/*                sltiu */ rv_op_illegal,
	/*                 xori */ rv_op_illegal,
	/*                  ori */ rv_op_illegal,
	/*                 andi */ rv_op_illegal,
	/*                 slli */ rv_op_illegal,
	/*                 srli */ rv_op_illegal,
	/*                 srai */ rv_op_illegal,
	/*                  add */ rv_op_illegal,
	/*                  sub */ rv_op_illegal,
	/*                  sll */ rv_op_illegal,
	/*                  slt */ rv_op_illegal,
	/*                 sltu */ rv_op_illegal,
	/*                  xor */ rv_op_illegal,
	/*                  srl */ rv_op_illegal,
	/*                  sra */ rv_op_illegal,
	/*                   or */ rv_op_illegal,
	/*                  and */ rv_op_illegal,
	/*                fence */ rv_op_illegal,
	/*              fence.i */ rv_op_illegal,
	/*                  lwu */ rv_op_illegal,
	/*                   ld */ rv_op_illegal,
	/*                   sd */ rv_op_illegal,
	/*                addiw */ rv_op_illegal,
	/*                slliw */ rv_op_illegal,
	/*                srliw */ rv_op_illegal,
	/*                sraiw */ rv_op_illegal,
	/*                 addw */ rv_op_illegal,
	/*                 subw */ rv_op_illegal,
	/*                 sllw */ rv_op_illegal,
	/*                 srlw */ rv_op_illegal,
	/*                 sraw */ rv_op_illegal,
	/*                  ldu */ rv_op_illegal,
	/*                   lq */ rv_op_illegal,
	/*                   sq */ rv_op_illegal,
	/*                addid */ rv_op_illegal,
	/*                sllid */ rv_op_illegal,
	/*                srlid */ rv_op_illegal,
	/*                sraid */ rv_op_illegal,
	/*                 addd */ rv_op_illegal,
	/*                 subd */ rv_op_illegal,
	/*                 slld */ rv_op_illegal,
	/*                 srld */ rv_op_illegal,
	/*                 srad */ rv_op_illegal,
	/*                  mul */ rv_op_illegal,
	/*                 mulh */ rv_op_illegal,
	/*               mulhsu */ rv_op_illegal,
	/*                mulhu */ rv_op_illegal,
	/*                  div */ rv_op_illegal,
	/*                 divu */ rv_op_illegal,
	/*                  rem */ rv_op_illegal,
	/*                 remu */ rv_op_illegal,
	/*                 mulw */ rv_op_illegal,
	/*                 divw */ rv_op_illegal,
	/*                divuw */ rv_op_illegal,
	/*                 remw */ rv_op_illegal,
	/*                remuw */ rv_op_illegal,
	/*                 muld */ rv_op_illegal,
	/*                 divd */ rv_op_illegal,
	/*                divud */ rv_op_illegal,
	/*                 remd */ rv_op_illegal,
	/*                remud */ rv_op_illegal,
	/*                 lr.w */ rv_op_illegal,
	/*                 sc.w */ rv_op_illegal,
	/*            amoswap.w */ rv_op_illegal,
	/*             amoadd.w */ rv_op_illegal,
	/*             amoxor.w */ rv_op_illegal,
	/*              amoor.w */ rv_op_illegal,
	/*             amoand.w */ rv_op_illegal,
	/*             amomin.w */ rv_op_illegal,
	/*             amomax.w */ rv_op_illegal,
	/*            amominu.w */ rv_op_illegal,
	/*            amomaxu.w */ rv_op_illegal,
	/*                 lr.d */ rv_op_illegal,
	/*                 sc.d */ rv_op_illegal,
	/*            amoswap.d */ rv_op_illegal,
	/*             amoadd.d */ rv_op_illegal,
	/*             amoxor.d */ rv_op_illegal,
	/*              amoor.d */ rv_op_illegal,
	/*             amoand.d */ rv_op_illegal,
	/*             amomin.d */ rv_op_illegal,
	/*             amomax.d */ rv_op_illegal,
	/*            amominu.d */ rv_op_illegal,
	/*            amomaxu.d */ rv_op_illegal,
	/*                 lr.q */ rv_op_illegal,
	/*                 sc.q */ rv_op_illegal,
	/*            amoswap.q */ rv_op_illegal,
	/*             amoadd.q */ rv_op_illegal,
	/*             amoxor.q */ rv_op_illegal,
	/*              amoor.q */ rv_op_illegal,
	/*             amoand.q */ rv_op_illegal,
	/*             amomin.q */ rv_op_illegal,
	/*             amomax.q */ rv_op_illegal,
	/*            amominu.q */ rv_op_illegal,
	/*            amomaxu.q */ rv_op_illegal,
	/*                ecall */ rv_op_illegal,
	/*               ebreak */ rv_op_illegal,
	/*                 uret */ rv_op_illegal,
	/*                 sret */ rv_op_illegal,
	/*                 hret */ rv_op_illegal,
	/*                 mret */ rv_op_illegal,
	/*                 dret */ rv_op_illegal,
	/*            sfence.vm */ rv_op_illegal,
	/*                  wfi */ rv_op_illegal,
	/*                csrrw */ rv_op_illegal,
	/*                csrrs */ rv_op_illegal,
	/*                csrrc */ rv_op_illegal,
	/*               csrrwi */ rv_op_illegal,
	/*               csrrsi */ rv_op_illegal,
	/*               csrrci */ rv_op_illegal,
	/*                  flw */ rv_op_illegal,
	/*                  fsw */ rv_op_illegal,
	/*              fmadd.s */ rv_op_illegal,
	/*              fmsub.s */ rv_op_illegal,
	/*             fnmsub.s */ rv_op_illegal,
	/*             fnmadd.s */ rv_op_illegal,
	/*               fadd.s */ rv_op_illegal,
	/*               fsub.s */ rv_op_illegal,
	/*               fmul.s */ rv_op_illegal,
	/*               fdiv.s */ rv_op_illegal,
	/*              fsgnj.s */ rv_op_illegal,
	/*             fsgnjn.s */ rv_op_illegal,
	/*             fsgnjx.s */ rv_op_illegal,
	/*               fmin.s */ rv_op_illegal,
	/*               fmax.s */ rv_op_illegal,
	/*              fsqrt.s */ rv_op_illegal,
	/*                fle.s */ rv_op_illegal,
	/*                flt.s */ rv_op_illegal,
	/*                feq.s */ rv_op_illegal,
	/*             fcvt.w.s */ rv_op_illegal,
	/*            fcvt.wu.s */ rv_op_illegal,
	/*             fcvt.s.w */ rv_op_illegal,
	/*            fcvt.s.wu */ rv_op_illegal,
	/*              fmv.x.s */ rv_op_illegal,
	/*             fclass.s */ rv_op_illegal,
	/*              fmv.s.x */ rv_op_illegal,
	/*             fcvt.l.s */ rv_op_illegal,
	/*            fcvt.lu.s */ rv_op_illegal,
	/*             fcvt.s.l */ rv_op_illegal,
	/*            fcvt.s.lu */ rv_op_illegal,
	/*                  fld */ rv_op_illegal,
	/*                  fsd */ rv_op_illegal,
	/*              fmadd.d */ rv_op_illegal,
	/*              fmsub.d */ rv_op_illegal,
	/*             fnmsub.d */ rv_op_illegal,
	/*             fnmadd.d */ rv_op_illegal,
	/*               fadd.d */ rv_op_illegal,
	/*               fsub.d */ rv_op_illegal,
	/*               fmul.d */ rv_op_illegal,
	/*               fdiv.d */ rv_op_illegal,
	/*              fsgnj.d */ rv_op_illegal,
	/*             fsgnjn.d */ rv_op_illegal,
	/*             fsgnjx.d */ rv_op_illegal,
	/*               fmin.d */ rv_op_illegal,
	/*               fmax.d */ rv_op_illegal,
	/*             fcvt.s.d */ rv_op_illegal,
	/*             fcvt.d.s */ rv_op_illegal,
	/*              fsqrt.d */ rv_op_illegal,
	/*                fle.d */ rv_op_illegal,
	/*                flt.d */ rv_op_illegal,
	/*                feq.d */ rv_op_illegal,
	/*             fcvt.w.d */ rv_op_illegal,
	/*            fcvt.wu.d */ rv_op_illegal,
	/*             fcvt.d.w */ rv_op_illegal,
	/*            fcvt.d.wu */ rv_op_illegal,
	/*             fclass.d */ rv_op_illegal,
	/*             fcvt.l.d */ rv_op_illegal,
	/*            fcvt.lu.d */ rv_op_illegal,
	/*              fmv.x.d */ rv_op_illegal,
	/*             fcvt.d.l */ rv_op_illegal,
	/*            fcvt.d.lu */ rv_op_illegal,
	/*              fmv.d.x */ rv_op_illegal,
	/*                  flq */ rv_op_illegal,
	/*                  fsq */ rv_op_illegal,
	/*              fmadd.q */ rv_op_illegal,
	/*              fmsub.q */ rv_op_illegal,
	/*             fnmsub.q */ rv_op_illegal,
	/*             fnmadd.q */ rv_op_illegal,
	/*               fadd.q */ rv_op_illegal,
	/*               fsub.q */ rv_op_illegal,
	/*               fmul.q */ rv_op_illegal,
	/*               fdiv.q */ rv_op_illegal,
	/*              fsgnj.q */ rv_op_illegal,
	/*             fsgnjn.q */ rv_op_illegal,
	/*             fsgnjx.q */ rv_op_illegal,
	/*               fmin.q */ rv_op_illegal,
	/*               fmax.q */ rv_op_illegal,
	/*             fcvt.s.q */ rv_op_illegal,
	/*             fcvt.q.s */ rv_op_illegal,
	/*             fcvt.d.q */ rv_op_illegal,
	/*             fcvt.q.d */ rv_op_illegal,
	/*              fsqrt.q */ rv_op_illegal,
	/*                fle.q */ rv_op_illegal,
	/*                flt.q */ rv_op_illegal,
	/*                feq.q */ rv_op_illegal,
	/*             fcvt.w.q */ rv_op_illegal,
	/*            fcvt.wu.q */ rv_op_illegal,
	/*             fcvt.q.w */ rv_op_illegal,
	/*            fcvt.q.wu */ rv_op_illegal,
	/*             fclass.q */ rv_op_illegal,
	/*             fcvt.l.q */ rv_op_illegal,
	/*            fcvt.lu.q */ rv_op_illegal,
	/*             fcvt.q.l */ rv_op_illegal,
	/*            fcvt.q.lu */ rv_op_illegal,
	/*              fmv.x.q */ rv_op_illegal,
	/*              fmv.q.x */ rv_op_illegal,
	/*           c.addi4spn */ rv_op_addi,
	/*                c.fld */ rv_op_fld,
	/*                 c.lw */ rv_op_lw,
	/*                c.flw */ rv_op_flw,
	/*                c.fsd */ rv_op_fsd,
	/*                 c.sw */ rv_op_sw,
	/*                c.fsw */ rv_op_fsw,
	/*                c.nop */ rv_op_addi,
	/*               c.addi */ rv_op_addi,
	/*                c.jal */ rv_op_jal,
	/*                 c.li */ rv_op_addi,
	/*           c.addi16sp */ rv_op_addi,
	/*                c.lui */ rv_op_lui,
	/*               c.srli */ rv_op_srli,
	/*               c.srai */ rv_op_srai,
	/*               c.andi */ rv_op_andi,
	/*                c.sub */ rv_op_sub,
	/*                c.xor */ rv_op_xor,
	/*                 c.or */ rv_op_or,
	/*                c.and */ rv_op_and,
	/*               c.subw */ rv_op_subw,
	/*               c.addw */ rv_op_addw,
	/*                  c.j */ rv_op_jal,
	/*               c.beqz */ rv_op_beq,
	/*               c.bnez */ rv_op_bne,
	/*               c.slli */ rv_op_slli,
	/*              c.fldsp */ rv_op_fld,
	/*               c.lwsp */ rv_op_lw,
	/*              c.flwsp */ rv_op_flw,
	/*                 c.jr */ rv_op_jalr,
	/*                 c.mv */ rv_op_add,
	/*             c.ebreak */ rv_op_ebreak,
	/*               c.jalr */ rv_op_jalr,
	/*                c.add */ rv_op_add,
	/*              c.fsdsp */ rv_op_fsd,
	/*               c.swsp */ rv_op_sw,
	/*              c.fswsp */ rv_op_fsw,
	/*                 c.ld */ rv_op_illegal,
	/*                 c.sd */ rv_op_illegal,
	/*              c.addiw */ rv_op_illegal,
	/*               c.ldsp */ rv_op_illegal,
	/*               c.sdsp */ rv_op_illegal,
	/*                 c.lq */ rv_op_illegal,
	/*                 c.sq */ rv_op_illegal,
	/*               c.lqsp */ rv_op_illegal,
	/*               c.sqsp */ rv_op_illegal,
	/*                  nop */ rv_op_illegal,
	/*                   mv */ rv_op_illegal,
	/*                  not */ rv_op_illegal,
	/*                  neg */ rv_op_illegal,
	/*                 negw */ rv_op_illegal,
	/*               sext.w */ rv_op_illegal,
	/*                 seqz */ rv_op_illegal,
	/*                 snez */ rv_op_illegal,
	/*                 sltz */ rv_op_illegal,
	/*                 sgtz */ rv_op_illegal,
	/*                fmv.s */ rv_op_illegal,
	/*               fabs.s */ rv_op_illegal,
	/*               fneg.s */ rv_op_illegal,
	/*                fmv.d */ rv_op_illegal,
	/*               fabs.d */ rv_op_illegal,
	/*               fneg.d */ rv_op_illegal,
	/*                fmv.q */ rv_op_illegal,
	/*               fabs.q */ rv_op_illegal,
	/*               fneg.q */ rv_op_illegal,
	/*                 beqz */ rv_op_illegal,
	/*                 bnez */ rv_op_illegal,
	/*                 blez */ rv_op_illegal,
	/*                 bgez */ rv_op_illegal,
	/*                 bltz */ rv_op_illegal,
	/*                 bgtz */ rv_op_illegal,
	/*                  ble */ rv_op_illegal,
	/*                 bleu */ rv_op_illegal,
	/*                  bgt */ rv_op_illegal,
	/*                 bgtu */ rv_op_illegal,
	/*                    j */ rv_op_illegal,
	/*                  ret */ rv_op_illegal,
	/*                   jr */ rv_op_illegal,
	/*              rdcycle */ rv_op_illegal,
	/*               rdtime */ rv_op_illegal,
	/*            rdinstret */ rv_op_illegal,
	/*             rdcycleh */ rv_op_illegal,
	/*              rdtimeh */ rv_op_illegal,
	/*           rdinstreth */ rv_op_illegal,
	/*                frcsr */ rv_op_illegal,
	/*                 frrm */ rv_op_illegal,
	/*              frflags */ rv_op_illegal,
	/*                fscsr */ rv_op_illegal,
	/*                 fsrm */ rv_op_illegal,
	/*              fsflags */ rv_op_illegal,
	/*                fsrmi */ rv_op_illegal,
	/*             fsflagsi */ rv_op_illegal,
};

const int rv_inst_decomp_rv64[] = {
	/*              unknown */ rv_op_illegal,
	/*                  lui */ rv_op_illegal,
	/*                auipc */ rv_op_illegal,
	/*                  jal */ rv_op_illegal,
	/*                 jalr */ rv_op_illegal,
	/*                  beq */ rv_op_illegal,
	/*                  bne */ rv_op_illegal,
	/*                  blt */ rv_op_illegal,
	/*                  bge */ rv_op_illegal,
	/*                 bltu */ rv_op_illegal,
	/*                 bgeu */ rv_op_illegal,
	/*                   lb */ rv_op_illegal,
	/*                   lh */ rv_op_illegal,
	/*                   lw */ rv_op_illegal,
	/*                  lbu */ rv_op_illegal,
	/*                  lhu */ rv_op_illegal,
	/*                   sb */ rv_op_illegal,
	/*                   sh */ rv_op_illegal,
	/*                   sw */ rv_op_illegal,
	/*                 addi */ rv_op_illegal,
	/*                 slti */ rv_op_illegal,
	/*                sltiu */ rv_op_illegal,
	/*                 xori */ rv_op_illegal,
	/*                  ori */ rv_op_illegal,
	/*                 andi */ rv_op_illegal,
	/*                 slli */ rv_op_illegal,
	/*                 srli */ rv_op_illegal,
	/*                 srai */ rv_op_illegal,
	/*                  add */ rv_op_illegal,
	/*                  sub */ rv_op_illegal,
	/*                  sll */ rv_op_illegal,
	/*                  slt */ rv_op_illegal,
	/*                 sltu */ rv_op_illegal,
	/*                  xor */ rv_op_illegal,
	/*                  srl */ rv_op_illegal,
	/*                  sra */ rv_op_illegal,
	/*                   or */ rv_op_illegal,
	/*                  and */ rv_op_illegal,
	/*                fence */ rv_op_illegal,
	/*              fence.i */ rv_op_illegal,
	/*                  lwu */ rv_op_illegal,
	/*                   ld */ rv_op_illegal,
	/*                   sd */ rv_op_illegal,
	/*                addiw */ rv_op_illegal,
	/*                slliw */ rv_op_illegal,
	/*                srliw */ rv_op_illegal,
	/*                sraiw */ rv_op_illegal,
	/*                 addw */ rv_op_illegal,
	/*                 subw */ rv_op_illegal,
	/*                 sllw */ rv_op_illegal,
	/*                 srlw */ rv_op_illegal,
	/*                 sraw */ rv_op_illegal,
	/*                  ldu */ rv_op_illegal,
	/*                   lq */ rv_op_illegal,
	/*                   sq */ rv_op_illegal,
	/*                addid */ rv_op_illegal,
	/*                sllid */ rv_op_illegal,
	/*                srlid */ rv_op_illegal,
	/*                sraid */ rv_op_illegal,
	/*                 addd */ rv_op_illegal,
	/*                 subd */ rv_op_illegal,
	/*                 slld */ rv_op_illegal,
	/*                 srld */ rv_op_illegal,
	/*                 srad */ rv_op_illegal,
	/*                  mul */ rv_op_illegal,
	/*                 mulh */ rv_op_illegal,
	/*               mulhsu */ rv_op_illegal,
	/*                mulhu */ rv_op_illegal,
	/*                  div */ rv_op_illegal,
	/*                 divu */ rv_op_illegal,
	/*                  rem */ rv_op_illegal,
	/*                 remu */ rv_op_illegal,
	/*                 mulw */ rv_op_illegal,
	/*                 divw */ rv_op_illegal,
	/*                divuw */ rv_op_illegal,
	/*                 remw */ rv_op_illegal,
	/*                remuw */ rv_op_illegal,
	/*                 muld */ rv_op_illegal,
	/*                 divd */ rv_op_illegal,
	/*                divud */ rv_op_illegal,
	/*                 remd */ rv_op_illegal,
	/*                remud */ rv_op_illegal,
	/*                 lr.w */ rv_op_illegal,
	/*                 sc.w */ rv_op_illegal,
	/*            amoswap.w */ rv_op_illegal,
	/*             amoadd.w */ rv_op_illegal,
	/*             amoxor.w */ rv_op_illegal,
	/*              amoor.w */ rv_op_illegal,
	/*             amoand.w */ rv_op_illegal,
	/*             amomin.w */ rv_op_illegal,
	/*             amomax.w */ rv_op_illegal,
	/*            amominu.w */ rv_op_illegal,
	/*            amomaxu.w */ rv_op_illegal,
	/*                 lr.d */ rv_op_illegal,
	/*                 sc.d */ rv_op_illegal,
	/*            amoswap.d */ rv_op_illegal,
	/*             amoadd.d */ rv_op_illegal,
	/*             amoxor.d */ rv_op_illegal,
	/*              amoor.d */ rv_op_illegal,
	/*             amoand.d */ rv_op_illegal,
	/*             amomin.d */ rv_op_illegal,
	/*             amomax.d */ rv_op_illegal,
	/*            amominu.d */ rv_op_illegal,
	/*            amomaxu.d */ rv_op_illegal,
	/*                 lr.q */ rv_op_illegal,
	/*                 sc.q */ rv_op_illegal,
	/*            amoswap.q */ rv_op_illegal,
	/*             amoadd.q */ rv_op_illegal,
	/*             amoxor.q */ rv_op_illegal,
	/*              amoor.q */ rv_op_illegal,
	/*             amoand.q */ rv_op_illegal,
	/*             amomin.q */ rv_op_illegal,
	/*             amomax.q */ rv_op_illegal,
	/*            amominu.q */ rv_op_illegal,
	/*            amomaxu.q */ rv_op_illegal,
	/*                ecall */ rv_op_illegal,
	/*               ebreak */ rv_op_illegal,
	/*                 uret */ rv_op_illegal,
	/*                 sret */ rv_op_illegal,
	/*                 hret */ rv_op_illegal,
	/*                 mret */ rv_op_illegal,
	/*                 dret */ rv_op_illegal,
	/*            sfence.vm */ rv_op_illegal,
	/*                  wfi */ rv_op_illegal,
	/*                csrrw */ rv_op_illegal,
	/*                csrrs */ rv_op_illegal,
	/*                csrrc */ rv_op_illegal,
	/*               csrrwi */ rv_op_illegal,
	/*               csrrsi */ rv_op_illegal,
	/*               csrrci */ rv_op_illegal,
	/*                  flw */ rv_op_illegal,
	/*                  fsw */ rv_op_illegal,
	/*              fmadd.s */ rv_op_illegal,
	/*              fmsub.s */ rv_op_illegal,
	/*             fnmsub.s */ rv_op_illegal,
	/*             fnmadd.s */ rv_op_illegal,
	/*               fadd.s */ rv_op_illegal,
	/*               fsub.s */ rv_op_illegal,
	/*               fmul.s */ rv_op_illegal,
	/*               fdiv.s */ rv_op_illegal,
	/*              fsgnj.s */ rv_op_illegal,
	/*             fsgnjn.s */ rv_op_illegal,
	/*             fsgnjx.s */ rv_op_illegal,
	/*               fmin.s */ rv_op_illegal,
	/*               fmax.s */ rv_op_illegal,
	/*              fsqrt.s */ rv_op_illegal,
	/*                fle.s */ rv_op_illegal,
	/*                flt.s */ rv_op_illegal,
	/*                feq.s */ rv_op_illegal,
	/*             fcvt.w.s */ rv_op_illegal,
	/*            fcvt.wu.s */ rv_op_illegal,
	/*             fcvt.s.w */ rv_op_illegal,
	/*            fcvt.s.wu */ rv_op_illegal,
	/*              fmv.x.s */ rv_op_illegal,
	/*             fclass.s */ rv_op_illegal,
	/*              fmv.s.x */ rv_op_illegal,
	/*             fcvt.l.s */ rv_op_illegal,
	/*            fcvt.lu.s */ rv_op_illegal,
	/*             fcvt.s.l */ rv_op_illegal,
	/*            fcvt.s.lu */ rv_op_illegal,
	/*                  fld */ rv_op_illegal,
	/*                  fsd */ rv_op_illegal,
	/*              fmadd.d */ rv_op_illegal,
	/*              fmsub.d */ rv_op_illegal,
	/*             fnmsub.d */ rv_op_illegal,
	/*             fnmadd.d */ rv_op_illegal,
	/*               fadd.d */ rv_op_illegal,
	/*               fsub.d */ rv_op_illegal,
	/*               fmul.d */ rv_op_illegal,
	/*               fdiv.d */ rv_op_illegal,
	/*              fsgnj.d */ rv_op_illegal,
	/*             fsgnjn.d */ rv_op_illegal,
	/*             fsgnjx.d */ rv_op_illegal,
	/*               fmin.d */ rv_op_illegal,
	/*               fmax.d */ rv_op_illegal,
	/*             fcvt.s.d */ rv_op_illegal,
	/*             fcvt.d.s */ rv_op_illegal,
	/*              fsqrt.d */ rv_op_illegal,
	/*                fle.d */ rv_op_illegal,
	/*                flt.d */ rv_op_illegal,
	/*                feq.d */ rv_op_illegal,
	/*             fcvt.w.d */ rv_op_illegal,
	/*            fcvt.wu.d */ rv_op_illegal,
	/*             fcvt.d.w */ rv_op_illegal,
	/*            fcvt.d.wu */ rv_op_illegal,
	/*             fclass.d */ rv_op_illegal,
	/*             fcvt.l.d */ rv_op_illegal,
	/*            fcvt.lu.d */ rv_op_illegal,
	/*              fmv.x.d */ rv_op_illegal,
	/*             fcvt.d.l */ rv_op_illegal,
	/*            fcvt.d.lu */ rv_op_illegal,
	/*              fmv.d.x */ rv_op_illegal,
	/*                  flq */ rv_op_illegal,
	/*                  fsq */ rv_op_illegal,
	/*              fmadd.q */ rv_op_illegal,
	/*              fmsub.q */ rv_op_illegal,
	/*             fnmsub.q */ rv_op_illegal,
	/*             fnmadd.q */ rv_op_illegal,
	/*               fadd.q */ rv_op_illegal,
	/*               fsub.q */ rv_op_illegal,
	/*               fmul.q */ rv_op_illegal,
	/*               fdiv.q */ rv_op_illegal,
	/*              fsgnj.q */ rv_op_illegal,
	/*             fsgnjn.q */ rv_op_illegal,
	/*             fsgnjx.q */ rv_op_illegal,
	/*               fmin.q */ rv_op_illegal,
	/*               fmax.q */ rv_op_illegal,
	/*             fcvt.s.q */ rv_op_illegal,
	/*             fcvt.q.s */ rv_op_illegal,
	/*             fcvt.d.q */ rv_op_illegal,
	/*             fcvt.q.d */ rv_op_illegal,
	/*              fsqrt.q */ rv_op_illegal,
	/*                fle.q */ rv_op_illegal,
	/*                flt.q */ rv_op_illegal,
	/*                feq.q */ rv_op_illegal,
	/*             fcvt.w.q */ rv_op_illegal,
	/*            fcvt.wu.q */ rv_op_illegal,
	/*             fcvt.q.w */ rv_op_illegal,
	/*            fcvt.q.wu */ rv_op_illegal,
	/*             fclass.q */ rv_op_illegal,
	/*             fcvt.l.q */ rv_op_illegal,
	/*            fcvt.lu.q */ rv_op_illegal,
	/*             fcvt.q.l */ rv_op_illegal,
	/*            fcvt.q.lu */ rv_op_illegal,
	/*              fmv.x.q */ rv_op_illegal,
	/*              fmv.q.x */ rv_op_illegal,
	/*           c.addi4spn */ rv_op_addi,
	/*                c.fld */ rv_op_fld,
	/*                 c.lw */ rv_op_lw,
	/*                c.flw */ rv_op_illegal,
	/*                c.fsd */ rv_op_fsd,
	/*                 c.sw */ rv_op_sw,
	/*                c.fsw */ rv_op_illegal,
	/*                c.nop */ rv_op_addi,
	/*               c.addi */ rv_op_addi,
	/*                c.jal */ rv_op_illegal,
	/*                 c.li */ rv_op_addi,
	/*           c.addi16sp */ rv_op_addi,
	/*                c.lui */ rv_op_lui,
	/*               c.srli */ rv_op_srli,
	/*               c.srai */ rv_op_srai,
	/*               c.andi */ rv_op_andi,
	/*                c.sub */ rv_op_sub,
	/*                c.xor */ rv_op_xor,
	/*                 c.or */ rv_op_or,
	/*                c.and */ rv_op_and,
	/*               c.subw */ rv_op_subw,
	/*               c.addw */ rv_op_addw,
	/*                  c.j */ rv_op_jal,
	/*               c.beqz */ rv_op_beq,
	/*               c.bnez */ rv_op_bne,
	/*               c.slli */ rv_op_slli,
	/*              c.fldsp */ rv_op_fld,
	/*               c.lwsp */ rv_op_lw,
	/*              c.flwsp */ rv_op_illegal,
	/*                 c.jr */ rv_op_jalr,
	/*                 c.mv */ rv_op_add,
	/*             c.ebreak */ rv_op_ebreak,
	/*               c.jalr */ rv_op_jalr,
	/*                c.add */ rv_op_add,
	/*              c.fsdsp */ rv_op_fsd,
	/*               c.swsp */ rv_op_sw,
	/*              c.fswsp */ rv_op_illegal,
	/*                 c.ld */ rv_op_ld,
	/*                 c.sd */ rv_op_sd,
	/*              c.addiw */ rv_op_addiw,
	/*               c.ldsp */ rv_op_ld,
	/*               c.sdsp */ rv_op_sd,
	/*                 c.lq */ rv_op_illegal,
	/*                 c.sq */ rv_op_illegal,
	/*               c.lqsp */ rv_op_illegal,
	/*               c.sqsp */ rv_op_illegal,
	/*                  nop */ rv_op_illegal,
	/*                   mv */ rv_op_illegal,
	/*                  not */ rv_op_illegal,
	/*                  neg */ rv_op_illegal,
	/*                 negw */ rv_op_illegal,
	/*               sext.w */ rv_op_illegal,
	/*                 seqz */ rv_op_illegal,
	/*                 snez */ rv_op_illegal,
	/*                 sltz */ rv_op_illegal,
	/*                 sgtz */ rv_op_illegal,
	/*                fmv.s */ rv_op_illegal,
	/*               fabs.s */ rv_op_illegal,
	/*               fneg.s */ rv_op_illegal,
	/*                fmv.d */ rv_op_illegal,
	/*               fabs.d */ rv_op_illegal,
	/*               fneg.d */ rv_op_illegal,
	/*                fmv.q */ rv_op_illegal,
	/*               fabs.q */ rv_op_illegal,
	/*               fneg.q */ rv_op_illegal,
	/*                 beqz */ rv_op_illegal,
	/*                 bnez */ rv_op_illegal,
	/*                 blez */ rv_op_illegal,
	/*                 bgez */ rv_op_illegal,
	/*                 bltz */ rv_op_illegal,
	/*                 bgtz */ rv_op_illegal,
	/*                  ble */ rv_op_illegal,
	/*                 bleu */ rv_op_illegal,
	/*                  bgt */ rv_op_illegal,
	/*                 bgtu */ rv_op_illegal,
	/*                    j */ rv_op_illegal,
	/*                  ret */ rv_op_illegal,
	/*                   jr */ rv_op_illegal,
	/*              rdcycle */ rv_op_illegal,
	/*               rdtime */ rv_op_illegal,
	/*            rdinstret */ rv_op_illegal,
	/*             rdcycleh */ rv_op_illegal,
	/*              rdtimeh */ rv_op_illegal,
	/*           rdinstreth */ rv_op_illegal,
	/*                frcsr */ rv_op_illegal,
	/*                 frrm */ rv_op_illegal,
	/*              frflags */ rv_op_illegal,
	/*                fscsr */ rv_op_illegal,
	/*                 fsrm */ rv_op_illegal,
	/*              fsflags */ rv_op_illegal,
	/*                fsrmi */ rv_op_illegal,
	/*             fsflagsi */ rv_op_illegal,
};

const int rv_inst_decomp_rv128[] = {
	/*              unknown */ rv_op_illegal,
	/*                  lui */ rv_op_illegal,
	/*                auipc */ rv_op_illegal,
	/*                  jal */ rv_op_illegal,
	/*                 jalr */ rv_op_illegal,
	/*                  beq */ rv_op_illegal,
	/*                  bne */ rv_op_illegal,
	/*                  blt */ rv_op_illegal,
	/*                  bge */ rv_op_illegal,
	/*                 bltu */ rv_op_illegal,
	/*                 bgeu */ rv_op_illegal,
	/*                   lb */ rv_op_illegal,
	/*                   lh */ rv_op_illegal,
	/*                   lw */ rv_op_illegal,
	/*                  lbu */ rv_op_illegal,
	/*                  lhu */ rv_op_illegal,
	/*                   sb */ rv_op_illegal,
	/*                   sh */ rv_op_illegal,
	/*                   sw */ rv_op_illegal,
	/*                 addi */ rv_op_illegal,
	/*                 slti */ rv_op_illegal,
	/*                sltiu */ rv_op_illegal,
	/*                 xori */ rv_op_illegal,
	/*                  ori */ rv_op_illegal,
	/*                 andi */ rv_op_illegal,
	/*                 slli */ rv_op_illegal,
	/*                 srli */ rv_op_illegal,
	/*                 srai */ rv_op_illegal,
	/*                  add */ rv_op_illegal,
	/*                  sub */ rv_op_illegal,
	/*                  sll */ rv_op_illegal,
	/*                  slt */ rv_op_illegal,
	/*                 sltu */ rv_op_illegal,
	/*                  xor */ rv_op_illegal,
	/*                  srl */ rv_op_illegal,
	/*                  sra */ rv_op_illegal,
	/*                   or */ rv_op_illegal,
	/*                  and */ rv_op_illegal,
	/*                fence */ rv_op_illegal,
	/*              fence.i */ rv_op_illegal,
	/*                  lwu */ rv_op_illegal,
	/*                   ld */ rv_op_illegal,
	/*                   sd */ rv_op_illegal,
	/*                addiw */ rv_op_illegal,
	/*                slliw */ rv_op_illegal,
	/*                srliw */ rv_op_illegal,
	/*                sraiw */ rv_op_illegal,
	/*                 addw */ rv_op_illegal,
	/*                 subw */ rv_op_illegal,
	/*                 sllw */ rv_op_illegal,
	/*                 srlw */ rv_op_illegal,
	/*                 sraw */ rv_op_illegal,
	/*                  ldu */ rv_op_illegal,
	/*                   lq */ rv_op_illegal,
	/*                   sq */ rv_op_illegal,
	/*                addid */ rv_op_illegal,
	/*                sllid */ rv_op_illegal,
	/*                srlid */ rv_op_illegal,
	/*                sraid */ rv_op_illegal,
	/*                 addd */ rv_op_illegal,
	/*                 subd */ rv_op_illegal,
	/*                 slld */ rv_op_illegal,
	/*                 srld */ rv_op_illegal,
	/*                 srad */ rv_op_illegal,
	/*                  mul */ rv_op_illegal,
	/*                 mulh */ rv_op_illegal,
	/*               mulhsu */ rv_op_illegal,
	/*                mulhu */ rv_op_illegal,
	/*                  div */ rv_op_illegal,
	/*                 divu */ rv_op_illegal,
	/*                  rem */ rv_op_illegal,
	/*                 remu */ rv_op_illegal,
	/*                 mulw */ rv_op_illegal,
	/*                 divw */ rv_op_illegal,
	/*                divuw */ rv_op_illegal,
	/*                 remw */ rv_op_illegal,
	/*                remuw */ rv_op_illegal,
	/*                 muld */ rv_op_illegal,
	/*                 divd */ rv_op_illegal,
	/*                divud */ rv_op_illegal,
	/*                 remd */ rv_op_illegal,
	/*                remud */ rv_op_illegal,
	/*                 lr.w */ rv_op_illegal,
	/*                 sc.w */ rv_op_illegal,
	/*            amoswap.w */ rv_op_illegal,
	/*             amoadd.w */ rv_op_illegal,
	/*             amoxor.w */ rv_op_illegal,
	/*              amoor.w */ rv_op_illegal,
	/*             amoand.w */ rv_op_illegal,
	/*             amomin.w */ rv_op_illegal,
	/*             amomax.w */ rv_op_illegal,
	/*            amominu.w */ rv_op_illegal,
	/*            amomaxu.w */ rv_op_illegal,
	/*                 lr.d */ rv_op_illegal,
	/*                 sc.d */ rv_op_illegal,
	/*            amoswap.d */ rv_op_illegal,
	/*             amoadd.d */ rv_op_illegal,
	/*             amoxor.d */ rv_op_illegal,
	/*              amoor.d */ rv_op_illegal,
	/*             amoand.d */ rv_op_illegal,
	/*             amomin.d */ rv_op_illegal,
	/*             amomax.d */ rv_op_illegal,
	/*            amominu.d */ rv_op_illegal,
	/*            amomaxu.d */ rv_op_illegal,
	/*                 lr.q */ rv_op_illegal,
	/*                 sc.q */ rv_op_illegal,
	/*            amoswap.q */ rv_op_illegal,
	/*             amoadd.q */ rv_op_illegal,
	/*             amoxor.q */ rv_op_illegal,
	/*              amoor.q */ rv_op_illegal,
	/*             amoand.q */ rv_op_illegal,
	/*             amomin.q */ rv_op_illegal,
	/*             amomax.q */ rv_op_illegal,
	/*            amominu.q */ rv_op_illegal,
	/*            amomaxu.q */ rv_op_illegal,
	/*                ecall */ rv_op_illegal,
	/*               ebreak */ rv_op_illegal,
	/*                 uret */ rv_op_illegal,
	/*                 sret */ rv_op_illegal,
	/*                 hret */ rv_op_illegal,
	/*                 mret */ rv_op_illegal,
	/*                 dret */ rv_op_illegal,
	/*            sfence.vm */ rv_op_illegal,
	/*                  wfi */ rv_op_illegal,
	/*                csrrw */ rv_op_illegal,
	/*                csrrs */ rv_op_illegal,
	/*                csrrc */ rv_op_illegal,
	/*               csrrwi */ rv_op_illegal,
	/*               csrrsi */ rv_op_illegal,
	/*               csrrci */ rv_op_illegal,
	/*                  flw */ rv_op_illegal,
	/*                  fsw */ rv_op_illegal,
	/*              fmadd.s */ rv_op_illegal,
	/*              fmsub.s */ rv_op_illegal,
	/*             fnmsub.s */ rv_op_illegal,
	/*             fnmadd.s */ rv_op_illegal,
	/*               fadd.s */ rv_op_illegal,
	/*               fsub.s */ rv_op_illegal,
	/*               fmul.s */ rv_op_illegal,
	/*               fdiv.s */ rv_op_illegal,
	/*              fsgnj.s */ rv_op_illegal,
	/*             fsgnjn.s */ rv_op_illegal,
	/*             fsgnjx.s */ rv_op_illegal,
	/*               fmin.s */ rv_op_illegal,
	/*               fmax.s */ rv_op_illegal,
	/*              fsqrt.s */ rv_op_illegal,
	/*                fle.s */ rv_op_illegal,
	/*                flt.s */ rv_op_illegal,
	/*                feq.s */ rv_op_illegal,
	/*             fcvt.w.s */ rv_op_illegal,
	/*            fcvt.wu.s */ rv_op_illegal,
	/*             fcvt.s.w */ rv_op_illegal,
	/*            fcvt.s.wu */ rv_op_illegal,
	/*              fmv.x.s */ rv_op_illegal,
	/*             fclass.s */ rv_op_illegal,
	/*              fmv.s.x */ rv_op_illegal,
	/*             fcvt.l.s */ rv_op_illegal,
	/*            fcvt.lu.s */ rv_op_illegal,
	/*             fcvt.s.l */ rv_op_illegal,
	/*            fcvt.s.lu */ rv_op_illegal,
	/*                  fld */ rv_op_illegal,
	/*                  fsd */ rv_op_illegal,
	/*              fmadd.d */ rv_op_illegal,
	/*              fmsub.d */ rv_op_illegal,
	/*             fnmsub.d */ rv_op_illegal,
	/*             fnmadd.d */ rv_op_illegal,
	/*               fadd.d */ rv_op_illegal,
	/*               fsub.d */ rv_op_illegal,
	/*               fmul.d */ rv_op_illegal,
	/*               fdiv.d */ rv_op_illegal,
	/*              fsgnj.d */ rv_op_illegal,
	/*             fsgnjn.d */ rv_op_illegal,
	/*             fsgnjx.d */ rv_op_illegal,
	/*               fmin.d */ rv_op_illegal,
	/*               fmax.d */ rv_op_illegal,
	/*             fcvt.s.d */ rv_op_illegal,
	/*             fcvt.d.s */ rv_op_illegal,
	/*              fsqrt.d */ rv_op_illegal,
	/*                fle.d */ rv_op_illegal,
	/*                flt.d */ rv_op_illegal,
	/*                feq.d */ rv_op_illegal,
	/*             fcvt.w.d */ rv_op_illegal,
	/*            fcvt.wu.d */ rv_op_illegal,
	/*             fcvt.d.w */ rv_op_illegal,
	/*            fcvt.d.wu */ rv_op_illegal,
	/*             fclass.d */ rv_op_illegal,
	/*             fcvt.l.d */ rv_op_illegal,
	/*            fcvt.lu.d */ rv_op_illegal,
	/*              fmv.x.d */ rv_op_illegal,
	/*             fcvt.d.l */ rv_op_illegal,
	/*            fcvt.d.lu */ rv_op_illegal,
	/*              fmv.d.x */ rv_op_illegal,
	/*                  flq */ rv_op_illegal,
	/*                  fsq */ rv_op_illegal,
	/*              fmadd.q */ rv_op_illegal,
	/*              fmsub.q */ rv_op_illegal,
	/*             fnmsub.q */ rv_op_illegal,
	/*             fnmadd.q */ rv_op_illegal,
	/*               fadd.q */ rv_op_illegal,
	/*               fsub.q */ rv_op_illegal,
	/*               fmul.q */ rv_op_illegal,
	/*               fdiv.q */ rv_op_illegal,
	/*              fsgnj.q */ rv_op_illegal,
	/*             fsgnjn.q */ rv_op_illegal,
	/*             fsgnjx.q */ rv_op_illegal,
	/*               fmin.q */ rv_op_illegal,
	/*               fmax.q */ rv_op_illegal,
	/*             fcvt.s.q */ rv_op_illegal,
	/*             fcvt.q.s */ rv_op_illegal,
	/*             fcvt.d.q */ rv_op_illegal,
	/*             fcvt.q.d */ rv_op_illegal,
	/*              fsqrt.q */ rv_op_illegal,
	/*                fle.q */ rv_op_illegal,
	/*                flt.q */ rv_op_illegal,
	/*                feq.q */ rv_op_illegal,
	/*             fcvt.w.q */ rv_op_illegal,
	/*            fcvt.wu.q */ rv_op_illegal,
	/*             fcvt.q.w */ rv_op_illegal,
	/*            fcvt.q.wu */ rv_op_illegal,
	/*             fclass.q */ rv_op_illegal,
	/*             fcvt.l.q */ rv_op_illegal,
	/*            fcvt.lu.q */ rv_op_illegal,
	/*             fcvt.q.l */ rv_op_illegal,
	/*            fcvt.q.lu */ rv_op_illegal,
	/*              fmv.x.q */ rv_op_illegal,
	/*              fmv.q.x */ rv_op_illegal,
	/*           c.addi4spn */ rv_op_illegal,
	/*                c.fld */ rv_op_illegal,
	/*                 c.lw */ rv_op_illegal,
	/*                c.flw */ rv_op_illegal,
	/*                c.fsd */ rv_op_illegal,
	/*                 c.sw */ rv_op_illegal,
	/*                c.fsw */ rv_op_illegal,
	/*                c.nop */ rv_op_illegal,
	/*               c.addi */ rv_op_illegal,
	/*                c.jal */ rv_op_illegal,
	/*                 c.li */ rv_op_illegal,
	/*           c.addi16sp */ rv_op_illegal,
	/*                c.lui */ rv_op_illegal,
	/*               c.srli */ rv_op_illegal,
	/*               c.srai */ rv_op_illegal,
	/*               c.andi */ rv_op_illegal,
	/*                c.sub */ rv_op_illegal,
	/*                c.xor */ rv_op_illegal,
	/*                 c.or */ rv_op_illegal,
	/*                c.and */ rv_op_illegal,
	/*               c.subw */ rv_op_illegal,
	/*               c.addw */ rv_op_illegal,
	/*                  c.j */ rv_op_illegal,
	/*               c.beqz */ rv_op_illegal,
	/*               c.bnez */ rv_op_illegal,
	/*               c.slli */ rv_op_illegal,
	/*              c.fldsp */ rv_op_illegal,
	/*               c.lwsp */ rv_op_illegal,
	/*              c.flwsp */ rv_op_illegal,
	/*                 c.jr */ rv_op_illegal,
	/*                 c.mv */ rv_op_illegal,
	/*             c.ebreak */ rv_op_illegal,
	/*               c.jalr */ rv_op_illegal,
	/*                c.add */ rv_op_illegal,
	/*              c.fsdsp */ rv_op_illegal,
	/*               c.swsp */ rv_op_illegal,
	/*              c.fswsp */ rv_op_illegal,
	/*                 c.ld */ rv_op_illegal,
	/*                 c.sd */ rv_op_illegal,
	/*              c.addiw */ rv_op_illegal,
	/*               c.ldsp */ rv_op_illegal,
	/*               c.sdsp */ rv_op_illegal,
	/*                 c.lq */ rv_op_lq,
	/*                 c.sq */ rv_op_sq,
	/*               c.lqsp */ rv_op_lq,
	/*               c.sqsp */ rv_op_sq,
	/*                  nop */ rv_op_illegal,
	/*                   mv */ rv_op_illegal,
	/*                  not */ rv_op_illegal,
	/*                  neg */ rv_op_illegal,
	/*                 negw */ rv_op_illegal,
	/*               sext.w */ rv_op_illegal,
	/*                 seqz */ rv_op_illegal,
	/*                 snez */ rv_op_illegal,
	/*                 sltz */ rv_op_illegal,
	/*                 sgtz */ rv_op_illegal,
	/*                fmv.s */ rv_op_illegal,
	/*               fabs.s */ rv_op_illegal,
	/*               fneg.s */ rv_op_illegal,
	/*                fmv.d */ rv_op_illegal,
	/*               fabs.d */ rv_op_illegal,
	/*               fneg.d */ rv_op_illegal,
	/*                fmv.q */ rv_op_illegal,
	/*               fabs.q */ rv_op_illegal,
	/*               fneg.q */ rv_op_illegal,
	/*                 beqz */ rv_op_illegal,
	/*                 bnez */ rv_op_illegal,
	/*                 blez */ rv_op_illegal,
	/*                 bgez */ rv_op_illegal,
	/*                 bltz */ rv_op_illegal,
	/*                 bgtz */ rv_op_illegal,
	/*                  ble */ rv_op_illegal,
	/*                 bleu */ rv_op_illegal,
	/*                  bgt */ rv_op_illegal,
	/*                 bgtu */ rv_op_illegal,
	/*                    j */ rv_op_illegal,
	/*                  ret */ rv_op_illegal,
	/*                   jr */ rv_op_illegal,
	/*              rdcycle */ rv_op_illegal,
	/*               rdtime */ rv_op_illegal,
	/*            rdinstret */ rv_op_illegal,
	/*             rdcycleh */ rv_op_illegal,
	/*              rdtimeh */ rv_op_illegal,
	/*           rdinstreth */ rv_op_illegal,
	/*                frcsr */ rv_op_illegal,
	/*                 frrm */ rv_op_illegal,
	/*              frflags */ rv_op_illegal,
	/*                fscsr */ rv_op_illegal,
	/*                 fsrm */ rv_op_illegal,
	/*              fsflags */ rv_op_illegal,
	/*                fsrmi */ rv_op_illegal,
	/*             fsflagsi */ rv_op_illegal,
};

