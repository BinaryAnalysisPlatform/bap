//
//  decode.h
//

#ifndef rv_decode_h
#define rv_decode_h

/* Instruction decoders */

/* Decode none */
template <typename T> inline void decode_none(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.imm = 0;
}

/* Decode C nop */
template <typename T> inline void decode_ci_none(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.imm = 0;
}

/* Decode CR */
template <typename T> inline void decode_cr(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rd::decode(inst);
	dec.rs2 = operand_crs2::decode(inst);
	dec.imm = 0;
}

/* Decode CR mv */
template <typename T> inline void decode_cr_mv(T &dec, inst_t inst)
{
	dec.rd = operand_crd::decode(inst);
	dec.rs1 = rv_ireg_zero;
	dec.rs2 = operand_crs2::decode(inst);
	dec.imm = 0;
}

/* Decode CR jalr */
template <typename T> inline void decode_cr_jalr(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_ra;
	dec.rs1 = operand_crs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = 0;
}

/* Decode CR jr */
template <typename T> inline void decode_cr_jr(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_crs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = 0;
}

/* Decode CI */
template <typename T> inline void decode_ci(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rd::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmi::decode(inst);
}

/* Decode CI shamt5 */
template <typename T> inline void decode_ci_sh5(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rd::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmsh5::decode(inst);
}

/* Decode CI shamt6 */
template <typename T> inline void decode_ci_sh6(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rd::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmsh6::decode(inst);
}

/* Decode CI li */
template <typename T> inline void decode_ci_li(T &dec, inst_t inst)
{
	dec.rd = operand_crd::decode(inst);
	dec.rs1 = rv_ireg_zero;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmi::decode(inst);
}

/* Decode CI lui */
template <typename T> inline void decode_ci_lui(T &dec, inst_t inst)
{
	dec.rd = operand_crd::decode(inst);
	dec.rs1 = rv_ireg_zero;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmui::decode(inst);
}

/* Decode CI lwsp */
template <typename T> inline void decode_ci_lwsp(T &dec, inst_t inst)
{
	dec.rd = operand_crd::decode(inst);
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmlwsp::decode(inst);
}

/* Decode CI ldsp */
template <typename T> inline void decode_ci_ldsp(T &dec, inst_t inst)
{
	dec.rd = operand_crd::decode(inst);
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmldsp::decode(inst);
}

/* Decode CI lqsp */
template <typename T> inline void decode_ci_lqsp(T &dec, inst_t inst)
{
	dec.rd = operand_crd::decode(inst);
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmlqsp::decode(inst);
}

/* Decode CI 16sp */
template <typename T> inline void decode_ci_16sp(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_sp;
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimm16sp::decode(inst);
}

/* Decode CSS swsp */
template <typename T> inline void decode_css_swsp(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = operand_crs2::decode(inst);
	dec.imm = operand_cimmswsp::decode(inst);
}

/* Decode CSS sdsp */
template <typename T> inline void decode_css_sdsp(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = operand_crs2::decode(inst);
	dec.imm = operand_cimmsdsp::decode(inst);
}

/* Decode CSS sqsp */
template <typename T> inline void decode_css_sqsp(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = operand_crs2::decode(inst);
	dec.imm = operand_cimmsqsp::decode(inst);
}

/* Decode CIW 4spn */
template <typename T> inline void decode_ciw_4spn(T &dec, inst_t inst)
{
	dec.rd = operand_crdq::decode(inst) + 8;
	dec.rs1 = rv_ireg_sp;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimm4spn::decode(inst);
}

/* Decode CL lw */
template <typename T> inline void decode_cl_lw(T &dec, inst_t inst)
{
	dec.rd = operand_crdq::decode(inst) + 8;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmw::decode(inst);
}

/* Decode CL ld */
template <typename T> inline void decode_cl_ld(T &dec, inst_t inst)
{
	dec.rd = operand_crdq::decode(inst) + 8;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmd::decode(inst);
}

/* Decode CL lq */
template <typename T> inline void decode_cl_lq(T &dec, inst_t inst)
{
	dec.rd = operand_crdq::decode(inst) + 8;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmq::decode(inst);
}

/* Decode CS f */
template <typename T> inline void decode_cs(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rdq::decode(inst) + 8;
	dec.rs2 = operand_crs2q::decode(inst) + 8;
	dec.imm = 0;
}

/* Decode CS sw */
template <typename T> inline void decode_cs_sw(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = operand_crs2q::decode(inst) + 8;
	dec.imm = operand_cimmw::decode(inst);
}
/* Decode CS sd */
template <typename T> inline void decode_cs_sd(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = operand_crs2q::decode(inst) + 8;
	dec.imm = operand_cimmd::decode(inst);
}

/* Decode CS sq */
template <typename T> inline void decode_cs_sq(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = operand_crs2q::decode(inst) + 8;
	dec.imm = operand_cimmq::decode(inst);
}

/* Decode CB */
template <typename T> inline void decode_cb(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_crs1q::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmb::decode(inst);
}

/* Decode CB imm */
template <typename T> inline void decode_cb_imm(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rdq::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmi::decode(inst);
}

/* Decode CB shamt5 */
template <typename T> inline void decode_cb_sh5(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rdq::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmsh5::decode(inst);
}

/* Decode CB shamt6 */
template <typename T> inline void decode_cb_sh6(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = operand_crs1rdq::decode(inst) + 8;
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmsh6::decode(inst);
}

/* Decode CJ */
template <typename T> inline void decode_cj(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmj::decode(inst);
}

/* Decode CJ jal */
template <typename T> inline void decode_cj_jal(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_ra;
	dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.imm = operand_cimmj::decode(inst);
}

/* Decode R */
template <typename T> inline void decode_r(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = operand_rs2::decode(inst);
	dec.imm = 0;
}

/* Decode R RM */
template <typename T> inline void decode_r_m(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = operand_rs2::decode(inst);
	dec.imm = 0;
	dec.rm = operand_rm::decode(inst);
}

/* Decode R AMO L */
template <typename T> inline void decode_r_l(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = 0;
	dec.aq = operand_aq::decode(inst);
	dec.rl = operand_rl::decode(inst);
}

/* Decode R AMO S */
template <typename T> inline void decode_r_a(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = operand_rs2::decode(inst);
	dec.imm = 0;
	dec.aq = operand_aq::decode(inst);
	dec.rl = operand_rl::decode(inst);
}

/* Decode R 4f */
template <typename T> inline void decode_r4_m(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = operand_rs2::decode(inst);
	dec.rs3 = operand_rs3::decode(inst);
	dec.imm = 0;
	dec.rm = operand_rm::decode(inst);
}

/* Decode R fence */
template <typename T> inline void decode_r_f(T &dec, inst_t inst)
{
	dec.rd = dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.pred = operand_pred::decode(inst);
	dec.succ = operand_succ::decode(inst);
	dec.imm = 0;
}

/* Decode I */
template <typename T> inline void decode_i(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_imm12::decode(inst);
}

/* Decode I CSR */
template <typename T> inline void decode_i_csr(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_csr12::decode(inst);
}

/* Decode I sh5 */
template <typename T> inline void decode_i_sh5(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_shamt5::decode(inst);
}

/* Decode I sh6 */
template <typename T> inline void decode_i_sh6(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_shamt6::decode(inst);
}

/* Decode I sh7 */
template <typename T> inline void decode_i_sh7(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = rv_ireg_zero;
	dec.imm = operand_shamt7::decode(inst);
}

/* Decode S Store */
template <typename T> inline void decode_s(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = operand_rs2::decode(inst);
	dec.imm = operand_simm12::decode(inst);
}

/* Decode SB Branch */
template <typename T> inline void decode_sb(T &dec, inst_t inst)
{
	dec.rd = rv_ireg_zero;
	dec.rs1 = operand_rs1::decode(inst);
	dec.rs2 = operand_rs2::decode(inst);
	dec.imm = operand_sbimm12::decode(inst);
}

/* Decode U */
template <typename T> inline void decode_u(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.imm = operand_imm20::decode(inst);
}

/* Decode UJ */
template <typename T> inline void decode_uj(T &dec, inst_t inst)
{
	dec.rd = operand_rd::decode(inst);
	dec.rs1 = dec.rs2 = rv_ireg_zero;
	dec.imm = operand_jimm20::decode(inst);
}

#endif
