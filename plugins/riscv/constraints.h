//
//  constraints.h
//
//  DANGER - This is machine generated code
//

#ifndef rv_constraints_h
#define rv_constraints_h

template <typename T>
inline bool constraint_check(T &dec, const rvc_constraint *c)
{
	auto imm = dec.imm;
	auto rd = dec.rd, rs1 = dec.rs1, rs2 = dec.rs2;
	while (*c != rvc_end) {
		switch (*c) {
			case rvc_simm_6:         if (!(imm >= -32 && imm < 32)) return false; break;
			case rvc_imm_6:          if (!(imm <= 0b111111)) return false; break;
			case rvc_imm_7:          if (!(imm <= 0b1111111)) return false; break;
			case rvc_imm_8:          if (!(imm <= 0b11111111)) return false; break;
			case rvc_imm_9:          if (!(imm <= 0b111111111)) return false; break;
			case rvc_imm_10:         if (!(imm <= 0b1111111111)) return false; break;
			case rvc_imm_12:         if (!(imm <= 0b111111111111)) return false; break;
			case rvc_imm_18:         if (!(imm <= 0b111111111111111111)) return false; break;
			case rvc_imm_nz:         if (!(imm != 0)) return false; break;
			case rvc_imm_x2:         if (!((imm & 0b1) == 0)) return false; break;
			case rvc_imm_x4:         if (!((imm & 0b11) == 0)) return false; break;
			case rvc_imm_x8:         if (!((imm & 0b111) == 0)) return false; break;
			case rvc_imm_x16:        if (!((imm & 0b1111) == 0)) return false; break;
			case rvc_rd_b3:          if (!(rd  >= 8 && rd  <= 15)) return false; break;
			case rvc_rs1_b3:         if (!(rs1 >= 8 && rs1 <= 15)) return false; break;
			case rvc_rs2_b3:         if (!(rs2 >= 8 && rs2 <= 15)) return false; break;
			case rvc_rd_eq_rs1:      if (!(rd == rs1)) return false; break;
			case rvc_rd_eq_ra:       if (!(rd == 1)) return false; break;
			case rvc_rd_eq_sp:       if (!(rd == 2)) return false; break;
			case rvc_rd_eq_x0:       if (!(rd == 0)) return false; break;
			case rvc_rs1_eq_sp:      if (!(rs1 == 2)) return false; break;
			case rvc_rs1_eq_x0:      if (!(rs1 == 0)) return false; break;
			case rvc_rs2_eq_x0:      if (!(rs2 == 0)) return false; break;
			case rvc_rd_ne_x0_x2:    if (!(rd != 0 && rd != 2)) return false; break;
			case rvc_rd_ne_x0:       if (!(rd != 0)) return false; break;
			case rvc_rs1_ne_x0:      if (!(rs1 != 0)) return false; break;
			case rvc_rs2_ne_x0:      if (!(rs2 != 0)) return false; break;
			case rvc_rs2_eq_rs1:     if (!(rs2 == rs1)) return false; break;
			case rvc_rs1_eq_ra:      if (!(rs1 == 1)) return false; break;
			case rvc_imm_eq_zero:    if (!(imm == 0)) return false; break;
			case rvc_imm_eq_n1:      if (!(imm == -1)) return false; break;
			case rvc_imm_eq_p1:      if (!(imm == 1)) return false; break;
			case rvc_csr_eq_0x001:   if (!(imm == 0x001)) return false; break;
			case rvc_csr_eq_0x002:   if (!(imm == 0x002)) return false; break;
			case rvc_csr_eq_0x003:   if (!(imm == 0x003)) return false; break;
			case rvc_csr_eq_0xc00:   if (!(imm == 0xc00)) return false; break;
			case rvc_csr_eq_0xc01:   if (!(imm == 0xc01)) return false; break;
			case rvc_csr_eq_0xc02:   if (!(imm == 0xc02)) return false; break;
			case rvc_csr_eq_0xc80:   if (!(imm == 0xc80)) return false; break;
			case rvc_csr_eq_0xc81:   if (!(imm == 0xc81)) return false; break;
			case rvc_csr_eq_0xc82:   if (!(imm == 0xc82)) return false; break;
			default:                 break;
		}
		c++;
	}
	return true;
}

template <typename T>
inline void constraint_set(T &dec, const rvc_constraint *c)
{
	auto &imm = dec.imm;
	auto &rd = dec.rd, &rs1 = dec.rs1, &rs2 = dec.rs2;
	while (*c != rvc_end) {
		switch (*c) {
			case rvc_rd_eq_rs1:      rd = rs1; break;
			case rvc_rd_eq_ra:       rd = 1; break;
			case rvc_rd_eq_sp:       rd = 2; break;
			case rvc_rd_eq_x0:       rd = 0; break;
			case rvc_rs1_eq_sp:      rs1 = 2; break;
			case rvc_rs1_eq_x0:      rs1 = 0; break;
			case rvc_rs2_eq_x0:      rs2 = 0; break;
			case rvc_rs2_eq_rs1:     rs2 = rs1; break;
			case rvc_rs1_eq_ra:      rs1 = 1; break;
			case rvc_imm_eq_zero:    imm = 0; break;
			case rvc_imm_eq_n1:      imm = -1; break;
			case rvc_imm_eq_p1:      imm = 1; break;
			case rvc_csr_eq_0x001:   imm = 0x001; break;
			case rvc_csr_eq_0x002:   imm = 0x002; break;
			case rvc_csr_eq_0x003:   imm = 0x003; break;
			case rvc_csr_eq_0xc00:   imm = 0xc00; break;
			case rvc_csr_eq_0xc01:   imm = 0xc01; break;
			case rvc_csr_eq_0xc02:   imm = 0xc02; break;
			case rvc_csr_eq_0xc80:   imm = 0xc80; break;
			case rvc_csr_eq_0xc81:   imm = 0xc81; break;
			case rvc_csr_eq_0xc82:   imm = 0xc82; break;
			default:                 break;
		}
		c++;
	}
}

#endif
