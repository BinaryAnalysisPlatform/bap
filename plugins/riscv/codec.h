//
//  codec.h
//

#ifndef rv_codec_h
#define rv_codec_h

/*
 *
 * Instruction length
 * ==================
 * Returns the instruction length, either 2, 4, 6 or 8 bytes.
 *
 *   inline size_t riscv::inst_length(riscv::inst_t inst)
 *
 * Instruction fetch
 * =================
 * Returns the instruction and its length
 *
 *   inline size_t riscv::inst_fetch(riscv::addr_t addr, riscv::addr_t *pc_offset)
 *
 * Decoding instructions
 * =====================
 * The decode functions decode the instruction passed as an argument in to
 * struct rv_decode using: op, codec, imm, rd, rs1, rs2, etc.
 * The encode function only depends on the fields in rv_decode.
 *
 *   template <typename T> inline void riscv::decode_inst_rv32(T &dec, riscv::inst_t inst)
 *   template <typename T> inline void riscv::decode_inst_rv64(T &dec, riscv::inst_t inst)
 *
 * Encoding instructions
 * =====================
 * The encode function encodes the operands in struct rv_decode using:
 * op, imm, rd, rs1, rs2, etc. The encode function only depends on 
 * rv_decode fields and it is up to the caller to save the instruction.
 * Returns the encoded instruction.
 *
 *   template <typename T> inline riscv::inst_t riscv::encode_inst(T &dec)
 *
 * Pseudo instructions
 * ===================
 * Pseudo instructions can be decoded by calling decode_pseudo_inst.
 * This will use constraints to transform a regular instruction into a
 * pseudo instruction.
 *
 *	 template <typename T> inline bool riscv::decode_pseudo_inst(T &dec)
 *
 * Pseudo instructions can be encoded by calling encode_psuedo
 * to translate it to a regular instruction (false is returned if the
 * intruction is not a valid pseudo-instruction) followed by calling
 * the normal enocde_inst function.
 *
 *   template <typename T> inline bool riscv::encode_pseudo(T &dec)
 *
 * Decompressing instructions
 * ==========================
 * The decompress functions work on an already decoded instruction and
 * they just set the op and codec field if the instruction is compressed.
 *
 *   template <typename T> inline void riscv::decompress_inst_rv32(T &dec)
 *   template <typename T> inline void riscv::decompress_inst_rv64(T &dec)
 *   template <typename T> inline void riscv::decompress_inst_rv128(T &dec)
 *
 * Compressing instructions
 * ========================
 * The compress functions work on an already decoded instruction and
 * they just set the op and codec field if the instruction is compressed.
 * Returns false if the instruction cannot be compressed.
 *
 *   template <typename T> inline bool riscv::compress_inst_rv32(T &dec)
 *   template <typename T> inline bool riscv::compress_inst_rv64(T &dec)
 *   template <typename T> inline bool riscv::compress_inst_rv128(T &dec)
 *
 */

/*
 * Decoded Instruction
 *
 * Structure that contains instruction decode information.
 */

namespace riscv
{

	#include "operands.h"
	#include "decode.h"
	#include "encode.h"
	#include "switch.h"
	#include "constraints.h"

	/* Instruction Set Combinations */

	enum rv_set {
		rv_set_none,
		rv_set_i,
		rv_set_im,
		rv_set_ima,
		rv_set_imac,
		rv_set_imafd,
		rv_set_imafdc,
	};

	/* CSR permissions */

	enum csr_perm {
		csr_rw,       /* Read/Write CSR */
		csr_rs,       /* Read/Set CSR */
		csr_rc        /* Read/Clear CSR */
	};

	/* Decode */

	struct decode
	{
		int32_t  imm;        /* decoded immediate */
		uint8_t  rd;         /* (5 bits) byte aligned for performance */
		uint8_t  rs1;        /* (5 bits) byte aligned for performance */
		uint8_t  rs2;        /* (5 bits) byte aligned for performance */
		uint8_t  rs3;        /* (5 bits) byte aligned for performance */
		uint16_t op    : 10; /* (>256 entries) nearly full */
		uint16_t codec : 6;  /* (>32 entries) can grow */
		uint8_t  rm    : 3;  /* round mode for some FPU ops */
		uint8_t  aq    : 1;  /* acquire for atomic ops */
		uint8_t  rl    : 1;  /* release for atomic ops */
		uint8_t  pred  : 4;  /* pred for fence */
		uint8_t  succ  : 4;  /* succ for fence */

		decode()
			: imm(0), rd(0), rs1(0), rs2(0), rs3(0), op(0), codec(0), rm(0), aq(0), rl(0), pred(0), succ(0) {}
	};


	/* Instruction Length */

	inline size_t inst_length(inst_t inst)
	{
		/* NOTE: supports maximum instruction size of 64-bits */

		/* instruction length coding
		 *
		 *      aa - 16 bit aa != 11
		 *   bbb11 - 32 bit bbb != 111
		 *  011111 - 48 bit
		 * 0111111 - 64 bit
		 */

		return (inst &      0b11) != 0b11      ? 2
			 : (inst &   0b11100) != 0b11100   ? 4
			 : (inst &  0b111111) == 0b011111  ? 6
			 : (inst & 0b1111111) == 0b0111111 ? 8
			 : 0;
	}

	/* Fetch Instruction */

	template <typename UX>
	inline inst_t inst_fetch(addr_t addr, UX &pc_offset)
	{
		/* NOTE: supports maximum instruction size of 64-bits */

		/* optimistically read 32-bit instruction */
		inst_t inst = htole32(*(uint32_t*)addr);
		if ((inst & 0b11) != 0b11) {
			inst &= 0xffff; // mask to 16-bits
			pc_offset = 2;
		} else if ((inst & 0b11100) != 0b11100) {
			pc_offset = 4;
		} else if ((inst & 0b111111) == 0b011111) {
			inst |= inst_t(htole16(*(uint16_t*)(addr + 4))) << 32;
			pc_offset = 6;
		} else if ((inst & 0b1111111) == 0b0111111) {
			inst |= inst_t(htole32(*(uint32_t*)(addr + 4))) << 32;
			pc_offset = 8;
		} else {
			pc_offset = inst = 0; /* illegal instruction */
		}
		return inst;
	}

	/* Decompress Instruction */

	template <typename T>
	inline void decompress_inst_rv32(T &dec)
	{
	    int decomp_op = rv_inst_decomp_rv32[dec.op];
	    if (decomp_op != rv_op_illegal) {
	        dec.op = decomp_op;
	        dec.codec = rv_inst_codec[decomp_op];
	    }
	}

	template <typename T>
	inline void decompress_inst_rv64(T &dec)
	{
	    int decomp_op = rv_inst_decomp_rv64[dec.op];
	    if (decomp_op != rv_op_illegal) {
	        dec.op = decomp_op;
	        dec.codec = rv_inst_codec[decomp_op];
	    }
	}

	template <typename T>
	inline void decompress_inst_rv128(T &dec)
	{
	    int decomp_op = rv_inst_decomp_rv128[dec.op];
	    if (decomp_op != rv_op_illegal) {
	        dec.op = decomp_op;
	        dec.codec = rv_inst_codec[decomp_op];
	    }
	}

	/* Decode Instruction */

	template <typename T, bool rv32, bool rv64, bool rv128, bool rvi = true, bool rvm = true, bool rva = true, bool rvs = true, bool rvf = true, bool rvd = true, bool rvq = true, bool rvc = true>
	inline void decode_inst(T &dec, inst_t inst)
	{
		dec.op = decode_inst_op<rv32,rv64,rv128,rvi,rvm,rva,rvs,rvf,rvd,rvq,rvc>(inst);
		decode_inst_type<T>(dec, inst);
	}

	template <typename T>
	inline void decode_inst_rv32(T &dec, inst_t inst)
	{
		decode_inst<T,true,false,false>(dec, inst);
		decompress_inst_rv32<T>(dec);
	}

	template <typename T>
	inline void decode_inst_rv64(T &dec, inst_t inst)
	{
		decode_inst<T,false,true,false>(dec, inst);
		decompress_inst_rv64<T>(dec);
	}

	template <typename T>
	inline void decode_inst_rv128(T &dec, inst_t inst)
	{
		decode_inst<T,false,false,true>(dec, inst);
		decompress_inst_rv128<T>(dec);
	}


	/* Decode Pseudoinstruction */

	template <typename T>
	inline bool decode_pseudo_inst(T &dec)
	{
		const rv_comp_data *comp_data = rv_inst_pseudo[dec.op];
		if (!comp_data) return false;
		while (comp_data->constraints) {
			if (constraint_check(dec, comp_data->constraints)) {
				dec.op = comp_data->op;
				dec.codec = rv_inst_codec[dec.op];
				return true;
			}
			comp_data++;
		}
		return false;
	}


	/* Compress Instruction */

	template <typename T>
	inline bool compress_inst_rv32(T &dec)
	{
		const rv_comp_data *comp_data = rv_inst_comp_rv32[dec.op];
		if (!comp_data) return false;
		while (comp_data->constraints) {
			if (constraint_check(dec, comp_data->constraints)) {
				dec.op = comp_data->op;
				dec.codec = rv_inst_codec[dec.op];
				return true;
			}
			comp_data++;
		}
		return false;
	}

	template <typename T>
	inline bool compress_inst_rv64(T &dec)
	{
		const rv_comp_data *comp_data = rv_inst_comp_rv64[dec.op];
		if (!comp_data) return false;
		while (comp_data->constraints) {
			if (constraint_check(dec, comp_data->constraints)) {
				dec.op = comp_data->op;
				dec.codec = rv_inst_codec[dec.op];
				return true;
			}
			comp_data++;
		}
		return false;
	}

	template <typename T>
	inline bool compress_inst_rv128(T &dec)
	{
		const rv_comp_data *comp_data = rv_inst_comp_rv128[dec.op];
		if (!comp_data) return false;
		while (comp_data->constraints) {
			if (constraint_check(dec, comp_data->constraints)) {
				dec.op = comp_data->op;
				dec.codec = rv_inst_codec[dec.op];
				return true;
			}
			comp_data++;
		}
		return false;
	}


	/* Encode Pseudoinstruction */

	template <typename T>
	inline bool encode_pseudo(T &dec)
	{
		const rv_comp_data *comp_data = &rv_inst_depseudo[dec.op];
		if (!comp_data->constraints) return false;
		dec.op = comp_data->op;
		dec.codec = rv_inst_codec[dec.op];
		constraint_set(dec, comp_data->constraints);
		return true;
	}

}

#endif
