//
//  types.h
//

#ifndef rv_types_h
#define rv_types_h

#include <cstdint>

namespace riscv {

	/*
	 * Host type checks
	 */

	static_assert(sizeof(short) == 2, "requires 16-bit short");
	static_assert(sizeof(int) == 4, "requires 32-bit int");
	static_assert(sizeof(long long) == 8, "requires 64-bit long long");
	static_assert(sizeof(float) == 4, "requires 32-bit float");
	static_assert(sizeof(double) == 8, "requires 64-bit double");

	/*
	 * Short-hand type aliases
	 *
	 * The purpose is to use a placeholder type globally for the natural
	 * register width of the target. The placeholder type names match the
	 * C pseudo-code cast notation in meta/instructions
	 *
	 * Type aliases are defined here instead of using stdint.h types due to
	 * the use of signed long int and unsigned long int for s64 and u64
	 * by some library headers. These definitions are compatible with ILP32, LLP64
	 * and LP64, which supports Windows and SVR4 ABIs for x86 and RISC-V.
	 */

	typedef signed char        s8;
	typedef unsigned char      u8;
	typedef signed short int   s16;
	typedef unsigned short int u16;
	typedef signed int         s32;
	typedef unsigned int       u32;
	typedef signed long long   s64;
	typedef unsigned long long u64;
	typedef double             f64;
	typedef float              f32;

	typedef s64                addr_t;
	typedef s64                integral_t;
	typedef u64                inst_t;
	typedef u32                label_t;
	typedef u32                opcode_t;

	struct __s128
	{
		union {
			struct { u8 arr[16];      }      b;
		#if _BYTE_ORDER == _LITTLE_ENDIAN
			struct { u64 lo;  s64 hi; }      d;
		#else
			struct { s64 hi;  u64 lo; }      d;
		#endif
		#if defined (__SIZEOF_INT128__)
			signed __int128                  q;
		#endif
		} r;

		__s128() : r{ .b = { {0} } } {}
		__s128(const __s128 &o) : r{ .b = o.r.b } {}

		#if _BYTE_ORDER == _LITTLE_ENDIAN
		__s128(u64 lo) : r{ .d = { .lo = lo, .hi = s64(~((lo >> 63) - 1)) } } {}
		__s128(s64 hi, u64 lo) : r{ .d = { .lo = lo, .hi = hi } } {}
		#else
		__s128(u64 lo) : r{ .d = { .hi = s64(~((lo >> 63) - 1)), .lo = lo } } {}
		__s128(s64 hi, u64 lo) : r{ .d = { .hi = hi, .lo = lo } } {}
		#endif
	};

	struct __u128
	{
		union {
			struct { u8 arr[16];      }      b;
		#if _BYTE_ORDER == _LITTLE_ENDIAN
			struct { u64 lo;  u64 hi; }      d;
		#else
			struct { u64 hi;  u64 lo; }      d;
		#endif
		#if defined (__SIZEOF_INT128__)
			unsigned __int128                q;
		#endif
		} r;

		__u128() : r{ .b = { {0} } } {}
		__u128(const __u128 &o) : r{ .b = o.r.b } {}

		#if _BYTE_ORDER == _LITTLE_ENDIAN
		__u128(u64 lo) : r{ .d = { .lo = lo, .hi = u64(~((lo >> 63) - 1)) } } {}
		__u128(u64 hi, u64 lo) : r{ .d = { .lo = lo, .hi = hi } } {}
		#else
		__u128(u64 lo) : r{ .d = { .hi = u64(~((lo >> 63) - 1)), .lo = lo } } {}
		__u128(u64 hi, u64 lo) : r{ .d = { .hi = hi, .lo = lo } } {}
		#endif
	};

	typedef __s128             s128;
	typedef __u128             u128;

	/*
	 * Width-typed immediate template aliases
	 */

	template <int W> struct offset_t;
	template <int W> struct ptr_t;
	template <int W> struct simm_t;
	template <int W> struct uimm_t;

	using offset32 = offset_t<32>;
	using offset21 = offset_t<21>;
	using offset13 = offset_t<13>;
	using offset12 = offset_t<12>;
	using ptr64 = ptr_t<64>;
	using ptr32 = ptr_t<32>;
	using simm32 = simm_t<32>;
	using uimm32 = uimm_t<32>;
	using simm20 = simm_t<20>;
	using uimm20 = uimm_t<20>;
	using simm12 = simm_t<12>;
	using uimm12 = uimm_t<12>;
	using uimm7 = uimm_t<7>;
	using uimm6 = uimm_t<6>;
	using uimm5 = uimm_t<5>;
	using ireg5 = uimm_t<5>;
	using freg5 = uimm_t<5>;
	using arg4 = uimm_t<4>;
	using arg3 = uimm_t<3>;
	using arg1 = uimm_t<1>;

	/*
	 * Width-typed immediate template definitions
	 */

	template <int W> struct offset_t
	{
		enum { width = W };
		enum : intptr_t { min = -(1LL<<(W-1)), max = (1LL<<(W-1))-1 };
		enum : bool { is_signed = true, is_integral = false, is_offset = true, is_pointer = false };
		typedef intptr_t value_type;
		intptr_t imm;
		offset_t(intptr_t imm) : imm(imm) {}
		bool valid() { return imm <= max && imm >= min; }
		operator intptr_t() const { return imm; }
	};

	template <int W> struct ptr_t
	{
		enum { width = W };
		enum : uintptr_t { min = 0, max = W == 64 ? ~0 : (1ULL<< W)-1 };
		enum : bool { is_signed = false, is_integral = true, is_offset = false, is_pointer = true };
		typedef uintptr_t value_type;
		uintptr_t imm;
		ptr_t(uintptr_t imm) : imm(imm) {}
		bool valid() { return imm <= max; }
		operator uintptr_t() const { return imm; }
	};

	template <int W> struct simm_t
	{
		enum { width = W };
		enum : s64 { min = -(1LL<<(W-1)), max = (1LL<<(W-1))-1 };
		enum : bool { is_signed = true, is_integral = true, is_offset = false, is_pointer = false };
		typedef s64 value_type;
		s64 imm;
		simm_t(s64 imm) : imm(imm) {}
		bool valid() { return imm <= max && imm >= min; }
		operator s64() const { return imm; }
	};

	template <int W> struct uimm_t
	{
		enum { width = W };
		enum : u64 { min = 0, max = (1ULL<< W)-1 };
		enum : bool { is_signed = false, is_integral = true, is_offset = false, is_pointer = false };
		typedef u64 value_type;
		u64 imm;
		uimm_t(u64 imm) : imm(imm) {}
		bool valid() { return imm <= max; }
		operator u64() const { return imm; }
	};

	/*
	 * Width-typed immediate constructor wrappers
	 *
	 * (template constructors cannot take template parameters)
	 *
	 * simm<N>(u64 imm)
	 * uimm<N>(u64 imm)
	 */

	template <int W> constexpr simm_t<W> simm(const u64 imm) { return simm_t<W>(imm); }
	template <int W> constexpr uimm_t<W> uimm(const u64 imm) { return uimm_t<W>(imm); }

	/* Sign extension template */

	template <typename T, unsigned B>
	inline T sign_extend(const T x)
	{
		struct { T x:B; } s;
		return s.x = x;
	}

	/*
	 * Bit range template
	 *
	 * N	=   decoded MSB offset
	 * M	=   decoded LSB offset
	 */

	template<int N, int M = N>
	struct B
	{
		enum { n = N };
		enum { m = M };
		enum { width = N - M + 1 };

		static_assert(N >= M, "N ≥ M");
	};

	/*
	 * Immediate bit range segment
	 *
	 * K	=   instruction MSB offset
	 * L	=   instruction LSB offset
	 * Args =   decoded bit offsets i.e. B<N:M>, ...
	 */

	template<int K, int L, typename... Args>
	struct S;

	template<int K, int L>
	struct S<K,L>
	{
		enum { offset = 0 };

		static_assert((L > 0), "L > 0");
		static_assert((K < sizeof(1ULL) << 3), "K < sizeof(1ULL) << 3");

		static inline constexpr u64 decode(u64 inst) { return 0; }
		static inline constexpr u64 encode(u64 imm) { return 0; }
	};

	template<int K, int L, typename H, typename... T>
	struct S<K,L,H,T...> : S<K,L,T...>
	{
		typedef S<K,L,T...> I;

		static_assert((L > 0), "L > 0");
		static_assert((K < sizeof(1ULL) << 3), "K < sizeof(1ULL) << 3");

		enum { offset = I::offset + H::width };
		enum { shift = offset + L - H::width - H::m };

		static inline constexpr u64 decode(u64 inst) {
			const u64 mask = ((u64(1) << (H::n + 1)) - 1) ^ ((u64(1) << H::m) - 1);
			return ((shift < 0 ? inst << -shift : inst >> shift) & mask) | I::decode(inst);
		}

		static inline constexpr u64 encode(u64 imm) {
			const u64 mask = ((u64(1) << (H::n + 1)) - 1) ^ ((u64(1) << H::m) - 1);
			return ((shift < 0 ? (imm & mask) >> -shift : (imm & mask) << shift)) | I::encode(imm);
		}
	};

	/*
	 * Immediate bit range notation template
	 *
	 * W	=   number of bits for sign extension
	 * Args =   bit range segments i.e. S<K,L, B<N:M>, ...>, ...
	 */

	template<typename R, int W, typename... Args>
	struct imm_operand_impl_t;

	template<typename R, int W>
	struct imm_operand_impl_t<R,W>
	{
		static inline constexpr R decode(u64 inst) { return 0; }
		static inline constexpr R encode(u64 imm) { return 0; }
	};

	template<typename R, int W, typename H, typename... T>
	struct imm_operand_impl_t<R,W,H,T...> : imm_operand_impl_t<R,W,T...>
	{
		typedef imm_operand_impl_t<R,W,T...> I;

		static inline constexpr R decode(u64 inst) { return I::decode(inst) | H::decode(inst); }
		static inline constexpr R encode(u64 imm) { return I::encode(imm) | H::encode(imm); }
	};

	template<int W, typename... Args>
	struct simm_operand_t : imm_operand_impl_t<s64,W,Args...>
	{
		typedef imm_operand_impl_t<s64,W,Args...> I;

		static constexpr s64 decode(u64 inst) { return sign_extend<s64,W>(I::decode(inst)); }
		static constexpr s64 encode(u64 imm) { return I::encode(imm); }
	};

	template<int W, typename... Args>
	struct uimm_operand_t : imm_operand_impl_t<u64,W,Args...>
	{
		typedef imm_operand_impl_t<u64,W,Args...> I;

		static constexpr u64 decode(u64 inst) { return I::decode(inst); }
		static constexpr u64 encode(u64 imm) { return I::encode(imm); }
	};

}

#endif
