/* Copyright (c) 2020 PCSX2 Dev Team
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of the copyright owner nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#include "xbyak.h"

// If your IDE autocomplete breaks with all the templating, uncomment this to unbreak it
#define XBYAK_ONE_TRUE_TARGET     Xbyak::Targets::X86
#define XBYAK_ONE_TRUE_TARGET_VEC Xbyak::Targets::AVX

namespace Xbyak
{

	namespace Targets
	{
		struct SSE2
		{
			constexpr static bool hasSSE3  = false;
			constexpr static bool hasSSE41 = false;
			constexpr static bool hasAVX   = false;
			constexpr static bool hasAVX2  = false;
		};
		struct SSE3
		{
			constexpr static bool hasSSE3  = true;
			constexpr static bool hasSSE41 = false;
			constexpr static bool hasAVX   = false;
			constexpr static bool hasAVX2  = false;
		};
		struct SSE41
		{
			constexpr static bool hasSSE3  = true;
			constexpr static bool hasSSE41 = true;
			constexpr static bool hasAVX   = false;
			constexpr static bool hasAVX2  = false;
		};
		struct AVX
		{
			constexpr static bool hasSSE3  = true;
			constexpr static bool hasSSE41 = true;
			constexpr static bool hasAVX   = true;
			constexpr static bool hasAVX2  = false;
		};
		struct AVX2
		{
			constexpr static bool hasSSE3  = true;
			constexpr static bool hasSSE41 = true;
			constexpr static bool hasAVX   = true;
			constexpr static bool hasAVX2  = true;
		};
		struct X86
		{
			constexpr static bool is32 = true;
			constexpr static bool is64 = false;
			using AddressReg = Reg32;
			using RipType = int;

			template <typename T32, typename T64>
			struct Choose3264 { using type = T32; };

			template <typename T32, typename T64>
			static T32 choose3264(T32 t32, T64 t64) { return t32; }
		};
#ifdef XBYAK64
		struct X64
		{
			constexpr static bool is32 = false;
			constexpr static bool is64 = true;
			using AddressReg = Reg64;
			using RipType = RegRip;

			template <typename T32, typename T64>
			struct Choose3264 { using type = T64; };

			template <typename T32, typename T64>
			static T64 choose3264(T32 t32, T64 t64) { return t64; }
		};
#endif
	}

#ifndef XBYAK_ONE_TRUE_TARGET
	template <typename Target, typename TargetVec>
#endif
	class SmartCodeGenerator
	{
#ifdef XBYAK_ONE_TRUE_TARGET
		using Target = XBYAK_ONE_TRUE_TARGET;
		using TargetVec = XBYAK_ONE_TRUE_TARGET_VEC;
#endif
		/// Make sure the register is okay to use
		void validateRegister(const Operand& op)
		{
			if (Target::is64)
				return;
			if (op.isREG() && (op.isExtIdx() || op.isExt8bit()))
				throw Error(ERR_64_BIT_REG_IN_32);
			if (op.isMEM() && static_cast<const Address&>(op).getRex() != 0)
				throw Error(ERR_64_BIT_REG_IN_32);
		}
		/// For easier macro-ing
		void validateRegister(int imm)
		{
		}

		void require64()
		{
			if (!Target::is64)
				throw Error(ERR_64_INSTR_IN_32);
		}
	public:
		using AddressReg = typename Target::AddressReg;

		CodeGenerator& actual;

		const Xmm xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15;
		const Ymm ymm0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6, ymm7, ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15;
		const AddressReg rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8,  r9,  r10,  r11,  r12,  r13,  r14,  r15;
		const Reg32      eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d;
		const Reg8        al,  cl,  dl,  bl;

		constexpr static Target::RipType rip{};
		constexpr static AddressFrame ptr{0}, byte{8}, word{16}, dword{32}, qword{64}, xword{128}, yword{256}, zword{512};

		SmartCodeGenerator(CodeGenerator* actual)
			: actual(*actual)
			, xmm0(0), xmm1(1), xmm2(2), xmm3(3), xmm4(4), xmm5(5), xmm6(6), xmm7(7), xmm8(8), xmm9(9), xmm10(10), xmm11(11), xmm12(12), xmm13(13), xmm14(14), xmm15(15)
			, ymm0(0), ymm1(1), ymm2(2), ymm3(3), ymm4(4), ymm5(5), ymm6(6), ymm7(7), ymm8(8), ymm9(9), ymm10(10), ymm11(11), ymm12(12), ymm13(13), ymm14(14), ymm15(15)
			, rax(Operand::RAX), rcx(Operand::RCX), rdx(Operand::RDX), rbx(Operand::RBX), rsp(Operand::RSP), rbp(Operand::RBP), rsi(Operand::RSI), rdi(Operand::RDI), r8(8), r9(9), r10(10), r11(11), r12(12), r13(13), r14(14), r15(15)
			, eax(Operand::EAX), ecx(Operand::ECX), edx(Operand::EDX), ebx(Operand::EBX), esp(Operand::ESP), ebp(Operand::EBP), esi(Operand::ESI), edi(Operand::EDI), r8d(8), r9d(9), r10d(10), r11d(11), r12d(12), r13d(13), r14d(14), r15d(15)
			, al(Operand::AL), cl(Operand::CL), dl(Operand::DL), bl(Operand::BL)
		{
		}

		void db(int code) { actual.db(code); }


		// FORWARD: Forward things to
		// SFORWARD: Forward vector ops to the actual generator, for ops that are the same in SSE and AVX (automatically adds the v prefix for AVX)
		// AFORWARD: Forward vector ops to the actual generator, for ops that have SSE and AVX (extra destination register) versions

#define ACTUAL_FORWARD_BASE(name, ...) \
	actual.name(__VA_ARGS__);

#define ACTUAL_FORWARD_SSE(name, ...) \
	if (TargetVec::hasAVX) \
		actual.v##name(__VA_ARGS__); \
	else \
		actual.name(__VA_ARGS__); \

#define ACTUAL_FORWARD_SSEONLY(name, ...) \
	if (TargetVec::hasAVX) \
		throw Error(ERR_SSE_INSTR_IN_AVX); \
	else \
		actual.name(__VA_ARGS__); \

#define ACTUAL_FORWARD_AVX(name, ...) \
	if (TargetVec::hasAVX) \
		actual.name(__VA_ARGS__); \
	else \
		throw Error(ERR_AVX_INSTR_IN_SSE); \

#define FORWARD2(category, name, type1, type2) \
	void name(type1 a, type2 b) \
	{ \
		validateRegister(a); \
		validateRegister(b); \
		ACTUAL_FORWARD_##category(name, a, b) \
	}

#define FORWARD3(category, name, type1, type2, type3) \
	void name(type1 a, type2 b, type3 c) \
	{ \
		validateRegister(a); \
		validateRegister(b); \
		validateRegister(c); \
		ACTUAL_FORWARD_##category(name, a, b, c) \
	}

#define FORWARD4(category, name, type1, type2, type3, type4) \
	void name(type1 a, type2 b, type3 c, type4 d) \
	{ \
		validateRegister(a); \
		validateRegister(b); \
		validateRegister(c); \
		validateRegister(d); \
		ACTUAL_FORWARD_##category(name, a, b, c, d) \
	}

#define FORWARD_(argcount, ...) FORWARD##argcount(__VA_ARGS__)
// Gets the macro evaluator to evaluate in the right order
#define FORWARD(...) FORWARD_(__VA_ARGS__)

#define FORWARD_JUMP(name) \
		void name(const void *addr) { actual.name(addr); } \
		void name(const Label& label, CodeGenerator::LabelType type = CodeGenerator::T_AUTO) { actual.name(label, type); } \
		void name(const char *label, CodeGenerator::LabelType type = CodeGenerator::T_AUTO) { actual.name(label, type); }

#define ADD_ONE_2 3
#define ADD_ONE_3 4

#define SFORWARD(argcount, name, ...) FORWARD(argcount, SSE, name, __VA_ARGS__)
#define AFORWARD_(argcount, name, arg1, ...)\
	SFORWARD(argcount, name, arg1, __VA_ARGS__)\
	FORWARD(ADD_ONE_##argcount, AVX, v##name, arg1, arg1, __VA_ARGS__)
// Gets the macro evaluator to evaluate in the right order
#define AFORWARD(...) AFORWARD_(__VA_ARGS__)

#define FORWARD_OO_OI(name) \
	FORWARD(2, BASE, name, ARGS_OO) \
	FORWARD(2, BASE, name, ARGS_OI)

#define ARGS_OI const Operand&, uint32
#define ARGS_OO const Operand&, const Operand&
#define ARGS_XI const Xmm&, int
#define ARGS_XO const Xmm&, const Operand&
#define ARGS_XOI const Xmm&, const Operand&, uint8
#define ARGS_XXOX const Xmm&, const Xmm&, const Operand&, const Xmm&
#define ARGS_YOI const Ymm&, const Operand&, uint8

#ifdef XBYAK64
# define REQUIRE64(action) require64(); action
#else
# define REQUIRE64(action) require64()
#endif

		void cdqe() { REQUIRE64(actual.cdqe()); }

		FORWARD_OO_OI(add)
		FORWARD_OO_OI(and)
		FORWARD_OO_OI(cmp)
		FORWARD_OO_OI(or)
		FORWARD_OO_OI(sub)
		FORWARD_OO_OI(xor)
		FORWARD(2, BASE, lea, const Reg&, const Address&)
		FORWARD(2, BASE, mov, const Operand&, size_t)
		FORWARD(2, BASE, mov, ARGS_OO)
		FORWARD(2, BASE, sar, const Operand&, const Reg8&)
		FORWARD(2, BASE, sar, const Operand&, int)
		FORWARD(2, BASE, shl, const Operand&, const Reg8&)
		FORWARD(2, BASE, shl, const Operand&, int)
		FORWARD(2, BASE, shr, const Operand&, const Reg8&)
		FORWARD(2, BASE, shr, const Operand&, int)

		FORWARD_JUMP(je)

		AFORWARD(2, addps,     ARGS_XO)
		SFORWARD(2, cvttps2dq, ARGS_XO)
		SFORWARD(2, movaps,    ARGS_XO)
		SFORWARD(2, movaps,    const Address&, const Xmm&)
		SFORWARD(2, movdqa,    ARGS_XO)
		SFORWARD(2, movdqa,    const Address&, const Xmm&)
		SFORWARD(2, movhps,    ARGS_XO)
		SFORWARD(2, movhps,    const Address&, const Xmm&)
		SFORWARD(2, movd,      const Xmm&, const Reg32&)
		SFORWARD(2, movd,      const Reg32&, const Xmm&)
		SFORWARD(2, movq,      const Xmm&, const Address&)
		SFORWARD(2, movq,      const Address&, const Xmm&)
		AFORWARD(2, mulps,     ARGS_XO)
		AFORWARD(2, packssdw,  ARGS_XO)
		AFORWARD(2, packuswb,  ARGS_XO)
		AFORWARD(2, paddd,     ARGS_XO)
		AFORWARD(2, paddw,     ARGS_XO)
		AFORWARD(2, pand,      ARGS_XO)
		AFORWARD(2, pandn,     ARGS_XO)
		AFORWARD(3, pblendw,   ARGS_XOI)
		AFORWARD(2, pcmpeqd,   ARGS_XO)
		AFORWARD(2, pcmpgtd,   ARGS_XO)
		AFORWARD(2, pextrd,    ARGS_XOI)
		AFORWARD(2, pmaxsw,    ARGS_XO)
		AFORWARD(2, pminsd,    ARGS_XO)
		SFORWARD(2, pmovmskb,  const Reg32e&, const Xmm&)
		SFORWARD(2, pmovzxbw,  ARGS_XO)
		AFORWARD(2, pmulhrsw,  ARGS_XO)
		AFORWARD(2, pmulhw,    ARGS_XO)
		AFORWARD(2, pmullw,    ARGS_XO)
		AFORWARD(2, por,       ARGS_XO)
		SFORWARD(3, pshufd,    ARGS_XOI)
		SFORWARD(3, pshufhw,   ARGS_XOI)
		SFORWARD(3, pshuflw,   ARGS_XOI)
		AFORWARD(2, pslld,     ARGS_XI)
		AFORWARD(2, psllw,     ARGS_XI)
		AFORWARD(2, psrad,     ARGS_XI)
		AFORWARD(2, psraw,     ARGS_XI)
		AFORWARD(2, psrld,     ARGS_XI)
		AFORWARD(2, psrlw,     ARGS_XI)
		AFORWARD(2, psubd,     ARGS_XO)
		AFORWARD(2, psubw,     ARGS_XO)
		AFORWARD(2, punpcklbw, ARGS_XO)
		AFORWARD(2, punpckldq, ARGS_XO)
		AFORWARD(2, punpckhwd, ARGS_XO)
		AFORWARD(2, punpcklwd, ARGS_XO)
		AFORWARD(2, pxor,      ARGS_XO)
		SFORWARD(2, rcpps,     ARGS_XO)
		AFORWARD(3, shufps,    ARGS_XOI)

		FORWARD(2, AVX,    vbroadcastss, ARGS_XO)
		FORWARD(2, SSEONLY, pblendvb, ARGS_XO)
		FORWARD(4, AVX,    vpblendvb, ARGS_XXOX)
		FORWARD(3, AVX,    vpermq,    ARGS_YOI)

#undef REQUIRE64
#undef ARGS_OI
#undef ARGS_OO
#undef ARGS_XI
#undef ARGS_XO
#undef ARGS_XOI
#undef ARGS_XXOX
#undef ARGS_YOI
#undef FORWARD_OO_OI
#undef AFORWARD
#undef AFORWARD_
#undef SFORWARD
#undef ADD_ONE_2
#undef ADD_ONE_3
#undef FORWARD_JUMP
#undef FORWARD
#undef FORWARD_
#undef FORWARD4
#undef FORWARD3
#undef FORWARD2
#undef FORWARD0
#undef ACTUAL_FORWARD_AVX
#undef ACTUAL_FORWARD_SSE
#undef ACTUAL_FORWARD_BASE
	};

}
