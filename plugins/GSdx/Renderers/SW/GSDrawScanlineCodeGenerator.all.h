/*
 *	Copyright (C) 2007-2009 Gabest
 *	Copyright (C) 2020 PCSX2 Dev Team
 *	http://www.gabest.org
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#pragma once

#include "GSScanlineEnvironment.h"
#include "Renderers/Common/GSFunctionMap.h"
#include "GSUtil.h"
#include "GSVertexSW.h"
#include "xbyak/xbyak_smart.h"

#undef a0
#undef a1
#undef a2
#undef a3
#undef t0
#undef t1

// Ease the reading of the code
// Note, there are versions without the _64 prefix that can be used as source (but not destination) operands on both 32 and 64 bit
#define _64_g_const r10
#define _64_m_local r12
#define _64_m_local__gd r13
#define _64_m_local__gd__vm a1
#define _64_m_local__gd__clut r11
// If m_sel.mmin, m_local.gd->tex, else m_local.gd->tex[0]
#define _64_m_local__gd__tex r14

// Careful with rip-based regexps, you can only add constants to them, not registers
#define _rip_local_regexp(field) ((is32 || m_rip) ? (RegExp)(rip + (size_t)&m_local.field) : (RegExp)(_m_local + offsetof(GSScanlineLocalData, field)))
#define _rip_global_regexp(field) ((is32 || m_rip) ? (RegExp)(rip + (size_t)&m_local.gd->field) : (RegExp)(_m_local__gd + offsetof(GSScanlineGlobalData, field)))
#define _rip_local(field) ((is32 || m_rip) ? ptr[rip + (size_t)&m_local.field] : ptr[_m_local + offsetof(GSScanlineLocalData, field)])
#define _rip_global(field) ((is32 || m_rip) ? ptr[rip + (size_t)&m_local.gd->field] : ptr[_m_local__gd + offsetof(GSScanlineGlobalData, field)])

/// Executes the given code only if targeting 32-bit
#define ONLY32(code) if (is32) (code)
/// Executes the given code only if targeting 64-bit
#define ONLY64(code) if (is64) (code)
/// Combines temporary with either dst64 on 64-bit or src32 on 32-bit
/// Follow up with an ONLY32 save back to src32
#define COMBINE(operation, dst64, temporary, src32) \
	if (is32) \
		operation(temporary, src32); \
	else \
		operation(dst64, temporary)
/// On AVX, does a v-prefixed separate destination operation
/// On SSE, moves src1 into dst using movdqa, then does the operation
#define THREEARG(operation, dst, src1, ...) \
	if (hasAVX) \
	{ \
		v##operation(dst, src1, __VA_ARGS__); \
	} \
	else \
	{ \
		movdqa(dst, src1); \
		operation(dst, __VA_ARGS__);\
	}
/// On x64, does a 3-operand move, on x86 uses a two-operand SSE-style
#define MOVE_IF_64(operation, dst, src64, ...) \
	if (is64) \
	{ \
		THREEARG(operation, dst, src64, __VA_ARGS__); \
	} \
	else \
	{ \
		operation(dst, __VA_ARGS__); \
	}

#if _M_SSE >= 0x501
# define VECTOR_REGISTER Ymm
# define USING_XMM 0
# define USING_YMM 1
/// On AVX2, uses the given broadcast to load from memory, otherwise uses the load
# define BROADCAST_OR_LOAD(broadcast, load, dst, src) \
	broadcast(dst, src)
/// On AVX2, uses the given broadcast to load into the temp register, then applies the given op
/// Otherwise, applies the given op directly
# define BROADCAST_AND_OP(broacast, op, dst, tmpReg, src) \
	do \
	{ \
		broadcast(tmpReg, src); \
		op(dst, tmpReg); \
	} while (0)
# define BROADCAST_GPR_TO_VEC(vec, gpr) \
	do \
	{ \
		movd(Xmm(vec.getIdx()), gpr); \
		vpbroadcastd(veclarge, Xmm(vec.getIdx())); \
	} while (0)
# define _rip_local_d(x) _rip_local(d8.x)
# define _rip_local_d_p(x) _rip_local_d(p.x)
#else
# define VECTOR_REGISTER Xmm
# define USING_XMM 1
# define USING_YMM 0
/// On AVX2, uses the given broadcast to load from memory, otherwise uses a the load
# define BROADCAST_OR_LOAD(broadcast, load, dst, src) \
	load(dst, src)
/// On AVX2, uses the given broadcast to load into the temp register, then applies the given op
/// Otherwise, applies the given op directly
# define BROADCAST_AND_OP(broacast, op, dst, tmpReg, src) \
	op(dst, src)
# define BROADCAST_GPR_TO_VEC(vec, gpr) \
	do \
	{ \
		movd(vec, gpr); \
		pshufd(vec, vec, _MM_SHUFFLE(0, 0, 0, 0)); \
	} while (0)
# define _rip_local_d(x) _rip_local(d4.x)
# define _rip_local_d_p(x) _rip_local_d(x)
#endif

enum class RegsUsed {
	A0, A1, A2, A3, T0, T1
};

using namespace Xbyak;

class GSDrawScanlineCodeGenerator2 : public Xbyak::SmartCodeGenerator
{
	using _parent = Xbyak::SmartCodeGenerator;
	using XYm = VECTOR_REGISTER;

	/// On x86-64 we reserve a bunch of GPRs for holding addresses of locals that would otherwise be hard to reach
	/// On x86-32 the same values are just raw 32-bit addresses
	using LocalAddr = Choose3264<size_t, AddressReg>::type;

	constexpr static bool isXmm = std::is_same<XYm, Xmm>::value;
	constexpr static bool isYmm = std::is_same<XYm, Ymm>::value;
	constexpr static int wordsize = is64 ? 8 : 4;
	constexpr static int vecsize = isXmm ? 16 : 32;
	constexpr static int vecsizelog = isXmm ? 4 : 5;
	constexpr static int vecints = vecsize / 4;


// MARK: - Constants

	constexpr static int _32_args = 16;
	constexpr static int _invalid = 0xaaaaaaaa;
#ifdef _WIN32
	// Windows has no redzone and also has 10 xmm registers to save
	constexpr static int _64_win_stack_size = 8 * 4 + 16 * 10;
	// XMM registers will be saved to `rsp + _64_win_xmm_start + id - 6`
	// Which will put xmm6 after the temporaries, them xmm7, etc
	constexpr static int _64_win_xmm_start = 8 * 4;
	constexpr static int _64_top = 8 * 3;
	constexpr static int _64_zs  = 8 * 2;
	constexpr static int _64_zd  = 8 * 1;
	constexpr static int _64_cov = 8 * 0;
#else
	// System-V has a redzone so stick everything there
	constexpr static int _64_rz_rbx = -8 * 1;
	constexpr static int _64_rz_r12 = -8 * 2;
	constexpr static int _64_rz_r13 = -8 * 3;
	constexpr static int _64_rz_r14 = -8 * 4;
	constexpr static int _64_rz_r15 = -8 * 5;
	constexpr static int _64_top    = -8 * 6;
	constexpr static int _64_zs     = -8 * 8;
	constexpr static int _64_zd     = -8 * 10;
	constexpr static int _64_cov    = -8 * 12;
#endif
	constexpr static int _top = is64 ? _64_top  : _32_args + 4;
	constexpr static int _v   = is64 ? _invalid : _32_args + 8;

	GSScanlineSelector m_sel;
	GSScanlineLocalData& m_local;
	bool m_rip;

	const XYm xym0, xym1, xym2, xym3, xym4, xym5, xym6, xym7, xym8, xym9, xym10, xym11, xym12, xym13, xym14, xym15;
	/// Note: a2 is only available on x86-64
	const AddressReg a0, a1, a2, a3, t0, t1;
	const LocalAddr _g_const, _m_local, _m_local__gd, _m_local__gd__vm;
	/// Available on both x86 and x64, not always valid
	const XYm _rb, _ga, _fm, _zm, _fd, _test;
	/// Always valid if needed, x64 only
	const XYm _z, _f, _s, _t, _q, _f_rb, _f_ga;

	/// Marks a register as unavailable in x86-32 by remapping it to a high register
	static int block32(int regID)
	{
		return is64 ? regID : regID | 8;
	}

	/// Returns the first arg on 32-bit, second on 64-bit
	static LocalAddr chooseLocal(const void *addr32, AddressReg reg64)
	{
		return choose3264((size_t)addr32, reg64);
	}

public:
	GSDrawScanlineCodeGenerator2(Xbyak::CodeGenerator* base, SSEVersion::SSEVersion sseVersion, bool hasFMA, void* param, uint64 key)
		: _parent(base, sseVersion, hasFMA)
		, m_local(*(GSScanlineLocalData*)param)
		, m_rip(false)
		, xym0(0), xym1(1), xym2(2), xym3(3), xym4(4), xym5(5), xym6(6), xym7(7), xym8(8), xym9(9), xym10(10), xym11(11), xym12(12), xym13(13), xym14(14), xym15(15)
#ifdef _WIN32
		, a0(rcx) , a1(rdx)
		, a2(r8)  , a3(is64 ? r9 : rbx)
		, t0(rdi) , t1(rsi)
#else
		, a0(is64 ? rdi : rcx), a1(is64 ? rsi : rdx)
		, a2(is64 ? rdx : r8),  a3(is64 ? rcx : rbx)
		, t0(is64 ? r8  : rdi), t1(is64 ? r9  : rsi)
#endif
		, _g_const(chooseLocal(&*g_const, _64_g_const))
		, _m_local(chooseLocal(&m_local, _64_m_local))
		, _m_local__gd(chooseLocal(m_local.gd, _64_m_local__gd))
		, _m_local__gd__vm(chooseLocal(m_local.gd->vm, _64_m_local__gd__vm))
		, _rb(is64 ? xym2 : xym5), _ga(is64 ? xym3 : xym6), _fm(is64 ? xym4 : xym3), _zm(is64 ? xym5 : xym4), _fd(is64 ? xym6 : xym2), _test(is64 ? xym15 : xym7)
		, _z(xym8), _f(xym9), _s(xym10), _t(xym11), _q(xym12), _f_rb(xym13), _f_ga(xym14)
	{
		m_sel.key = key;
		if (isYmm)
			ASSERT(hasAVX2);
	}

private:

// MARK: - Helpers

	/// Loads the given address into the given register if needed, and returns something that can be used in a `ptr[]`
	LocalAddr loadAddress(AddressReg reg, const void *addr)
	{
		if (is64)
			mov(reg, (size_t)addr);
		return choose3264((size_t)addr, reg);
	}

	void modulate16(const XYm& a, const Operand& f, uint8 shift)
	{
		if (shift == 0 && hasSSE3)
		{
			pmulhrsw(a, f);
		}
		else
		{
			psllw(a, shift + 1);
			pmulhw(a, f);
		}
	}

	void lerp16(const XYm& a, const XYm& b, const XYm& f, uint8 shift)
	{
		psubw(a, b);
		modulate16(a, f, shift);
		paddw(a, b);
	}

	void lerp16_4(const XYm& a, const XYm& b, const XYm& f)
	{
		psubw(a, b);
		pmullw(a, f);
		psraw(a, 4);
		paddw(a, b);
	}

	void mix16(const XYm& a, const XYm& b, const XYm& temp)
	{
		if(hasSSE41)
		{
			pblendw(a, b, 0xaa);
		}
		else
		{
			pcmpeqd(temp, temp);
			psrld(temp, 16);
			pand(a, temp);
			pandn(temp, b);
			por(a, temp);
		}
	}

	void clamp16(const XYm& a, const XYm& temp)
	{
		if (hasSSE41 && isXmm)
		{
			packuswb(a, a);
			pmovzxbw(a, a);
		}
		else
		{
			packuswb(a, a);
			pxor(temp, temp);
			punpcklbw(a, temp);
		}
	}

	void alltrue(const XYm& test)
	{
		uint32 mask = test.isYMM() ? 0xffffffff : 0xffff;
		pmovmskb(eax, test);
		cmp(eax, mask);
		je("step", GSCodeGenerator::T_NEAR);
	}

	void blend(const XYm& a, const XYm& b, const XYm& mask)
	{
		pand(b, mask);
		pandn(mask, a);
		if (hasAVX)
		{
			vpor(a, b, mask);
		}
		else
		{
			por(b, mask);
			movdqa(a, b);
		}
	}

	void blendr(const XYm& b, const XYm& a, const XYm& mask)
	{
		pand(b, mask);
		pandn(mask, a);
		por(b, mask);
	}

	void blend8(const XYm& a, const XYm& b)
	{
		if (hasSSE41)
			pblendvb(a, b /*, xym0 */);
		else
			blend(a, b, xym0);
	}

	void blend8r(const XYm& b, const XYm& a)
	{
		if (hasAVX)
		{
			vpblendvb(b, a, b, xym0);
		}
		else if (hasSSE41)
		{
			pblendvb(a, b);
			movdqa(b, a);
		}
		else
		{
			blendr(b, a, xym0);
		}
	}

	void split16_2x8(const XYm& l, const XYm& h, const XYm& src)
	{
		// l = src & 0xFF; (1 left shift + 1 right shift)
		// h = (src >> 8) & 0xFF; (1 right shift)

		if (hasAVX)
		{
			if (src == h) {
				vpsllw(l, src, 8);
				psrlw(h, 8);
			} else if (src == l) {
				vpsrlw(h, src, 8);
				psllw(l, 8);
			} else {
				vpsllw(l, src, 8);
				vpsrlw(h, src, 8);
			}
			psrlw(l, 8);
		}
		else
		{
			if (src == h) {
				movdqa(l, src);
			} else if (src == l) {
				movdqa(h, src);
			} else {
				movdqa(l, src);
				movdqa(h, src);
			}
			psllw(l, 8);
			psrlw(l, 8);
			psrlw(h, 8);
		}
	}

// MARK: - Main Implementation
public:
	void Generate()
	{
		bool need_tex = m_sel.fb && m_sel.tfx != TFX_NONE;
		bool need_clut = need_tex && m_sel.tlu;
		m_rip = (size_t)getCurr() < 0x80000000;
		m_rip &= (size_t)&m_local < 0x80000000;
		m_rip &= (size_t)&m_local.gd < 0x80000000;

		if (is32)
		{
			push(rbx);
			push(rsi);
			push(rdi);
			push(rbp);
		}
		else
		{
			push(rbp);
#ifdef _WIN32
			push(rbx);
			push(rsi);
			push(rdi);
			push(r12);
			push(r13);
			push(r14);
			push(r15);

			sub(rsp, _64_win_stack_size);

			for (int i = 0; i < 10; i++)
			{
				movdqa(ptr[rsp + _64_win_xmm_start + 16*i], Xmm(i+6));
			}
#else
			mov(ptr[rsp + _64_rz_rbx], rbx);
			if (!m_rip)
			{
				mov(ptr[rsp + _64_rz_r12], r12);
				mov(ptr[rsp + _64_rz_r13], r13);
			}
			mov(ptr[rsp + _64_rz_r14], r14);
			mov(ptr[rsp + _64_rz_r15], r15);
#endif
			mov(_64_g_const, (size_t)&*g_const);
			if (!m_rip)
			{
				mov(_64_m_local, (size_t)&m_local);
				mov(_64_m_local__gd, _rip_local(gd));
			}

			if(need_clut)
				mov(_64_m_local__gd__clut, _rip_global(clut));
		}

		Init();

		if(!m_sel.edge)
		{
			align(16);
		}

	L("loop");

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// xym0 = z/zi      |
		// xym2 = s/u (tme) | free
		// xym3 = t/v (tme) | free
		// xym4 = q (tme)   | free
		// xym5 = rb (!tme) | free
		// xym6 = ga (!tme) | free
		// xym7 = test      | free
		// xym15 =          | test

		bool tme = m_sel.tfx != TFX_NONE;
		
		TestZ(tme ? xym5 : xym2, tme ? xym6 : xym3);

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = s/u (tme) | free
		// xym3 = t/v (tme) | free
		// xym4 = q (tme)   | free
		// xym5 = rb (!tme) | free
		// xym6 = ga (!tme) | free
		// xym7 = test      | free
		// xym15 =          | test

		if(m_sel.mmin)
		{
			SampleTextureLOD();
		}
		else
		{
			SampleTexture();
		}

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = free | rb
		// xym3 = free | ga
		// xym4 = free | free
		// xym5 = rb   | free
		// xym6 = ga   | free
		// xym7 = test | free
		// xym15 =     | test

		AlphaTFX();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = gaf (TFX_HIGHLIGHT || TFX_HIGHLIGHT2 && !tcc) | rb
		// xym3 = free | ga
		// xym4 = free | free
		// xym5 = rb   | free
		// xym6 = ga   | free
		// xym7 = test | free
		// xym15 =     | test

		ReadMask();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = gaf (TFX_HIGHLIGHT || TFX_HIGHLIGHT2 && !tcc) | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | free
		// xym7 = test | free
		// xym15 =     | test

		TestAlpha();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = gaf (TFX_HIGHLIGHT || TFX_HIGHLIGHT2 && !tcc) | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | free
		// xym7 = test | free
		// xym15 =     | test

		ColorTFX();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = free | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | free
		// xym7 = test | free
		// xym15 =     | test

		Fog();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// xym2 = free | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | free
		// xym7 = test | free
		// xym15 =     | test

		ReadFrame();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// ebx = fa
		// xym2 = fd   | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | fd
		// xym7 = test | free
		// xym15 =     | test

		TestDestAlpha();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// ebp = za
		// ebx = fa
		// xym2 = fd   | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | fd
		// xym7 = test | free
		// xym15 =     | test

		WriteMask();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// edx = fzm
		// ebp = za
		// ebx = fa
		// xym2 = fd   | rb
		// xym3 = fm   | ga
		// xym4 = zm   | fm
		// xym5 = rb   | zm
		// xym6 = ga   | fd

		WriteZBuf();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// edx = fzm
		// ebx = fa
		// xym2 = fd   | rb
		// xym3 = fm   | ga
		// xym4 = free | fm
		// xym5 = rb   | free
		// xym6 = ga   | fd

		AlphaBlend();

		// a0 = steps
		// t1 = fza_base
		// t0 = fza_offset
		// edx = fzm
		// ebx = fa
		// xym2 = fd   | rb
		// xym3 = fm   | ga
		// xym4 = free | fm
		// xym5 = rb   | free
		// xym6 = ga   | fd

		WriteFrame();

	L("step");

		// if(steps <= 0) break;

		if (!m_sel.edge)
		{
			test(a0.cvt32(), a0.cvt32());

			jle("exit", CodeGenerator::T_NEAR);

			Step();

			jmp("loop", CodeGenerator::T_NEAR);
		}

	L("exit");



		if (is32)
		{
			pop(ebp);
			pop(edi);
			pop(esi);
			pop(ebx);

			ret(8);
		}
		else
		{
#ifdef _WIN32
			for (int i = 0; i < 10; i++)
			{
				movdqa(ptr[rsp + _64_win_xmm_start + 16*i], Xmm(i+6));
			}
			add(rsp, _64_win_stack_size);

			pop(r15);
			pop(r14);
			pop(r13);
			pop(r12);
			pop(rdi);
			pop(rsi);
			pop(rbx);
#else
			mov(rbx, ptr[rsp + _64_rz_rbx]);
			if (!m_rip)
			{
				mov(r12, ptr[rsp + _64_rz_r12]);
				mov(r13, ptr[rsp + _64_rz_r13]);
			}
			mov(r14, ptr[rsp + _64_rz_r14]);
			mov(r15, ptr[rsp + _64_rz_r15]);
#endif
			pop(rbp);
			ret();
		}
	}

private:

	/// Inputs: a0=pixels, a1=left, a2[x64]=top, a3[x64]=v
	void Init()
	{
		if(!m_sel.notest)
		{
			// int skip = left & 3;

			mov(ebx, a1.cvt32());
			and(a1.cvt32(), vecints-1);

			// left -= skip;

			sub(ebx, a1.cvt32());

			// int steps = pixels + skip - 4;

			lea(a0.cvt32(), ptr[a0 + a1 - vecints]);

			// GSVector4i test = m_test[skip] | m_test[7 + (steps & (steps >> 31))];

			shl(a1.cvt32(), vecsizelog); // * sizeof(m_test[0])

			mov(eax, a0.cvt32());
			sar(eax, 31); // GH: 31 to extract the sign of the register
			and(eax, a0.cvt32());
			if (isXmm)
				shl(eax, 4); // * sizeof(m_test[0])
			ONLY64(cdqe());

			if (isXmm)
			{
				movdqa(_test, ptr[a1 + _g_const + offsetof(GSScanlineConstantData, m_test_128b[0])]);
				por(_test, ptr[rax + _g_const + offsetof(GSScanlineConstantData, m_test_128b[7])]);
			}
			else
			{
				pmovsxbd(_test, ptr[a1*8 + _g_const + offsetof(GSScanlineConstantData, m_test_256b[0])]);
				pmovsxbd(xym0, ptr[a1*8 + _g_const + offsetof(GSScanlineConstantData, m_test_256b[15])]);
				por(_test, xym0);
			}
		}
		else
		{
			mov(ebx, a1.cvt32()); // left
			xor(a1.cvt32(), a1.cvt32()); // skip
			lea(a0.cvt32(), ptr[a0 - vecints]); // steps
		}

		// a0 = steps
		// a1 = skip
		// rbx = left

		if (is64)
		{
			// GSVector2i* fza_base = &m_local.gd->fzbr[top];
			mov(rax, _rip_global(fzbr));
			lea(t1, ptr[rax + a2 * 8]);

			// GSVector2i* fza_offset = &m_local.gd->fzbc[left >> 2];
			mov(rax, _rip_global(fzbc));
			lea(t0, ptr[rax + rbx * 2]);
		}
		else
		{
			// GSVector2i* fza_base = &m_local.gd->fzbr[top];
			mov(rax, ptr[rsp + _top]);
			lea(t1, ptr[rax * 8 + (size_t)&m_local.gd->fzbr]);

			// GSVector2i* fza_offset = &m_local.gd->fzbc[left >> 2];
			lea(t0, ptr[(size_t)&m_local.gd->fzbc + rbx * 2]);
		}

		if(m_sel.prim != GS_SPRITE_CLASS && (m_sel.fwrite && m_sel.fge || m_sel.zb) || m_sel.fb && (m_sel.edge || m_sel.tfx != TFX_NONE || m_sel.iip))
		{
			// a1 = &m_local.d[skip] // note a1 was (skip << 4)

			if (is64)
			{
				lea(rax, _rip_local(d));
				lea(a1, ptr[rax + a1 * 8]);
			}
			else
			{
				lea(a1, ptr[(size_t)m_local.d + a1 * 8]);
				// a3 starts on the stack in x86, we want it in a register
				mov(a3, ptr[rsp + _v]);
			}
		}

		const XYm& f = is64 ? _f : xym1;
		const XYm& z = is64 ? _z : xym0;

		if(m_sel.prim != GS_SPRITE_CLASS)
		{
			if(m_sel.fwrite && m_sel.fge || m_sel.zb)
			{
				BROADCAST_OR_LOAD(vbroadcastf128, movaps, z, ptr[a3 + offsetof(GSVertexSW, p)]); // v.p

				if(m_sel.fwrite && m_sel.fge)
				{
					// f = GSVector4i(vp).zzzzh().zzzz().add16(m_local.d[skip].f);

					cvttps2dq(f, z);
					pshufhw(f, f, _MM_SHUFFLE(2, 2, 2, 2));
					pshufd(f, f, _MM_SHUFFLE(2, 2, 2, 2));
					paddw(f, ptr[a1 + offsetof(GSScanlineLocalData::skip, f)]);

					if (is32) // _f is shared on x86
						movdqa(ptr[&m_local.temp.f], f);
				}

				if(m_sel.zb)
				{
					// z = vp.zzzz() + m_local.d[skip].z;
					shufps(z, z, _MM_SHUFFLE(2, 2, 2, 2));
					ONLY32(movaps(ptr[&m_local.temp.z], z));
					if (is64)
					{
						addps(z, ptr[a1 + offsetof(GSScanlineLocalData::skip, z)]);
					}
					else
					{
						movaps(xym2, ptr[a1 + offsetof(GSScanlineLocalData::skip, z)]);
						movaps(ptr[&m_local.temp.zo], xym2);
						addps(z, xym2);
					}
				}
			}
		}
		else
		{
			if(m_sel.ztest)
			{
				movdqa(z, _rip_local(p.z));
			}

			if(m_sel.fwrite && m_sel.fge && is64)
				movdqa(_f, _rip_local(p.f));
		}

		const XYm& vt = xym4;

		if(m_sel.fb)
		{
			if(m_sel.edge || m_sel.tfx != TFX_NONE)
			{
				BROADCAST_OR_LOAD(vbroadcastf128, movaps, vt, ptr[a3 + offsetof(GSVertexSW, t)]); // v.t
			}

			if(m_sel.edge)
			{
				// m_local.temp.cov = GSVector4i::cast(v.t).zzzzh().wwww().srl16(9);

				pshufhw(xym3, vt, _MM_SHUFFLE(2, 2, 2, 2));
				pshufd(xym3, xym3, _MM_SHUFFLE(3, 3, 3, 3));
				psrlw(xym3, 9);

				movdqa(_rip_local(temp.cov), xym3);
//	#ifdef _WIN64
//				vmovdqa(_rip_local(temp.cov), xym3);
//	#else
//				vmovdqa(ptr[rsp + _rz_cov], xym3);
//	#endif
			}

			if(m_sel.tfx != TFX_NONE)
			{
				// a1 = &m_local.d[skip]

				const XYm& s = is64 ? _s   : xym2;
				const XYm& t = is64 ? _t   : xym3;

				if(m_sel.fst)
				{
					// GSVector4i vti(vt);

					cvttps2dq(xym6, vt);

					// s = vti.xxxx() + m_local.d[skip].s;
					// t = vti.yyyy(); if(!sprite) t += m_local.d[skip].t;

					pshufd(s, xym6, _MM_SHUFFLE(0, 0, 0, 0));
					pshufd(t, xym6, _MM_SHUFFLE(1, 1, 1, 1));

					paddd(s, ptr[a1 + offsetof(GSScanlineLocalData::skip, s)]);

					if(m_sel.prim != GS_SPRITE_CLASS || m_sel.mmin)
					{
						paddd(t, ptr[a1 + offsetof(GSScanlineLocalData::skip, t)]);
					}
					else if(m_sel.ltf)
					{
						XYm vf = is64 ? xym7 : xym6;
						pshuflw(vf, t, _MM_SHUFFLE(2, 2, 0, 0));
						pshufhw(vf, vf, _MM_SHUFFLE(2, 2, 0, 0));
						psrlw(vf, 12);
						movdqa(_rip_local(temp.vf), vf);
					}

					ONLY32(movdqa(_rip_local(temp.s), s));
					ONLY32(movdqa(_rip_local(temp.t), t));
				}
				else
				{
					const XYm& q = is64 ? _q : vt;

					// s = vt.xxxx() + m_local.d[skip].s;
					// t = vt.yyyy() + m_local.d[skip].t;
					// q = vt.zzzz() + m_local.d[skip].q;

					if (hasAVX)
					{
						vshufps(s, vt, vt, _MM_SHUFFLE(0, 0, 0, 0));
						vshufps(t, vt, vt, _MM_SHUFFLE(1, 1, 1, 1));
						vshufps(q, vt, vt, _MM_SHUFFLE(2, 2, 2, 2));
					}
					else
					{
						movaps(s, vt);
						movaps(t, vt);
						ONLY64(movaps(q, vt));

						shufps(s, s, _MM_SHUFFLE(0, 0, 0, 0));
						shufps(t, t, _MM_SHUFFLE(1, 1, 1, 1));
						shufps(q, q, _MM_SHUFFLE(2, 2, 2, 2));
					}

					addps(s, ptr[a1 + offsetof(GSScanlineLocalData::skip, s)]);
					addps(t, ptr[a1 + offsetof(GSScanlineLocalData::skip, t)]);
					addps(q, ptr[a1 + offsetof(GSScanlineLocalData::skip, q)]);

					if (is32)
					{
						movaps(ptr[&m_local.temp.s], s);
						movaps(ptr[&m_local.temp.t], t);
						movaps(ptr[&m_local.temp.q], q);
					}
				}
			}

			if(!(m_sel.tfx == TFX_DECAL && m_sel.tcc))
			{
				const XYm& f_rb = is64 ? _f_rb : xym5;
				const XYm& f_ga = is64 ? _f_ga : xym6;
				if(m_sel.iip)
				{
					// GSVector4i vc = GSVector4i(v.c);

					if (isXmm)
					{
						cvttps2dq(xym6, ptr[a3 + offsetof(GSVertexSW, c)]); // v.c
					}
					else
					{
						vbroadcastf128(ymm6, ptr[a3 + offsetof(GSVertexSW, c)]);
						cvttps2dq(ymm6, ymm6);
					}

					// vc = vc.upl16(vc.zwxy());

					pshufd(xym5, xym6, _MM_SHUFFLE(1, 0, 3, 2));
					punpcklwd(xym6, xym5);

					// rb = vc.xxxx().add16(m_local.d[skip].rb);
					// ga = vc.zzzz().add16(m_local.d[skip].ga);

					pshufd(f_rb, xym6, _MM_SHUFFLE(0, 0, 0, 0));
					pshufd(f_ga, xym6, _MM_SHUFFLE(2, 2, 2, 2));

					paddw(f_rb, ptr[a1 + offsetof(GSScanlineLocalData::skip, rb)]);
					paddw(f_ga, ptr[a1 + offsetof(GSScanlineLocalData::skip, ga)]);

					ONLY32(movdqa(ptr[&m_local.temp.rb], f_rb));
					ONLY32(movdqa(ptr[&m_local.temp.ga], f_ga));
				}
				else if (is64 || m_sel.tfx == TFX_NONE)
				{
					movdqa(f_rb, _rip_local(c.rb));
					movdqa(f_ga, _rip_local(c.ga));
				}

				ONLY64(movdqa(_rb, _f_rb));
				ONLY64(movdqa(_ga, _f_ga));
			}
		}

		if (is64)
		{
			if(m_sel.fwrite && m_sel.fpsm == 2 && m_sel.dthe)
			{
				// On linux, a2 is edx which will be used for fzm
				// In all case, it will require a mov in dthe code, so let's keep the value on the stack
				mov(ptr[rsp + _top], a2);
			}

			mov(_64_m_local__gd__vm, _rip_global(vm));
			if(m_sel.fb && m_sel.tfx != TFX_NONE)
			{
				if (m_sel.mmin)
					lea(_64_m_local__gd__tex, _rip_global(tex));
				else
					mov(_64_m_local__gd__tex, _rip_global(tex));
			}
		}
	}

	/// Inputs: a0=steps, t0=fza_offset
	/// Outputs[x86]: xym0=z xym2=s, xym3=t, xym4=q, xym5=rb, xym6=ga, xym7=test
	/// Destroys[x86]: all
	/// Destroys[x64]: xym0, xym1, xym2, xym3
	void Step()
	{
		// steps -= 4;

		sub(a0.cvt32(), vecints);

		// fza_offset++;

		add(t0, vecsize/2);

		const XYm& z = is64 ? _z : xym0;
		const XYm& f = is64 ? _f : xym1;

		if(m_sel.prim != GS_SPRITE_CLASS)
		{
			// z += m_local.d4.z;

			if(m_sel.zb)
			{
				if (is32)
				{
					BROADCAST_OR_LOAD(vbroadcastss, movaps, z, _rip_local_d_p(z));
					addps(z, _rip_local(temp.zo));
					movaps(_rip_local(temp.zo), z);
					addps(z, _rip_local(temp.z));
				}
				else
				{
					BROADCAST_AND_OP(vbroadcastss, addps, z, xym0, _rip_local_d_p(z));
				}
			}

			// f = f.add16(m_local.d4.f);

			if(m_sel.fwrite && m_sel.fge)
			{
				if (is32)
				{
					BROADCAST_OR_LOAD(vbroadcastw, movdqa, z, _rip_local_d_p(f));
					paddw(f, _rip_local(temp.f));
					movdqa(_rip_local(temp.f), f);
				}
				else
				{
					BROADCAST_AND_OP(vpbroadcastw, paddw, f, xym0, _rip_local_d_p(f));
				}
			}
		}
		else
		{
			if(is32 && m_sel.ztest)
			{
				BROADCAST_OR_LOAD(vpbroadcastd, movdqa, z, _rip_local(p.z));
			}
		}

		if(m_sel.fb)
		{
			if(m_sel.tfx != TFX_NONE)
			{
				if(m_sel.fst)
				{
					const XYm& stq = is64 ? xym0 : xym4;
					// GSVector4i stq = m_local.d4.stq;

					// s += stq.xxxx();
					// if(!sprite) t += st.yyyy();

					BROADCAST_OR_LOAD(vbroadcasti128, movdqa, stq, _rip_local_d(stq));

					XYm s = is64 ? xym1 : xym2;
					pshufd(s, stq, _MM_SHUFFLE(0, 0, 0, 0));
					COMBINE(paddd, _s, s, _rip_local(temp.s));
					ONLY32(movdqa(_rip_local(temp.s), s));

					XYm t = is64 ? xym1 : xym3;
					if(m_sel.prim != GS_SPRITE_CLASS || m_sel.mmin)
					{
						pshufd(t, stq, _MM_SHUFFLE(1, 1, 1, 1));
						COMBINE(paddd, _t, t, _rip_local(temp.t));
						ONLY32(movdqa(_rip_local(temp.t), t));
					}
					else
					{
						ONLY32(movdqa(t, _rip_local(temp.t)));
					}
				}
				else
				{
					const XYm& s = xym2;
					const XYm& t = xym3;
					const XYm& q = is64 ? xym1 : xym4;
					// GSVector4 stq = m_local.d4.stq;

					// s += stq.xxxx();
					// t += stq.yyyy();
					// q += stq.zzzz();

					if (hasAVX)
					{
						BROADCAST_OR_LOAD(vbroadcastf128, movaps, q, _rip_local_d(stq));

						vshufps(s, q, q, _MM_SHUFFLE(0, 0, 0, 0));
						vshufps(t, q, q, _MM_SHUFFLE(1, 1, 1, 1));
						vshufps(q, q, q, _MM_SHUFFLE(2, 2, 2, 2));
					}
					else
					{
						movaps(q, _rip_local(d4.stq));
						movaps(s, q);
						movaps(t, q);

						shufps(s, s, _MM_SHUFFLE(0, 0, 0, 0));
						shufps(t, t, _MM_SHUFFLE(1, 1, 1, 1));
						shufps(q, q, _MM_SHUFFLE(2, 2, 2, 2));
					}

					COMBINE(addps, _s, s, _rip_local(temp.s));
					COMBINE(addps, _t, t, _rip_local(temp.t));
					COMBINE(addps, _q, q, _rip_local(temp.q));

					ONLY32(movaps(_rip_local(temp.s), s));
					ONLY32(movaps(_rip_local(temp.t), t));
					ONLY32(movaps(_rip_local(temp.q), q));
				}
			}

			if(!(m_sel.tfx == TFX_DECAL && m_sel.tcc))
			{
				XYm rb = is64 ? xym1 : xym5, ga = is64 ? xym2 : xym6;
				if(m_sel.iip)
				{
					XYm c = is64 ? xym0 : xym7;
					// GSVector4i c = m_local.d4.c;

					// rb = rb.add16(c.xxxx());
					// ga = ga.add16(c.yyyy());

					BROADCAST_OR_LOAD(vpbroadcastq, movdqa, c, _rip_local_d(c));

					pshufd(rb, c, _MM_SHUFFLE(0, 0, 0, 0));
					pshufd(ga, c, _MM_SHUFFLE(1, 1, 1, 1));

					COMBINE(paddw, _f_rb, rb, _rip_local(temp.rb));
					COMBINE(paddw, _f_ga, ga, _rip_local(temp.ga));

					// FIXME: color may underflow and roll over at the end of the line, if decreasing

					pxor(c, c);
					pmaxsw(is64 ? _f_rb : rb, c);
					pmaxsw(is64 ? _f_ga : ga, c);

					ONLY32(movdqa(_rip_local(temp.rb), rb));
					ONLY32(movdqa(_rip_local(temp.ga), ga));
				}
				else
				{
					if(m_sel.tfx == TFX_NONE)
					{
						ONLY32(movdqa(rb, ptr[&m_local.c.rb]));
						ONLY32(movdqa(ga, ptr[&m_local.c.ga]));
					}
				}

				ONLY64(movdqa(_rb, _f_rb));
				ONLY64(movdqa(_ga, _f_ga));
			}
		}

		if(!m_sel.notest)
		{
			// test = m_test[7 + (steps & (steps >> 31))];

			mov(eax, a0.cvt32());
			sar(eax, 31); // GH: 31 to extract the sign of the register
			and(eax, a0.cvt32());
			if (isXmm)
				shl(eax, 4);
			ONLY64(cdqe());

#if USING_XMM
			movdqa(_test, ptr[rax + _g_const + offsetof(GSScanlineConstantData, m_test_128b[7])]);
#else
			vpmovsxbd(_test, ptr[rax*8 + _g_const + offsetof(GSScanlineConstantData, m_test_256b[15])]);
#endif
		}
	}

	/// Inputs: xym0[x86]=z, t1=fza_base, t0=fza_offset, _test
	/// Outputs: rbp=za
	/// Destroys: rax, xym0, temp1, temp2
	void TestZ(const XYm& temp1, const XYm& temp2)
	{
		if(!m_sel.zb)
		{
			return;
		}

		const XYm& z = is64 ? _z : xym0;

		// int za = fza_base.y + fza_offset->y;

		mov(ebp, dword[t1 + 4]);
		add(ebp, dword[t0 + 4]);
		and(ebp, HALF_VM_SIZE - 1);

		// GSVector4i zs = zi;

		if(m_sel.prim != GS_SPRITE_CLASS)
		{
			if(m_sel.zoverflow)
			{
				// zs = (GSVector4i(z * 0.5f) << 1) | (GSVector4i(z) & GSVector4i::x00000001());

				auto m_half = loadAddress(rax, &GSVector4::m_half);

				if (hasAVX)
					vbroadcastss(temp1, ptr[m_half]);
				else
					movaps(temp1, ptr[m_half]);
				mulps(temp1, z);
				cvttps2dq(temp1, temp1);
				pslld(temp1, 1);

				cvttps2dq(xym0, z);
				pcmpeqd(temp2, temp2);
				psrld(temp2, 31);
				pand(xym0, temp2);

				por(xym0, temp1);
			}
			else
			{
				// zs = GSVector4i(z);

				cvttps2dq(xym0, z);
			}

			if (m_sel.zclamp)
			{
				const uint8 amt = (uint8)((m_sel.zpsm & 0x3) * 8);
				if (hasSSE41)
				{
					pcmpeqd(temp1, temp1);
					psrld(temp1, amt);
					pminsd(xym0, temp1);
				}
				else
				{
					pcmpeqd(temp1, temp1);
					psrld(temp1, amt);
					pcmpgtd(temp1, xym0);
					pand(xym0, temp1);
					pcmpeqd(temp2, temp2);
					pxor(temp1, temp2);
					psrld(temp1, amt);
					por(xym0, temp1);
				}
			}

			if(m_sel.zwrite)
			{
//	#ifdef _WIN64
				movdqa(_rip_local(temp.zs), xym0);
//	#else
//				movdqa(ptr[rsp + _rz_zs], xym0);
//	#endif
			}
		}
		else
		{
			ONLY64(movdqa(xym0, _z));
		}

		if(m_sel.ztest)
		{
			ReadPixel(temp2, temp1, rbp);

			if(m_sel.zwrite && m_sel.zpsm < 2)
			{
//	#ifdef _WIN64
				movdqa(_rip_local(temp.zd), temp2);
//	#else
//				movdqa(ptr[rsp + _rz_zd], temp2);
//	#endif
			}

			// zd &= 0xffffffff >> m_sel.zpsm * 8;

			if(m_sel.zpsm)
			{
				pslld(temp2, static_cast<uint8>(m_sel.zpsm * 8));
				psrld(temp2, static_cast<uint8>(m_sel.zpsm * 8));
			}

			if(m_sel.zoverflow || m_sel.zpsm == 0)
			{
				// GSVector4i o = GSVector4i::x80000000();

				pcmpeqd(temp1, temp1);
				pslld(temp1, 31);

				// GSVector4i zso = zs - o;
				// GSVector4i zdo = zd - o;

				psubd(xym0, temp1);
				psubd(temp2, temp1);
			}

			switch(m_sel.ztst)
			{
			case ZTST_GEQUAL:
				// test |= zso < zdo; // ~(zso >= zdo)
				pcmpgtd(temp2, xym0);
				por(_test, temp2);
				break;

			case ZTST_GREATER: // TODO: tidus hair and chocobo wings only appear fully when this is tested as ZTST_GEQUAL
				// test |= zso <= zdo; // ~(zso > zdo)
				pcmpgtd(xym0, temp2);
				pcmpeqd(temp1, temp1);
				pxor(xym0, temp1);
				por(_test, xym0);
				break;
			}

			alltrue(_test);
		}
	}

	/// Input[x86]: xym4=q, xym2=s, xym3=t
	/// Output: _rb, _ga
	/// Destroys everything except xym7[x86]
	void SampleTexture()
	{
		if(!m_sel.fb || m_sel.tfx == TFX_NONE)
		{
			return;
		}


		if (is32)
		{
			mov(a3.cvt32(), ptr[&m_local.gd->tex[0]]);

			if(m_sel.tlu)
			{
				mov(a1.cvt32(), ptr[&m_local.gd->clut]);
			}
		}

		const bool needsMoreRegs = !hasSSE41 || isYmm;

		// x86-64 and i386 want the output of this function in different places :(
		// Solve by remapping registers
		const XYm
			&xtm0 = xym0, // Temp register
			&xtm1 = xym1, // Temp register
			&xtm2 = is64 ? xym5 : xym2, // Input register for s on i386
			&xtm3 = is64 ? xym6 : xym3, // Input register for t on i386
			&xtm4 = xym4, // Temp register
//			&xtm5 = is64 ? xym2 : xym5, // Output register for rb
//			&xtm6 = is64 ? xym3 : xym6, // Output register for ga
			&xtm7 = xym7; // Available on x64 only

		if(!m_sel.fst)
		{
			rcpps(xtm0, is64 ? _q : xtm4);

			MOVE_IF_64(mulps, xtm2, _s, xtm0);
			MOVE_IF_64(mulps, xtm3, _t, xtm0);

			cvttps2dq(xtm2, xtm2);
			cvttps2dq(xtm3, xtm3);

			if(m_sel.ltf)
			{
				// u -= 0x8000;
				// v -= 0x8000;

				mov(eax, 0x8000);
				BROADCAST_GPR_TO_VEC(xtm1, eax);

				psubd(xtm2, xtm1);
				psubd(xtm3, xtm1);
			}
		}
		else
		{
			ONLY64(movdqa(xtm2, _s));
			ONLY64(movdqa(xtm3, _t));
		}

		if(m_sel.ltf)
		{
			const XYm& vf = is64 ? xtm7 : xtm0;

			// GSVector4i uf = u.xxzzlh().srl16(12);

			pshuflw(xtm4, xtm2, _MM_SHUFFLE(2, 2, 0, 0));
			pshufhw(xtm4, xtm4, _MM_SHUFFLE(2, 2, 0, 0));
			psrlw(xtm4, 12);
			if (is32 && needsMoreRegs)
				movdqa(_rip_local(temp.uf), xtm4);

			if(m_sel.prim != GS_SPRITE_CLASS)
			{
				// GSVector4i vf = v.xxzzlh().srl16(12);

				pshuflw(vf, xtm3, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(vf, vf, _MM_SHUFFLE(2, 2, 0, 0));
				psrlw(vf, 12);
				if (is32 || needsMoreRegs)
					movdqa(_rip_local(temp.vf), vf);
			}
			else if (is64 && !needsMoreRegs)
			{
				movdqa(vf, _rip_local(temp.vf));
			}
		}

		// GSVector4i uv0 = u.sra32(16).ps32(v.sra32(16));

		psrad(xtm2, 16);
		psrad(xtm3, 16);
		packssdw(xtm2, xtm3);

		if(m_sel.ltf)
		{
			// GSVector4i uv1 = uv0.add16(GSVector4i::x0001());

			pcmpeqd(xtm0, xtm0);
			psrlw(xtm0, 15);
			THREEARG(paddw, xtm3, xtm2, xtm0);

			// uv0 = Wrap(uv0);
			// uv1 = Wrap(uv1);

			Wrap(xtm2, xtm3);
		}
		else
		{
			// uv0 = Wrap(uv0);

			Wrap(xtm2);
		}

		// xtm2 = uv0
		// xtm3 = uv1
		// xtm4 = uf[x64||!needsMoreRegs]
		// xtm7 = used[x86] vf[x64&&!needsMoreRegs]
		// Free: xtm0, xtm1, xtm5, xtm6

		SampleTexture_TexelReadHelper();

		// xtm5 = rb (xym5[x86], xym2[x64])
		// xtm6 = ga (xym6[x86], xym3[x64])
	}

	/// Input[x86]: xym2=uv0, xym3=uv1 (ltf), xym4=uf (!needsMoreRegs)
	/// Input[x64]: xym2=uv0, xym3=uv1 (ltf), xym4=uf, xym7=vf (!needsMoreRegs)
	/// Output[x86]: xym5=rb, xym6=ga
	/// Output[x64]: xym2=rb, xym3=ga
	/// Destroys all registers except outputs, xmm4 and xmm7
	void SampleTexture_TexelReadHelper()
	{
		const bool needsMoreRegs = !hasSSE41 || isYmm;
		// Match registers from SampleTexture
		const XYm
			&xtm0 = xym0,
			&xtm1 = xym1,
			&xtm2 = is64 ? xym5 : xym2,
			&xtm3 = is64 ? xym6 : xym3,
			&xtm4 = xym4,
			&xtm5 = is64 ? xym2 : xym5, // Output register for rb
			&xtm6 = is64 ? xym3 : xym6, // Output register for ga
			&xtm7 = xym7;

		// GSVector4i x0 = uv0.upl16();
		// GSVector4i y0 = uv0.uph16() << tw;

		pxor(xtm0, xtm0);

		THREEARG(punpcklwd, xtm5, xtm2, xtm0);
		punpckhwd(xtm2, xtm0);
		pslld(xtm2, static_cast<uint8>(m_sel.tw + 3));

		// xtm0 = 0
		// xtm2 = y0
		// xtm3 = uv1 (ltf)
		// xtm4 = uf[x64||!needsMoreRegs]
		// xtm5 = x0
		// xtm7 = used[x86] vf[x64&&!needsMoreRegs]
		// Free: xtm1, xtm6

		if(m_sel.ltf)
		{
			// GSVector4i x1 = uv1.upl16();
			// GSVector4i y1 = uv1.uph16() << tw;

			THREEARG(punpcklwd, xtm1, xtm3, xtm0);
			punpckhwd(xtm3, xtm0);
			pslld(xtm3, static_cast<uint8>(m_sel.tw + 3));

			// xtm1 = x1
			// xtm2 = y0
			// xtm3 = y1
			// xtm4 = uf[x64||!needsMoreRegs]
			// xtm5 = x0
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]
			// Free: xtm0, xtm6

			// GSVector4i addr00 = y0 + x0;
			// GSVector4i addr01 = y0 + x1;
			// GSVector4i addr10 = y1 + x0;
			// GSVector4i addr11 = y1 + x1;

			THREEARG(paddd, xtm0, xtm3, xtm1); // addr11
			paddd(xtm1, xtm2); // addr01
			paddd(xtm2, xtm5); // addr00
			paddd(xtm3, xtm5); // addr10

			// xtm0 = addr11
			// xtm1 = addr01
			// xtm2 = addr00
			// xtm3 = addr10
			// xtm4 = uf[x64||!needsMoreRegs]
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]
			// Free: xtm4, xtm5

			// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);
			// c01 = addr01.gather32_32((const uint32/uint8*)tex[, clut]);
			// c10 = addr10.gather32_32((const uint32/uint8*)tex[, clut]);
			// c11 = addr11.gather32_32((const uint32/uint8*)tex[, clut]);

			const Xmm& tmp1 = is64 ? xtm7 : xtm4; // OK to destroy if needsMoreRegs
			const Xmm& tmp2 = is64 ? xtm4 : xtm7;
			//         d0    d1    d2s0  d3s1  s1    s2
			ReadTexel4(xtm5, xtm6, xtm0, xtm2, xtm1, xtm3, tmp1, tmp2, 0);

			// xtm0 = c01
			// xtm2 = c10
			// xtm4 = uf[x64||!needsMoreRegs]
			// xtm5 = c11
			// xtm6 = c00
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]

			if (is32 && needsMoreRegs)
				movdqa(xtm4, _rip_local(temp.uf));

			// GSVector4i rb00 = c00 & mask;
			// GSVector4i ga00 = (c00 >> 8) & mask;

			split16_2x8(xtm3, xtm6, xtm6);

			// GSVector4i rb01 = c01 & mask;
			// GSVector4i ga01 = (c01 >> 8) & mask;

			split16_2x8(xtm0, xtm1, xtm0);

			// xtm0 = rb01
			// xtm1 = ga01
			// xtm2 = c10
			// xtm3 = rb00
			// xtm4 = uf
			// xtm5 = c11
			// xtm6 = ga00
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]

			// rb00 = rb00.lerp16_4(rb01, uf);
			// ga00 = ga00.lerp16_4(ga01, uf);

			lerp16_4(xtm0, xtm3, xtm4);
			lerp16_4(xtm1, xtm6, xtm4);

			// xtm0 = rb00
			// xtm1 = ga00
			// xtm2 = c10
			// xtm4 = uf
			// xtm5 = c11
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]

			// GSVector4i rb10 = c10 & mask;
			// GSVector4i ga10 = (c10 >> 8) & mask;

			split16_2x8(xtm2, xtm3, xtm2);

			// GSVector4i rb11 = c11 & mask;
			// GSVector4i ga11 = (c11 >> 8) & mask;

			split16_2x8(xtm5, xtm6, xtm5);

			// xtm0 = rb00
			// xtm1 = ga00
			// xtm2 = rb10
			// xtm3 = ga10
			// xtm4 = uf
			// xtm5 = rb11
			// xtm6 = ga11
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]

			// rb10 = rb10.lerp16_4(rb11, uf);
			// ga10 = ga10.lerp16_4(ga11, uf);

			lerp16_4(xtm5, xtm2, xtm4);
			lerp16_4(xtm6, xtm3, xtm4);

			// xtm0 = rb00
			// xtm1 = ga00
			// xtm5 = rb10
			// xtm6 = ga10
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]

			// rb00 = rb00.lerp16_4(rb10, vf);
			// ga00 = ga00.lerp16_4(ga10, vf);

			XYm vf = is64 ? xtm7 : xtm2;
			if (needsMoreRegs || is32)
				movdqa(vf, _rip_local(temp.vf));

			lerp16_4(xtm5, xtm0, vf);
			lerp16_4(xtm6, xtm1, vf);
		}
		else
		{
			// GSVector4i addr00 = y0 + x0;

			paddd(xtm2, xtm5);

			// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);

			ReadTexel1(xtm5, xtm2, xtm0, xtm1, 0);

			// GSVector4i mask = GSVector4i::x00ff();

			// c[0] = c00 & mask;
			// c[1] = (c00 >> 8) & mask;

			split16_2x8(xtm5, xtm6, xtm5);
		}
	}

	void Wrap(const XYm& uv)
	{
		// Registers free from SampleTexture
		const XYm
			&mask = xym0,
			&min = xym1,
			&max = is64 ? xym2 : xym5,
			&tmp = is64 ? xym3 : xym6;

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					BROADCAST_AND_OP(vbroadcasti128, pmaxsw, uv, min, _rip_global(t.min));
				}
				else
				{
					pxor(tmp, tmp);
					pmaxsw(uv, tmp);
				}

				BROADCAST_AND_OP(vbroadcasti128, pminsw, uv, max, _rip_global(t.max));
			}
			else
			{
				BROADCAST_AND_OP(vbroadcasti128, pmaxsw, uv, min, _rip_global(t.min));

				if(region)
				{
					BROADCAST_AND_OP(vbroadcasti128, pminsw, uv, max, _rip_global(t.max));
				}
			}
		}
		else
		{
			BROADCAST_OR_LOAD(vbroadcasti128, movdqa, min, _rip_global(t.min));
			BROADCAST_OR_LOAD(vbroadcasti128, movdqa, max, _rip_global(t.max));
			BROADCAST_OR_LOAD(vbroadcasti128, movdqa, mask, _rip_global(t.mask));

			// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;
			THREEARG(pand, tmp, uv, min);
			if(region)
				por(tmp, max);
			// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);
			pmaxsw(uv, min);
			pminsw(uv, max);
			// clamp.blend8(repeat, m_local.gd->t.mask);
			blend8(uv, tmp /*, xym0==mask */);
		}
	}

	/// Destroys[x86]: xym0, xym1, xym2, xym3, xym4[!sse41]
	/// Destroys[x64]: xym0, xym1, xym5, xym6, xym7[!sse41]
	void Wrap(const XYm& uv0, const XYm& uv1)
	{
		// Registers free from SampleTexture
		const XYm
			&mask = xym0,
			&min = xym1,
			&max = is64 ? xym2 : xym5,
			&tmp = is64 ? xym3 : xym6;

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					BROADCAST_OR_LOAD(vbroadcasti128, movdqa, min, _rip_global(t.min));
					pmaxsw(uv0, min);
					pmaxsw(uv1, min);
				}
				else
				{
					pxor(tmp, tmp);
					pmaxsw(uv0, tmp);
					pmaxsw(uv1, tmp);
				}

				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, max, _rip_global(t.max));
				pminsw(uv0, max);
				pminsw(uv1, max);
			}
			else
			{
				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, min, _rip_global(t.min));
				pand(uv0, min);
				pand(uv1, min);

				if(region)
				{
					BROADCAST_OR_LOAD(vbroadcasti128, movdqa, max, _rip_global(t.max));
					por(uv0, max);
					por(uv1, max);
				}
			}
		}
		else
		{
			const XYm& mask2 = is64 ? xym7 : xym4; // <SSE4.1 only
			BROADCAST_OR_LOAD(vbroadcasti128, movdqa, min, _rip_global(t.min));
			BROADCAST_OR_LOAD(vbroadcasti128, movdqa, max, _rip_global(t.max));
			if (hasSSE41)
			{
				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, mask, _rip_global(t.mask));
			}
			else
			{
				movdqa(mask, _rip_global(t.invmask));
				movdqa(mask2, mask);
			}


			for (const XYm& uv : {uv0, uv1})
			{
				// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;
				THREEARG(pand, tmp, uv, min);
				if(region)
					por(tmp, max);
				// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);
				pmaxsw(uv, min);
				pminsw(uv, max);
				// clamp.blend8(repeat, m_local.gd->t.mask);
				if (hasSSE41)
					pblendvb(uv, tmp /*, xym0==mask */);
				else
					blendr(uv, tmp, uv == uv0 ? mask : mask2);
			}
		}
	}

	/// Input[x86]: xym4=q, xym2=s, xym3=t
	/// Output: _rb, _ga
	/// Destroys everything except xym7[x86]
	void SampleTextureLOD()
	{
		if(!m_sel.fb || m_sel.tfx == TFX_NONE)
		{
			return;
		}

		if(is32)
		{
			push(ebp);

			mov(ebp, (size_t)m_local.gd->tex);

			if(m_sel.tlu)
			{
				mov(edx, ptr[&m_local.gd->clut]);
			}
		}

		const bool needsMoreRegs = !hasSSE41 || isYmm;

		// x86-64 and i386 want the output of this function in different places :(
		// Solve by remapping registers
		const XYm
			&xtm0 = xym0, // Temp register
			&xtm1 = xym1, // Temp register
			&xtm2 = is64 ? xym5 : xym2, // Input register for s on i386
			&xtm3 = is64 ? xym6 : xym3, // Input register for t on i386
			&xtm4 = xym4, // Temp register
			&xtm5 = is64 ? xym2 : xym5, // Output register for rb
			&xtm6 = is64 ? xym3 : xym6, // Output register for ga
			&xtm7 = xym7; // Available on x64 only

		if(is64)
			movdqa(xtm4, _q);

		if(!m_sel.fst)
		{
			rcpps(xtm0, xtm4);

			MOVE_IF_64(mulps, xtm2, _s, xtm0);
			MOVE_IF_64(mulps, xtm3, _t, xtm0);

			cvttps2dq(xtm2, xtm2);
			cvttps2dq(xtm3, xtm3);
		}

		// xtm2 = u
		// xtm3 = v
		// xtm4 = q
		// xtm0 = xtm1 = xtm5 = xtm6 = free

		// TODO: if the fractional part is not needed in round-off mode then there is a faster integer log2 (just take the exp) (but can we round it?)

		if(!m_sel.lcm)
		{
			// lod = -log2(Q) * (1 << L) + K

			pcmpeqd(xtm1, xtm1);
			psrld(xtm1, 25);
			THREEARG(pslld, xtm0, xtm4, 1);
			psrld(xtm0, 24);
			psubd(xtm0, xtm1);
			cvtdq2ps(xtm0, xtm0);

			// xtm0 = (float)(exp(q) - 127)

			pslld(xtm4, 9);
			psrld(xtm4, 9);

			auto log2_coeff = [this](int i) -> Address
			{
				if (isXmm)
					return ptr[_g_const + offsetof(GSScanlineConstantData, m_log2_coef_128b[i])];
				else
					return ptr[_g_const + offsetof(GSScanlineConstantData, m_log2_coef_256b[i])];
			};

			orps(xtm4, log2_coeff(3));

			// xtm4 = mant(q) | 1.0f

			if(hasFMA)
			{
				movaps(xtm5, log2_coeff(0)); // c0
				vfmadd213ps(xtm5, xtm4, log2_coeff(1)); // c0 * xtm4 + c1
				vfmadd213ps(xtm5, xtm4, log2_coeff(2)); // (c0 * xtm4 + c1) * xtm4 + c2
				subps(xtm4, log2_coeff(3)); // xtm4 - 1.0f
				vfmadd213ps(xtm4, xtm5, xtm0); // ((c0 * xtm4 + c1) * xtm4 + c2) * (xtm4 - 1.0f) + xtm0
			}
			else
			{
				THREEARG(mulps, xtm5, xtm4, log2_coeff(0));
				addps(xtm5, log2_coeff(1));
				mulps(xtm5, xtm4);
				subps(xtm4, log2_coeff(3));
				addps(xtm5, log2_coeff(2));
				mulps(xtm4, xtm5);
				addps(xtm4, xtm0);
			}

			// xtm4 = log2(Q) = ((((c0 * xtm4) + c1) * xtm4) + c2) * (xtm4 - 1.0f) + xtm0

			if(hasFMA)
			{
				movaps(xtm5, _rip_global(l));
				vfmadd213ps(xtm4, xtm5, _rip_global(k));
			}
			else
			{
				mulps(xtm4, _rip_global(l));
				addps(xtm4, _rip_global(k));
			}

			// xtm4 = (-log2(Q) * (1 << L) + K) * 0x10000

			xorps(xtm0, xtm0);
			minps(xtm4, _rip_global(mxl));
			maxps(xtm4, xtm0);
			cvtps2dq(xtm4, xtm4);

			if(m_sel.mmin == 1) // round-off mode
			{
				mov(eax, 0x8000);
				BROADCAST_GPR_TO_VEC(xtm0, eax);
				paddd(xtm4, xtm0);
			}

			THREEARG(psrld, xtm0, xtm4, 16);

			movdqa(_rip_local(temp.lod.i), xtm0);
			/*
			 vpslld(xtm5, xtm0, 6);
			 vpslld(xtm6, xtm4, 16);
			 vpsrld(xtm6, xtm6, 24);
			 return;
			 */
			if(m_sel.mmin == 2) // trilinear mode
			{
				pshuflw(xtm1, xtm4, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(xtm1, xtm1, _MM_SHUFFLE(2, 2, 0, 0));
				movdqa(_rip_local(temp.lod.f), xtm1);
			}

			// shift u/v/minmax by (int)lod

			if(hasAVX2)
			{
				vpsravd(xtm2, xtm2, xtm0);
				vpsravd(xtm3, xtm3, xtm0);

				movdqa(_rip_local(temp.uv[0]), xtm2);
				movdqa(_rip_local(temp.uv[1]), xtm3);

				// m_local.gd->t.minmax => m_local.temp.uv_minmax[0/1]

				pxor(xtm1, xtm1);

				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, xtm4, _rip_global(t.min));
				vpunpcklwd(xtm5, xtm4, xtm1); // minu
				vpunpckhwd(xtm6, xtm4, xtm1); // minv
				vpsrlvd(xtm5, xtm5, xtm0);
				vpsrlvd(xtm6, xtm6, xtm0);
				packusdw(xtm5, xtm6);

				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, xtm4, _rip_global(t.max));
				vpunpcklwd(xtm6, xtm4, xtm1); // maxu
				vpunpckhwd(xtm4, xtm4, xtm1); // maxv
				vpsrlvd(xtm6, xtm6, xtm0);
				vpsrlvd(xtm4, xtm4, xtm0);
				packusdw(xtm6, xtm4);

				movdqa(_rip_local(temp.uv_minmax[0]), xtm5);
				movdqa(_rip_local(temp.uv_minmax[1]), xtm6);
			}
			else
			{
				movq(xtm4, _rip_global(t.minmax));

				THREEARG(punpckhdq, xtm6, xtm2, xtm3);
				punpckldq(xtm2, xtm3);
				movdqa(xtm5, xtm2);
				movdqa(xtm3, xtm6);

				movd(xtm0, _rip_local(temp.lod.i.u32[0]));
				psrad(xtm2, xtm0);
				THREEARG(psrlw, xtm1, xtm4, xtm0);
				movq(_rip_local(temp.uv_minmax[0].u32[0]), xtm1);

				movd(xtm0, _rip_local(temp.lod.i.u32[1]));
				psrad(xtm5, xtm0);
				THREEARG(psrlw, xtm1, xtm4, xtm0);
				movq(_rip_local(temp.uv_minmax[1].u32[0]), xtm1);

				movd(xtm0, _rip_local(temp.lod.i.u32[2]));
				psrad(xtm3, xtm0);
				THREEARG(psrlw, xtm1, xtm4, xtm0);
				movq(_rip_local(temp.uv_minmax[0].u32[2]), xtm1);

				movd(xtm0, _rip_local(temp.lod.i.u32[3]));
				psrad(xtm6, xtm0);
				THREEARG(psrlw, xtm1, xtm4, xtm0);
				movq(_rip_local(temp.uv_minmax[1].u32[2]), xtm1);

				punpckldq(xtm2, xtm3);
				punpckhdq(xtm5, xtm6);
				THREEARG(punpckhdq, xtm3, xtm2, xtm5);
				punpckldq(xtm2, xtm5);

				movdqa(_rip_local(temp.uv[0]), xtm2);
				movdqa(_rip_local(temp.uv[1]), xtm3);

				movdqa(xtm5, _rip_local(temp.uv_minmax[0]));
				movdqa(xtm6, _rip_local(temp.uv_minmax[1]));

				if (hasAVX)
				{
					vpunpcklwd(xtm0, xtm5, xtm6);
					vpunpckhwd(xtm1, xtm5, xtm6);
					vpunpckldq(xtm5, xtm0, xtm1);
					vpunpckhdq(xtm6, xtm0, xtm1);
				}
				else
				{
					movdqa(xtm0, xtm5);
					punpcklwd(xtm5, xtm6);
					punpckhwd(xtm0, xtm6);
					movdqa(xtm6, xtm5);
					punpckldq(xtm5, xtm0);
					punpckhdq(xtm6, xtm0);
				}

				movdqa(_rip_local(temp.uv_minmax[0]), xtm5);
				movdqa(_rip_local(temp.uv_minmax[1]), xtm6);
			}
		}
		else
		{
			// lod = K

			movd(xtm0, _rip_global(lod.i.u32[0]));

			psrad(xtm2, xtm0);
			psrad(xtm3, xtm0);

			movdqa(_rip_local(temp.uv[0]), xtm2);
			movdqa(_rip_local(temp.uv[1]), xtm3);

			movdqa(xtm5, _rip_local(temp.uv_minmax[0]));
			movdqa(xtm6, _rip_local(temp.uv_minmax[1]));
		}

		// xtm2 = m_local.temp.uv[0] = u (level m)
		// xtm3 = m_local.temp.uv[1] = v (level m)
		// xtm5 = minuv
		// xtm6 = maxuv

		if(m_sel.ltf)
		{
			const XYm& vf = is64 ? xtm7 : xtm0;
			// u -= 0x8000;
			// v -= 0x8000;

			mov(eax, 0x8000);
			BROADCAST_GPR_TO_VEC(xtm4, eax);

			psubd(xtm2, xtm4);
			psubd(xtm3, xtm4);

			// GSVector4i uf = u.xxzzlh().srl16(1);

			pshuflw(xtm4, xtm2, _MM_SHUFFLE(2, 2, 0, 0));
			pshufhw(xtm4, xtm4, _MM_SHUFFLE(2, 2, 0, 0));
			psrlw(xtm4, 12);
			if (is32 && needsMoreRegs)
				movdqa(_rip_local(temp.uf), xtm4);

			// GSVector4i vf = v.xxzzlh().srl16(1);

			pshuflw(vf, xtm3, _MM_SHUFFLE(2, 2, 0, 0));
			pshufhw(vf, vf, _MM_SHUFFLE(2, 2, 0, 0));
			psrlw(vf, 12);
			if (is32 || needsMoreRegs)
				movdqa(_rip_local(temp.vf), vf);
		}

		// GSVector4i uv0 = u.sra32(16).ps32(v.sra32(16));

		psrad(xtm2, 16);
		psrad(xtm3, 16);
		packssdw(xtm2, xtm3);

		if(m_sel.ltf)
		{
			// GSVector4i uv1 = uv0.add16(GSVector4i::x0001());

			pcmpeqd(xtm1, xtm1);
			psrlw(xtm1, 15);
			THREEARG(paddw, xtm3, xtm2, xtm1);

			// uv0 = Wrap(uv0);
			// uv1 = Wrap(uv1);

			WrapLOD(xtm2, xtm3);
		}
		else
		{
			// uv0 = Wrap(uv0);

			WrapLOD(xtm2);
		}

		// xtm2 = uv0
		// xtm3 = uv1 (ltf)
		// xtm4 = uf[x64||!needsMoreRegs]
		// xtm7 = used[x86] vf[x64&&!needsMoreRegs]
		// Free: xtm0, xtm1, xtm5, xtm6

		SampleTexture_TexelReadHelper();

		// xtm5: rb
		// xtm6: ga


		if(m_sel.mmin != 1) // !round-off mode
		{
			movdqa(_rip_local(temp.trb), xtm5);
			movdqa(_rip_local(temp.tga), xtm6);

			movdqa(xtm2, _rip_local(temp.uv[0]));
			movdqa(xtm3, _rip_local(temp.uv[1]));

			psrad(xtm2, 1);
			psrad(xtm3, 1);

			movdqa(xtm5, _rip_local(temp.uv_minmax[0]));
			movdqa(xtm6, _rip_local(temp.uv_minmax[1]));

			psrlw(xtm5, 1);
			psrlw(xtm6, 1);

			if(m_sel.ltf)
			{
				const XYm& vf = is64 ? xtm7 : xtm0;
				// u -= 0x8000;
				// v -= 0x8000;

				mov(eax, 0x8000);
				BROADCAST_GPR_TO_VEC(xtm4, eax);

				psubd(xtm2, xtm4);
				psubd(xtm3, xtm4);

				// GSVector4i uf = u.xxzzlh().srl16(1);

				pshuflw(xtm4, xtm2, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(xtm4, xtm4, _MM_SHUFFLE(2, 2, 0, 0));
				psrlw(xtm4, 12);
				if (is32 && needsMoreRegs)
					movdqa(_rip_local(temp.uf), xtm4);

				// GSVector4i vf = v.xxzzlh().srl16(1);

				pshuflw(vf, xtm3, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(vf, vf, _MM_SHUFFLE(2, 2, 0, 0));
				psrlw(vf, 12);
				if (is32 || needsMoreRegs)
					movdqa(_rip_local(temp.vf), vf);
			}

			// GSVector4i uv0 = u.sra32(16).ps32(v.sra32(16));

			psrad(xtm2, 16);
			psrad(xtm3, 16);
			packssdw(xtm2, xtm3);

			if(m_sel.ltf)
			{
				// GSVector4i uv1 = uv0.add16(GSVector4i::x0001());

				pcmpeqd(xtm1, xtm1);
				psrlw(xtm1, 15);
				vpaddw(xtm3, xtm2, xtm1);

				// uv0 = Wrap(uv0);
				// uv1 = Wrap(uv1);

				WrapLOD(xtm2, xtm3);
			}
			else
			{
				// uv0 = Wrap(uv0);

				WrapLOD(xtm2);
			}

			// xtm2 = uv0
			// xtm3 = uv1 (ltf)
			// xtm4 = uf[x64||!needsMoreRegs]
			// xtm7 = used[x86] vf[x64&&!needsMoreRegs]
			// Free: xtm0, xtm1, xtm5, xtm6

			SampleTexture_TexelReadHelper();

			// xtm5: rb
			// xtm6: ga

			movdqa(xtm0, m_sel.lcm ? _rip_global(lod.f) : _rip_local(temp.lod.f));
			psrlw(xtm0, 1);

			movdqa(xtm2, _rip_local(temp.trb));
			movdqa(xtm3, _rip_local(temp.tga));

			lerp16(xtm5, xtm2, xtm0, 0);
			lerp16(xtm6, xtm3, xtm0, 0);
		}

		if (is32)
			pop(ebp);
	}

	void WrapLOD(const XYm& uv)
	{
		// Registers free from SampleTexture
		const XYm
			&mask = xym0,
			&tmp = xym1,
			&min = is64 ? xym2 : xym5,
			&max = is64 ? xym3 : xym6;

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					pmaxsw(uv, min);
				}
				else
				{
					pxor(tmp, tmp);
					pmaxsw(uv, tmp);
				}

				pminsw(uv, max);
			}
			else
			{
				pand(uv, min);

				if(region)
				{
					por(uv, max);
				}
			}
		}
		else
		{
			BROADCAST_OR_LOAD(vbroadcasti128, movdqa, mask, _rip_global(t.mask));

			// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;
			THREEARG(pand, tmp, uv, min);
			if(region)
				por(tmp, max);
			// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);
			pmaxsw(uv, min);
			pminsw(uv, max);
			// clamp.blend8(repeat, m_local.gd->t.mask);
			blend8(uv, tmp /*, xym0==mask */);
		}
	}

	void WrapLOD(const XYm& uv0, const XYm& uv1)
	{
		// Registers free from SampleTexture
		const XYm
			&mask = xym0,
			&tmp = xym1,
			&min = is64 ? xym2 : xym5,
			&max = is64 ? xym3 : xym6;

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					pmaxsw(uv0, min);
					pmaxsw(uv1, min);
				}
				else
				{
					pxor(tmp, tmp);
					pmaxsw(uv0, tmp);
					pmaxsw(uv1, tmp);
				}

				pminsw(uv0, max);
				pminsw(uv1, max);
			}
			else
			{
				pand(uv0, min);
				pand(uv1, min);

				if(region)
				{
					por(uv0, max);
					por(uv1, max);
				}
			}
		}
		else
		{
			const XYm& mask2 = is64 ? xym7 : xym4; // <SSE4.1 only
			if (hasSSE41)
			{
				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, mask, _rip_global(t.mask));
			}
			else
			{
				movdqa(mask, _rip_global(t.invmask));
				movdqa(mask2, mask);
			}

			for (const XYm& uv : {uv0, uv1})
			{
				// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;
				THREEARG(pand, tmp, uv, min);
				if(region)
					por(tmp, max);
				// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);
				pmaxsw(uv, min);
				pminsw(uv, max);
				// clamp.blend8(repeat, m_local.gd->t.mask);*
				if (hasSSE41)
					pblendvb(uv, tmp /*, xym0==mask */);
				else
					blendr(uv, tmp, uv == uv0 ? mask : mask2);
			}
		}
	}

	/// Input: _ga
	/// Output: xym2[x86]=gaf (TFX_HIGHLIGHT || TFX_HIGHLIGHT2 && !tcc)
	/// Destroys: xym0, xym1, xym3[x86], xym4[x86]
	void AlphaTFX()
	{
		if(!m_sel.fb)
		{
			return;
		}

		const XYm& f_ga  = is64 ? _f_ga : xym4;
		const XYm& tmpga = is64 ? xym1  : f_ga;
		const XYm& tmp   = is64 ? xym0  : xym3;
		Address _32_gaptr = m_sel.iip ? _rip_local(temp.ga) : _rip_local(c.ga);

		switch(m_sel.tfx)
		{
			case TFX_MODULATE:

				// GSVector4i ga = iip ? gaf : m_local.c.ga;

				ONLY32(movdqa(f_ga, _32_gaptr));

				// gat = gat.modulate16<1>(ga).clamp8();

				modulate16(_ga, f_ga, 1);

				clamp16(_ga, tmp);

				// if(!tcc) gat = gat.mix16(ga.srl16(7));

				if(!m_sel.tcc)
				{
					MOVE_IF_64(psrlw, tmpga, f_ga, 7);

					mix16(_ga, tmpga, tmp);
				}

				break;

			case TFX_DECAL:

				// if(!tcc) gat = gat.mix16(ga.srl16(7));
				if(!m_sel.tcc)
				{
					// GSVector4i ga = iip ? gaf : m_local.c.ga;

					ONLY32(movdqa(f_ga, _32_gaptr));

					MOVE_IF_64(psrlw, tmpga, f_ga, 7);

					mix16(_ga, tmpga, tmp);
				}

				break;

			case TFX_HIGHLIGHT:

				// GSVector4i ga = iip ? gaf : m_local.c.ga;

				ONLY32(movdqa(f_ga, _32_gaptr));
				ONLY32(movdqa(xym2, f_ga)); // WHY

				// gat = gat.mix16(!tcc ? ga.srl16(7) : gat.addus8(ga.srl16(7)));

				MOVE_IF_64(psrlw, tmpga, f_ga, 7);

				if(m_sel.tcc)
				{
					paddusb(tmpga, _ga);
				}

				mix16(_ga, tmpga, tmp);

				break;

			case TFX_HIGHLIGHT2:

				// if(!tcc) gat = gat.mix16(ga.srl16(7));

				if(!m_sel.tcc)
				{
					// GSVector4i ga = iip ? gaf : m_local.c.ga;

					ONLY32(movdqa(f_ga, _32_gaptr));
					ONLY32(movdqa(xym2, f_ga));

					MOVE_IF_64(psrlw, tmpga, f_ga, 7);

					mix16(_ga, tmpga, tmp);
				}

				break;

			case TFX_NONE:

				// gat = iip ? ga.srl16(7) : ga;

				if(m_sel.iip)
				{
					MOVE_IF_64(psrlw, _ga, f_ga, 7);
				}

				break;
		}

		if(m_sel.aa1)
		{
			// gs_user figure 3-2: anti-aliasing after tfx, before tests, modifies alpha

			// FIXME: bios config screen cubes

			if(!m_sel.abe)
			{
				// a = cov

				if(m_sel.edge)
				{
//#ifdef _WIN64
					movdqa(xym0, _rip_local(temp.cov));
//#else
//					movdqa(xym0, ptr[rsp + _rz_cov]);
//#endif
				}
				else
				{
					pcmpeqd(xym0, xym0);
					psllw(xym0, 15);
					psrlw(xym0, 8);
				}

				mix16(_ga, xym0, xym1);
			}
			else
			{
				// a = a == 0x80 ? cov : a

				pcmpeqd(xym0, xym0);
				psllw(xym0, 15);
				psrlw(xym0, 8);

				if(m_sel.edge)
				{
//#ifdef _WIN64
					movdqa(xym1, _rip_local(temp.cov));
//#else
//					movdqa(xym1, ptr[rsp + _rz_cov]);
//#endif
				}
				else
				{
					movdqa(xym1, xym0);
				}

				pcmpeqw(xym0, _ga);
				psrld(xym0, 16);
				pslld(xym0, 16);

				blend8(_ga, xym1 /*, xym0 */);
			}
		}
	}

	/// Output: _fm, _zm
	void ReadMask()
	{
		if(m_sel.fwrite)
		{
			BROADCAST_OR_LOAD(vpbroadcastd, movdqa, _fm, _rip_global(fm));
		}

		if(m_sel.zwrite)
		{
			BROADCAST_OR_LOAD(vpbroadcastd, movdqa, _zm, _rip_global(zm));
		}
	}

	/// Input[x86]: xym3=fm, xym4=zm, xym6=ga
	/// Input[x64]: _ga, _fm, _zm
	/// Destroys: xym0, xym1
	void TestAlpha()
	{
		const XYm& ga = is64 ? _ga : xym6;
		switch(m_sel.atst)
		{
			case ATST_NEVER:
				// t = GSVector4i::xffffffff();
				pcmpeqd(xym1, xym1);
				break;

			case ATST_ALWAYS:
				return;

			case ATST_LESS:
			case ATST_LEQUAL:
				// t = (ga >> 16) > m_local.gd->aref;
				THREEARG(psrld, xym1, ga, 16);
				BROADCAST_AND_OP(vbroadcasti128, pcmpgtd, xym1, xym0, _rip_global(aref));
				break;

			case ATST_EQUAL:
				// t = (ga >> 16) != m_local.gd->aref;
				THREEARG(psrld, xym1, ga, 16);
				BROADCAST_AND_OP(vbroadcasti128, pcmpeqd, xym1, xym0, _rip_global(aref));
				pcmpeqd(xym0, xym0);
				pxor(xym1, xym0);
				break;

			case ATST_GEQUAL:
			case ATST_GREATER:
				// t = (ga >> 16) < m_local.gd->aref;
				THREEARG(psrld, xym0, ga, 16);
				BROADCAST_OR_LOAD(vbroadcasti128, movdqa, xym1, _rip_global(aref));
				pcmpgtd(xym1, xym0);
				break;

			case ATST_NOTEQUAL:
				// t = (ga >> 16) == m_local.gd->aref;
				THREEARG(psrld, xym1, ga, 16);
				BROADCAST_AND_OP(vbroadcasti128, pcmpeqd, xym1, xym0, _rip_global(aref));
				break;
		}

		switch(m_sel.afail)
		{
			case AFAIL_KEEP:
				// test |= t;
				por(_test, xym1);
				alltrue(_test);
				break;

			case AFAIL_FB_ONLY:
				// zm |= t;
				por(_zm, xym1);
				break;

			case AFAIL_ZB_ONLY:
				// fm |= t;
				por(_fm, xym1);
				break;

			case AFAIL_RGB_ONLY:
				// zm |= t;
				por(_zm, xym1);
				// fm |= t & GSVector4i::xff000000();
				psrld(xym1, 24);
				pslld(xym1, 24);
				por(_fm, xym1);
				break;
		}
	}

	/// Input: xym2[x86]=gaf, _rb, _ga
	/// Destroys: xym0, xym1, xym2[x86]
	void ColorTFX()
	{
		if(!m_sel.fwrite)
		{
			return;
		}

		const XYm& f_ga  = is64 ? _f_ga : xym2;
		const XYm& tmpga = is64 ? xym6  : f_ga;

		auto modulate16_1_rb = [&]{
			// GSVector4i rb = iip ? rbf : m_local.c.rb;
			if (is64)
				modulate16(_rb, _f_rb, 1);
			else
				modulate16(_rb, m_sel.iip ? _rip_local(temp.rb) : _rip_local(c.rb), 1);
		};

		switch(m_sel.tfx)
		{
			case TFX_MODULATE:

				// GSVector4i rb = iip ? rbf : m_local.c.rb;

				// rbt = rbt.modulate16<1>(rb).clamp8();

				modulate16_1_rb();

				clamp16(_rb, xym0);

				break;

			case TFX_DECAL:

				break;

			case TFX_HIGHLIGHT:
			case TFX_HIGHLIGHT2:

				if(m_sel.tfx == TFX_HIGHLIGHT2 && m_sel.tcc)
				{
					// GSVector4i ga = iip ? gaf : m_local.c.ga;

					ONLY32(movdqa(f_ga, m_sel.iip ? _rip_local(temp.ga) : _rip_local(c.ga)));
				}

				// gat = gat.modulate16<1>(ga).add16(af).clamp8().mix16(gat);

				movdqa(xym1, _ga);

				modulate16(_ga, f_ga, 1);

				pshuflw(tmpga, f_ga, _MM_SHUFFLE(3, 3, 1, 1));
				pshufhw(tmpga, tmpga, _MM_SHUFFLE(3, 3, 1, 1));
				psrlw(tmpga, 7);

				paddw(_ga, tmpga);

				clamp16(_ga, xym0);

				mix16(_ga, xym1, xym0);

				// rbt = rbt.modulate16<1>(rb).add16(af).clamp8();

				modulate16_1_rb();

				paddw(_rb, tmpga);

				clamp16(_rb, xym0);

				break;

			case TFX_NONE:

				// rbt = iip ? rb.srl16(7) : rb;

				if(m_sel.iip)
				{
					MOVE_IF_64(psrlw, _rb, _f_rb, 7);
				}

				break;
		}
	}

	/// Input: _rb, _ga
	/// Destroys: xym0, xym1, xym2[x86]
	void Fog()
	{
		if(!m_sel.fwrite || !m_sel.fge)
		{
			return;
		}

		const XYm& f   = is64 ? _f   : xym0;
		const XYm& tmp = is64 ? xym0 : xym2;

		// rb = m_local.gd->frb.lerp16<0>(rb, f);
		// ga = m_local.gd->fga.lerp16<0>(ga, f).mix16(ga);

		if (m_sel.prim != GS_SPRITE_CLASS)
		{
			ONLY32(movdqa(f, _rip_local(temp.f)));
		}
		else
		{
			ONLY32(BROADCAST_OR_LOAD(vpbroadcastw, movdqa, f, _rip_local(p.f)));
		}

		movdqa(xym1, _ga);

		BROADCAST_OR_LOAD(vpbroadcastd, movdqa, tmp, _rip_global(frb));
		lerp16(_rb, tmp, f, 0);

		BROADCAST_OR_LOAD(vpbroadcastd, movdqa, tmp, _rip_global(fga));
		lerp16(_ga, tmp, f, 0);

		mix16(_ga, xym1, f);
	}

	/// Outputs: _fd, rbx=fa
	void ReadFrame()
	{
		if(!m_sel.fb)
		{
			return;
		}

		mov(ebx, dword[t1]);
		add(ebx, dword[t0]);
		and(ebx, HALF_VM_SIZE - 1);

		if(!m_sel.rfb)
		{
			return;
		}

		ReadPixel(_fd, xym0, rbx);
	}

	/// Input: _fd, _test
	/// Destroys: xym0, xym1
	void TestDestAlpha()
	{
		if(!m_sel.date || m_sel.fpsm != 0 && m_sel.fpsm != 2)
		{
			return;
		}

		// test |= ((fd [<< 16]) ^ m_local.gd->datm).sra32(31);

		if(m_sel.datm)
		{
			if(m_sel.fpsm == 2)
			{
				pxor(xym0, xym0);
				//vpsrld(xym1, _fd, 15);
				THREEARG(pslld, xym1, _fd, 16);
				psrad(xym1, 31);
				pcmpeqd(xym1, xym0);
			}
			else
			{
				pcmpeqd(xym0, xym0);
				THREEARG(pxor, xym1, _fd, xym0);
				psrad(xym1, 31);
			}
		}
		else
		{
			if(m_sel.fpsm == 2)
			{
				THREEARG(pslld, xym1, _fd, 16);
				psrad(xym1, 31);
			}
			else
			{
				THREEARG(psrad, xym1, _fd, 31);
			}
		}

		por(_test, xym1);

		alltrue(_test);
	}

	/// Input: _fm, _zm, _test
	/// Output: edx=fzm
	/// Destroys: xym0, xym1
	void WriteMask()
	{
		if(m_sel.notest)
		{
			return;
		}

		// fm |= test;
		// zm |= test;

		if(m_sel.fwrite)
		{
			por(_fm, _test);
		}

		if(m_sel.zwrite)
		{
			por(_zm, _test);
		}

		// int fzm = ~(fm == GSVector4i::xffffffff()).ps32(zm == GSVector4i::xffffffff()).mask();

		pcmpeqd(xym1, xym1);

		if(m_sel.fwrite && m_sel.zwrite)
		{
			THREEARG(pcmpeqd, xym0, xym1, _zm);
			pcmpeqd(xym1, _fm);
			packssdw(xym1, xym0);
		}
		else if(m_sel.fwrite)
		{
			pcmpeqd(xym1, _fm);
			packssdw(xym1, xym1);
		}
		else if(m_sel.zwrite)
		{
			pcmpeqd(xym1, _zm);
			packssdw(xym1, xym1);
		}

		pmovmskb(edx, xym1);

		not(edx);
	}

	/// Inputs: rbp=za, edx=fzm, _zm
	/// Destroys: xym0, xym1, xym7
	void WriteZBuf()
	{
		if(!m_sel.zwrite)
		{
			return;
		}

		if (m_sel.prim != GS_SPRITE_CLASS)
//#ifdef _WIN64
			movdqa(xym1, _rip_local(temp.zs));
//#else
//			movdqa(xym1, ptr[rsp + _rz_zs]);
//#endif
		else
			BROADCAST_OR_LOAD(vpbroadcastd, movdqa, xym1, _rip_local(p.z));

		if(m_sel.ztest && m_sel.zpsm < 2)
		{
			// zs = zs.blend8(zd, zm);

			if (hasAVX)
			{
//#ifdef _WIN64
				vpblendvb(xym1, xym1, _rip_local(temp.zd), _zm);
//#else
//				pblendvb(xym1, ptr[rsp + _rz_zd], _zm);
//#endif
			}
			else
			{
				movdqa(xym0, _zm);
				movdqa(xym7, _rip_local(temp.zd));
				blend8(xym1, xym7 /*, xym0 */);
			}
		}

		// Clamp Z to ZPSM_FMT_MAX
		if (m_sel.zclamp)
		{
			const uint8 amt = (uint8)((m_sel.zpsm & 0x3) * 8);
			if (hasSSE41)
			{
				pcmpeqd(xym7, xym7);
				psrld(xym7, amt);
				pminsd(xym1, xym7);
			}
			else
			{
				pcmpeqd(xym7, xym7);
				psrld(xym7, amt);
				pcmpgtd(xym7, xym1);
				pand(xym1, xym7);
				pcmpeqd(xym0, xym0);
				pxor(xym7, xym0);
				psrld(xym7, amt);
				por(xym1, xym7);
			}
		}

		bool fast = m_sel.ztest ? m_sel.zpsm < 2 : m_sel.zpsm == 0 && m_sel.notest;

#if USING_XMM
		WritePixel(xym1, rbp, dh, fast, m_sel.zpsm, 1);
#else
		WritePixel(xym1, rbp, edx, fast, m_sel.zpsm, 1);
#endif
	}

	/// Input: _fd, _rb, _ga
	/// Destroys: xym0, xym1, xym4[x86], xym5[x64], xym7[x86], xym15[x64]
	void AlphaBlend()
	{
		if(!m_sel.fwrite)
		{
			return;
		}

		if(m_sel.abe == 0 && m_sel.aa1 == 0)
		{
			return;
		}

		const XYm& _dst_rb = xym0;
		const XYm& _dst_ga = xym1;
		const XYm& tmp1 = _test;
		const XYm& tmp2 = is64 ? xym5 : xym4;

		if((m_sel.aba != m_sel.abb) && (m_sel.aba == 1 || m_sel.abb == 1 || m_sel.abc == 1) || m_sel.abd == 1)
		{
			switch(m_sel.fpsm)
			{
				case 0:
				case 1:

					// c[2] = fd & mask;
					// c[3] = (fd >> 8) & mask;

					split16_2x8(_dst_rb, _dst_ga, _fd);

					break;

				case 2:

					// c[2] = ((fd & 0x7c00) << 9) | ((fd & 0x001f) << 3);
					// c[3] = ((fd & 0x8000) << 8) | ((fd & 0x03e0) >> 2);

					pcmpeqd(tmp1, tmp1);

					psrld(tmp1, 27); // 0x0000001f
					THREEARG(pand, _dst_rb, _fd, tmp1);
					pslld(_dst_rb, 3);

					pslld(tmp1, 10); // 0x00007c00
					THREEARG(pand, tmp2, _fd, tmp1);
					pslld(tmp2, 9);

					por(_dst_rb, tmp2);

					psrld(tmp1, 5); // 0x000003e0
					THREEARG(pand, _dst_ga, _fd, tmp1);
					psrld(_dst_ga, 2);

					psllw(tmp1, 10); // 0x00008000
					THREEARG(pand, tmp2, _fd, tmp1);
					pslld(tmp2, 8);

					por(_dst_ga, tmp2);

					break;
			}
		}

		// rb,   ga   = src rb, ga
		// xym0, xym1 = dst rb, ga
		// tmp1, tmp2 = free

		if(m_sel.pabe || (m_sel.aba != m_sel.abb) && (m_sel.abb == 0 || m_sel.abd == 0))
		{
			movdqa(tmp2, _rb);
		}

		if(m_sel.aba != m_sel.abb)
		{
			// rb = c[aba * 2 + 0];

			switch(m_sel.aba)
			{
				case 0: break;
				case 1: movdqa(_rb, _dst_rb); break;
				case 2: pxor(_rb, _rb); break;
			}

			// rb = rb.sub16(c[abb * 2 + 0]);

			switch(m_sel.abb)
			{
				case 0: psubw(_rb, tmp2); break;
				case 1: psubw(_rb, _dst_rb); break;
				case 2: break;
			}

			if(!(m_sel.fpsm == 1 && m_sel.abc == 1))
			{
				// GSVector4i a = abc < 2 ? c[abc * 2 + 1].yywwlh().sll16(7) : m_local.gd->afix;

				switch(m_sel.abc)
				{
					case 0:
					case 1:
						pshuflw(tmp1, m_sel.abc ? _dst_ga : _ga, _MM_SHUFFLE(3, 3, 1, 1));
						pshufhw(tmp1, tmp1, _MM_SHUFFLE(3, 3, 1, 1));
						psllw(tmp1, 7);
						break;
					case 2:
						BROADCAST_OR_LOAD(vpbroadcastw, movdqa, tmp1, _rip_global(afix));
						break;
				}

				// rb = rb.modulate16<1>(a);

				modulate16(_rb, tmp1, 1);
			}

			// rb = rb.add16(c[abd * 2 + 0]);

			switch(m_sel.abd)
			{
				case 0: paddw(_rb, tmp2); break;
				case 1: paddw(_rb, _dst_rb); break;
				case 2: break;
			}
		}
		else
		{
			// rb = c[abd * 2 + 0];

			switch(m_sel.abd)
			{
				case 0: break;
				case 1: movdqa(_rb, _dst_rb); break;
				case 2: pxor(_rb, _rb); break;
			}
		}

		if(m_sel.pabe)
		{
			// mask = (c[1] << 8).sra32(31);

			THREEARG(pslld, xym0, _ga, 8);
			psrad(xym0, 31);

			// rb = c[0].blend8(rb, mask);

			blend8r(_rb, tmp1 /*, xym0 */);
		}

		// xym0 = pabe mask (>=sse41)
		// ga   = src ga
		// xym1 = dst ga
		// rb   = rb
		// tmp1 = a
		// tmp2 = free

		movdqa(tmp2, _ga);

		if(m_sel.aba != m_sel.abb)
		{
			// ga = c[aba * 2 + 1];

			switch(m_sel.aba)
			{
				case 0: break;
				case 1: movdqa(_ga, _dst_ga); break;
				case 2: pxor(_ga, _ga); break;
			}

			// ga = ga.sub16(c[abeb * 2 + 1]);

			switch(m_sel.abb)
			{
				case 0: psubw(_ga, tmp2); break;
				case 1: psubw(_ga, _dst_ga); break;
				case 2: break;
			}

			if(!(m_sel.fpsm == 1 && m_sel.abc == 1))
			{
				// ga = ga.modulate16<1>(a);

				modulate16(_ga, tmp1, 1);
			}

			// ga = ga.add16(c[abd * 2 + 1]);

			switch(m_sel.abd)
			{
				case 0: paddw(_ga, tmp2); break;
				case 1: paddw(_ga, _dst_ga); break;
				case 2: break;
			}
		}
		else
		{
			// ga = c[abd * 2 + 1];

			switch(m_sel.abd)
			{
				case 0: break;
				case 1: movdqa(_ga, _dst_ga); break;
				case 2: pxor(_ga, _ga); break;
			}
		}

		// xym0 = pabe mask (>=sse41)
		// tmp2 = src ga
		// rb = rb
		// ga = ga
		// xym1, tmp1 = free

		if(m_sel.pabe)
		{
			if (!hasSSE41)
			{
				// doh, previous blend8r overwrote xym0 (sse41+ uses pblendvb)
				THREEARG(pslld, xym0, _ga, 8);
				psrad(xym0, 31);
			}

			psrld(xym0, 16); // zero out high words to select the source alpha in blend (so it also does mix16)

			// ga = c[1].blend8(ga, mask).mix16(c[1]);

			blend8r(_ga, tmp2 /*, xym0 */);
		}
		else
		{
			if(m_sel.fpsm != 1) // TODO: fm == 0xffxxxxxx
			{
				mix16(_ga, tmp2, tmp1);
			}
		}
	}

	/// Input: rbx=fa, rdx=fzm, _fd, _fm
	/// Destroys[x86]: rax, xym4, xym5, xym6, xym7
	/// Destroys[x64]: rax, xym0, xym1, xym2, xym3, xym15
	void WriteFrame()
	{
		if(!m_sel.fwrite)
		{
			return;
		}


		const XYm& tmp0 = is64 ? xym15 : xym7;
		const XYm& tmp1 = is64 ? xym2 : xym5;
		const XYm& tmp2 = is64 ? xym3 : xym6;

		if(m_sel.fpsm == 2 && m_sel.dthe)
		{
			// y = (top & 3) << 5

			mov(eax, ptr[rsp + _top]);
			and(eax, 3);
			shl(eax, 5);

			// rb = rb.add16(m_global.dimx[0 + y]);
			// ga = ga.add16(m_global.dimx[1 + y]);

			add(rax, _rip_global(dimx));

			BROADCAST_AND_OP(vbroadcasti128, paddw, tmp1, tmp0, ptr[rax + sizeof(GSVector4i) * 0]);
			BROADCAST_AND_OP(vbroadcasti128, paddw, tmp2, tmp0, ptr[rax + sizeof(GSVector4i) * 1]);

		}

		if(m_sel.colclamp == 0)
		{
			// c[0] &= 0x00ff00ff;
			// c[1] &= 0x00ff00ff;

			pcmpeqd(tmp0, tmp0);
			psrlw(tmp0, 8);
			pand(tmp1, tmp0);
			pand(tmp2, tmp0);
		}

		// GSVector4i fs = c[0].upl16(c[1]).pu16(c[0].uph16(c[1]));

		THREEARG(punpckhwd, tmp0, tmp1, tmp2);
		punpcklwd(tmp1, tmp2);
		packuswb(tmp1, tmp0);

		if(m_sel.fba && m_sel.fpsm != 1)
		{
			// fs |= 0x80000000;

			pcmpeqd(tmp0, tmp0);
			pslld(tmp0, 31);
			por(tmp1, tmp0);
		}

		// tmp1 = fs
		// xym4 = fm
		// xym6 = fd

		if(m_sel.fpsm == 2)
		{
			// TODO:TKR: Why do we randomly switch register mappings between x86 and x64?

			if (is64)
			{
				// GSVector4i rb = fs & 0x00f800f8;
				// GSVector4i ga = fs & 0x8000f800;

				mov(eax, 0x00f800f8);
				BROADCAST_GPR_TO_VEC(xym0, eax);

				mov(eax, 0x8000f800);
				BROADCAST_GPR_TO_VEC(xym1, eax);

				pand(xym0, tmp1);
				pand(xym1, tmp1);

				// fs = (ga >> 16) | (rb >> 9) | (ga >> 6) | (rb >> 3);

				THREEARG(psrld, tmp1, xym0, 9);
				psrld(xym0, 3);
				THREEARG(psrld, xym3, xym1, 16);
				psrld(xym1, 6);

				por(xym0, xym1);
				por(tmp1, xym3);
				por(tmp1, xym0);
			}
			else
			{
				// GSVector4i rb = fs & 0x00f800f8;
				// GSVector4i ga = fs & 0x8000f800;

				mov(eax, 0x00f800f8);
				BROADCAST_GPR_TO_VEC(xym6, eax);

				mov(eax, 0x8000f800);
				BROADCAST_GPR_TO_VEC(xym7, eax);

				THREEARG(pand, xym4, tmp1, xym6);
				pand(tmp1, xym7);

				// fs = (ga >> 16) | (rb >> 9) | (ga >> 6) | (rb >> 3);

				THREEARG(psrld, xym6, xym4, 9);
				psrld(xym4, 3);
				THREEARG(psrld, xym7, tmp1, 16);
				psrld(xym5, 6);

				por(tmp1, xym4);
				por(xym7, xym6);
				por(tmp1, xym7);
			}
		}

		if(m_sel.rfb)
		{
			// fs = fs.blend(fd, fm);

			blend(tmp1, _fd, _fm); // TODO: could be skipped in certain cases, depending on fpsm and fm
		}

		bool fast = m_sel.rfb ? m_sel.fpsm < 2 : m_sel.fpsm == 0 && m_sel.notest;

#if USING_XMM
		WritePixel(tmp1, rbx, dl, fast, m_sel.fpsm, 0);
#else
		WritePixel(tmp1, rbx, edx, fast, m_sel.fpsm, 0);
#endif
	}

	/// Destroys: tmp[isYmm]
	void ReadPixel(const XYm& dst, const XYm& tmp, const AddressReg& addr)
	{
		RegExp base = _m_local__gd__vm + addr*2;
#if USING_XMM
		movq(dst, qword[base]);
		movhps(dst, qword[base + 8*2]);
#else
		Xmm dstXmm = Xmm(dst.getIdx());
		Xmm tmpXmm = Xmm(tmp.getIdx());
		movq(dstXmm, qword[base]);
		movhps(dstXmm, qword[base + 8*2]);
		movq(tmpXmm, qword[base + 16*2]);
		movhps(tmpXmm, qword[base + 24*2]);
		vinserti128(dst, dst, tmpXmm, 1);
#endif
	}

#if USING_XMM
	void WritePixel(const XYm& src_, const AddressReg& addr, const Reg8& mask, bool fast, int psm, int fz)
#else
	void WritePixel(const XYm& src_, const AddressReg& addr, const Reg32& mask, bool fast, int psm, int fz)
#endif
	{
#if USING_XMM
		const Xmm& src = src_;
		int shift = 0;
#else
		Xmm src = Xmm(src_.getIdx());
		int shift = fz*8;
#endif
		RegExp base = _m_local__gd__vm + addr*2;

		if(m_sel.notest)
		{
			if(fast)
			{
				movq(qword[base], src);
				movhps(qword[base + 8*2], src);
#if USING_YMM
				vextracti128(src, src_, 1);
				movq(qword[base + 16*2], src);
				movhps(qword[base + 24*2], src);
#endif
			}
			else
			{
				WritePixel(src, addr, 0, 0, psm);
				WritePixel(src, addr, 1, 1, psm);
				WritePixel(src, addr, 2, 2, psm);
				WritePixel(src, addr, 3, 3, psm);
#if USING_YMM
				vextracti128(src, src_, 1);
				WritePixel(src, addr, 4, 0, psm);
				WritePixel(src, addr, 5, 1, psm);
				WritePixel(src, addr, 6, 2, psm);
				WritePixel(src, addr, 7, 3, psm);
#endif
			}
		}
		else
		{
			if(fast)
			{
				// if(fzm & 0x0f) GSVector4i::storel(&vm16[addr + 0], fs);
				// if(fzm & 0xf0) GSVector4i::storeh(&vm16[addr + 8], fs);

				test(mask, 0x0000000f << shift);
				je("@f");
				movq(qword[base], src);
				L("@@");

				test(mask, 0x000000f0 << shift);
				je("@f");
				movhps(qword[base + 8*2], src);
				L("@@");

#if USING_YMM
				vextracti128(src, src_, 1);

				test(mask, 0x000f0000 << shift);
				je("@f");
				movq(qword[base + 16*2], src);
				L("@@");

				test(mask, 0x00f00000 << shift);
				je("@f");
				movhps(qword[base + 24*2], src);
				L("@@");
#endif
				// vmaskmovps?
			}
			else
			{
				// if(fzm & 0x03) WritePixel(fpsm, &vm16[addr + 0], fs.extract32<0>());
				// if(fzm & 0x0c) WritePixel(fpsm, &vm16[addr + 2], fs.extract32<1>());
				// if(fzm & 0x30) WritePixel(fpsm, &vm16[addr + 8], fs.extract32<2>());
				// if(fzm & 0xc0) WritePixel(fpsm, &vm16[addr + 10], fs.extract32<3>());

				test(mask, 0x00000003 << shift);
				je("@f");
				WritePixel(src, addr, 0, 0, psm);
				L("@@");

				test(mask, 0x0000000c << shift);
				je("@f");
				WritePixel(src, addr, 1, 1, psm);
				L("@@");

				test(mask, 0x00000030 << shift);
				je("@f");
				WritePixel(src, addr, 2, 2, psm);
				L("@@");

				test(mask, 0x000000c0 << shift);
				je("@f");
				WritePixel(src, addr, 3, 3, psm);
				L("@@");

#if USING_YMM
				vextracti128(src, src_, 1);

				test(mask, 0x00030000 << shift);
				je("@f");
				WritePixel(src, addr, 4, 0, psm);
				L("@@");

				test(mask, 0x000c0000 << shift);
				je("@f");
				WritePixel(src, addr, 5, 1, psm);
				L("@@");

				test(mask, 0x00300000 << shift);
				je("@f");
				WritePixel(src, addr, 6, 2, psm);
				L("@@");

				test(mask, 0x00c00000 << shift);
				je("@f");
				WritePixel(src, addr, 7, 3, psm);
				L("@@");
#endif
			}
		}
	}

	void WritePixel(const Xmm& src, const AddressReg& addr, uint8 i, uint8 j, int psm)
	{
		constexpr int s_offsets[4] = {0, 2, 8, 10};

		Address dst = ptr[_m_local__gd__vm + addr * 2 + s_offsets[i] * 2];

		switch(psm)
		{
			case 0:
				if(j == 0) movd(dst, src);
				else {
					if (hasSSE41) {
						pextrd(dst, src, j);
					} else {
						pshufd(xmm0, src, _MM_SHUFFLE(j, j, j, j));
						movd(dst, xmm0);
					}
				}
				break;
			case 1:
				if(j == 0) movd(eax, src);
				else {
					if (hasSSE41) {
						pextrd(eax, src, j);
					} else {
						pshufd(xmm0, src, _MM_SHUFFLE(j, j, j, j));
						movd(eax, xmm0);
					}
				}
				xor(eax, dst);
				and(eax, 0xffffff);
				xor(dst, eax);
				break;
			case 2:
				if(j == 0) movd(eax, src);
				else pextrw(eax, src, j * 2);
				mov(dst, ax);
				break;
		}
	}

	/// Input:
	///  a3 = m_local.tex[0]  (x86 && !m_sel.mmin)
	///  rbp  = m_local.tex (x86 && m_sel.mmin)
	///  a1  = m_local.clut (x86 && m_sel.tlu)
	/// Destroys: rax, src, tmp1, tmp2
	/// Destroys a3 (!m_sel.mmin)
	void ReadTexel1(const XYm& dst, const XYm& src, const XYm& tmp1, const XYm& tmp2, int mip_offset)
	{
		const Xmm no(-1); // Hopefully this will assert if we accidentally use it
		ReadTexelImpl(dst, tmp1, src, no, no, no, tmp2, no, 1, mip_offset);
	}

	/// Will process addr## to c## from s registers to d registers
	/// Destroys contents of s registers
	/// Destroys tmp1 if <sse41 or isYmm
	/// Will preserve tmp2
	/// Input:
	///  a3 = m_local.tex[0]  (x86 && !m_sel.mmin)
	///  rbp  = m_local.tex (x86 && m_sel.mmin)
	///  a1  = m_local.clut (x86 && m_sel.tlu)
	/// Destroys: rax
	/// Destroys a3 (!m_sel.mmin)
	void ReadTexel4(
		const XYm& d0,   const XYm& d1,
		const XYm& d2s0, const XYm& d3s1,
		const XYm& s2,   const XYm& s3,
		const XYm& tmp1, const XYm& tmp2,
		int mip_offset)
	{
		ReadTexelImpl(d0, d1, d2s0, d3s1, s2, s3, tmp1, tmp2, 4, mip_offset);
	}

	void ReadTexelImpl(
		const Xmm& d0,   const Xmm& d1,
		const Xmm& d2s0, const Xmm& d3s1,
		const Xmm& s2,   const Xmm& s3,
		const Xmm& tmp1, const Xmm& tmp2,
		int pixels,      int mip_offset)
	{
		mip_offset *= wordsize;
#if USING_XMM
		if (hasSSE41)
			ReadTexelImplSSE4(d0, d1, d2s0, d3s1, s2, s3, pixels, mip_offset);
		else
			ReadTexelImplSSE3(d0, d1, d2s0, d3s1, s2, s3, tmp1, tmp2, pixels, mip_offset);
#else
		ReadTexelImplYmm(d0, d1, d2s0, d3s1, s2, s3, tmp1, pixels, mip_offset);
#endif

	}

	void ReadTexelImplLoadTexLOD(int lod, int mip_offset)
	{
		AddressReg texIn = is64 ? _64_m_local__gd__tex : rbp;
		Address lod_addr = m_sel.lcm ? _rip_global(lod.i.u32[lod]) : _rip_local(temp.lod.i.u32[lod]);
		mov(a3.cvt32(), lod_addr);
		mov(a3, ptr[texIn + a3*wordsize + mip_offset]);
	}

	void ReadTexelImplYmm(
		const Ymm& d0,   const Ymm& d1,
		const Ymm& d2s0, const Ymm& d3s1,
		const Ymm& s2,   const Ymm& s3,
		const Ymm& tmp,
		int pixels,      int mip_offset)
	{
		const Ymm dst[] = { d0,   d1,   d2s0, d3s1 };
		const Ymm src[] = { d2s0, d3s1,   s2,   s3 };
		const Ymm t1[]  = { d1,   d2s0, d3s1,   s2 };
		const Ymm t2[]  = { tmp,  tmp,  tmp,  tmp  };

		bool texInA3 = is32;
		if(m_sel.mmin && m_sel.lcm)
		{
			ReadTexelImplLoadTexLOD(0, mip_offset);
			texInA3 = true;
		}

		for (int i = 0; i < pixels; i++) {
			const Xmm xdst{dst[i].getIdx()};
			const Xmm xsrc{src[i].getIdx()};
			const Xmm xt1{t1[i].getIdx()};
			const Xmm xt2{t2[i].getIdx()};

			if(m_sel.mmin && !m_sel.lcm)
			{
				texInA3 = true;

				vextracti128(xt1, src[i], 1);

				for (int j = 0; j < 4; j++)
				{
					ReadTexelImplLoadTexLOD(j, mip_offset);

					ReadTexelImpl(xdst, xsrc, j, texInA3, false);

					ReadTexelImplLoadTexLOD(j+4, mip_offset);

					ReadTexelImpl(xt2, xt1, j, texInA3, false);
				}

				vinserti128(dst[i], dst[i], xt2, 1);
			}
			else
			{
				AddressReg tex  = texInA3 ? a3 : _64_m_local__gd__tex;
				if (!m_sel.tlu)
				{
					pcmpeqd(t1[i], t1[i]);
					vpgatherdd(dst[i], ptr[tex + src[i]*4], t1[i]);
				}
				else
				{
					vextracti128(xt1, src[i], 1);

					for (int j = 0; j < 4; j++)
					{
						ReadTexelImpl(xdst, xsrc, j, texInA3, false);
						ReadTexelImpl(xt2, xt1, j, texInA3, false);
					}

					vinserti128(dst[i], dst[i], xt2, 1);

					/*
					pcmpeqd(t1[i], t1[i]);
					vpgatherdd(t2[i], ptr[tex + src[i]*1], t1[i]); // either this 1x scale, or the latency of two dependendent gathers are too slow
					pslld(t2[i], 24);
					psrld(t2[i], 24);
					pcmpeqd(t1[i], t1[i]);
					vpgatherdd(dst[i], ptr[clut + t2[i]*4], t1[i]);
					*/
				}
			}
		}
	}

	void ReadTexelImplSSE4(
		const Xmm& d0,   const Xmm& d1,
		const Xmm& d2s0, const Xmm& d3s1,
		const Xmm& s2,   const Xmm& s3,
		int pixels,      int mip_offset)
	{
		const bool preserve[] = { false, false, true, true };
		const Xmm dst[]       = { d0,    d1,    d2s0, d3s1 };
		const Xmm src[]       = { d2s0,  d3s1,    s2,   s3 };

		if (m_sel.mmin && !m_sel.lcm)
		{
			bool texInA3 = true;
			for (int j = 0; j < 4; j++)
			{
				ReadTexelImplLoadTexLOD(j, mip_offset);

				for (int i = 0; i < pixels; i++)
				{
					ReadTexelImpl(dst[i], src[i], j, texInA3, preserve[i]);
				}
			}
		}
		else
		{
			bool preserve = false;
			bool texInA3 = is32;

			if (m_sel.mmin && m_sel.lcm)
			{
				ReadTexelImplLoadTexLOD(0, mip_offset);
				texInA3 = true;
			}

			for (int i = 0; i < pixels; i++)
			{
				for (int j = 0; j < 4; j++)
				{
					ReadTexelImpl(dst[i], src[i], j, texInA3, preserve);
				}
			}
		}
	}

	void ReadTexelImplSSE3(
		const Xmm& d0,   const Xmm& d1,
		const Xmm& d2s0, const Xmm& d3s1,
		const Xmm& s2,   const Xmm& s3,
		const Xmm& tmp1, const Xmm& tmp2,
		int pixels,      int mip_offset)
	{
		bool texInA3 = is32;
		auto Read = [&](const Xmm& dst, const Xmm& src)
		{
			ReadTexelImpl(dst, src, 0, texInA3, /*preserveDst=*/false);
		};

		// Note: Should use d1 and tmp1 as temp if pixels == 1
		if(m_sel.mmin && !m_sel.lcm)
		{
			texInA3 = true;
			if (pixels == 1)
			{
				ReadTexelImplLoadTexLOD(0, mip_offset);
				Read(d0, d2s0);
				psrldq(d2s0, 4);

				ReadTexelImplLoadTexLOD(1, mip_offset);
				Read(d1, d2s0);
				psrldq(d2s0, 4);

				punpckldq(d0, d1);

				ReadTexelImplLoadTexLOD(2, mip_offset);
				Read(d1, d2s0);
				psrldq(d2s0, 4);

				ReadTexelImplLoadTexLOD(3, mip_offset);
				Read(tmp1, d2s0);

				punpckldq(d1, tmp1);

				punpcklqdq(d0, d1);
			}
			else
			{
				movdqa(_rip_local(temp.test), tmp2);

				Xmm dst[2][2] = {{d0,   d1}, {d2s0, d3s1}};
				Xmm src[2][2] = {{d2s0, d3s1}, {s2,   s3}};

				for (int i = 0; i < 2; i++)
				{
					ReadTexelImplLoadTexLOD(0, mip_offset);
					Read(dst[i][0], src[i][0]);
					psrldq(src[i][0], 4);
					Read(dst[i][1], src[i][1]);
					psrldq(src[i][1], 4);

					ReadTexelImplLoadTexLOD(1, mip_offset);
					Read(tmp1, src[i][0]);
					psrldq(src[i][0], 4);
					Read(tmp2, src[i][1]);
					psrldq(src[i][1], 4);

					punpckldq(dst[i][0], tmp1);
					punpckldq(dst[i][0], tmp2);

					ReadTexelImplLoadTexLOD(2, mip_offset);
					Read(tmp1, src[i][0]);
					psrldq(src[i][0], 4);
					Read(tmp2, src[i][1]);
					psrldq(src[i][1], 4);

					ReadTexelImplLoadTexLOD(3, mip_offset);
					Read(src[i][0], src[i][0]);
					Read(src[i][1], src[i][1]);

					punpckldq(tmp1, src[i][0]);
					punpckldq(tmp2, src[i][1]);

					punpcklqdq(dst[i][0], tmp1);
					punpcklqdq(dst[i][1], tmp2);
				}

				movdqa(tmp2, _rip_local(temp.test));
			}
		}
		else
		{
			if(m_sel.mmin && m_sel.lcm)
			{
				ReadTexelImplLoadTexLOD(0, mip_offset);
				texInA3 = true;
			}

			const Xmm dst[] = { d0,   d1,   d2s0, d3s1 };
			const Xmm src[] = { d2s0, d3s1,   s2,   s3 };
			const Xmm tmp[] = { d1,   d2s0, d3s1,   s2 };

			for (int i = 0; i < pixels; i++)
			{
				Read(dst[i], src[i]);
				psrldq(src[i], 4);
				Read(tmp[i], src[i]);
				psrldq(src[i], 4);
				punpckldq(dst[i], tmp[i]);

				Read(tmp[i], src[i]);
				psrldq(src[i], 4);
				Read(src[i], src[i]);
				punpckldq(tmp[i], src[i]);

				punpcklqdq(dst[i], tmp[i]);
			}
		}
	}

	void ReadTexelImpl(const Xmm& dst, const Xmm& addr, uint8 i, bool texInA3, bool preserveDst)
	{
		ASSERT(i < 4);

		AddressReg clut = is64 ? _64_m_local__gd__clut : a1;
		AddressReg tex  = texInA3 ? a3 : _64_m_local__gd__tex;
		Address src = m_sel.tlu ? ptr[clut + rax*4] : ptr[tex + rax*4];

		// Extract address offset
		if (i == 0) movd(eax, addr);
		else pextrd(eax, addr, i);

		// If clut, load the value as a byte index
		if (m_sel.tlu) movzx(eax, byte[tex + rax]);

		if (i == 0 && !preserveDst) movd(dst, src);
		else pinsrd(dst, src, i);
	}
};
