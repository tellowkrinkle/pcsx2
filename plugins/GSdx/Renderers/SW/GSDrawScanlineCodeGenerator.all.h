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
#define _64_m_test r10
#define _64_m_local r12
#define _64_m_local__gd r13
#define _64_m_local__gd__vm a1
#define _64_m_local__gd__vm a1
#define _64_m_local__gd__clut r11
// If m_sel.mmin, m_local.gd->tex, else m_local.gd->tex[0]
#define _64_m_local__gd__tex r14

#define _rip_local(field) ((Target::is32 || m_rip) ? (RegExp)(rip + (size_t)&m_local.field) : (RegExp)(_m_local + offsetof(GSScanlineLocalData, field)))
#define _rip_global(field) ((Target::is32 || m_rip) ? (RegExp)(rip + (size_t)&m_local.gd->field) : (RegExp)(_m_local__gd + offsetof(GSScanlineGlobalData, field)))
/// Executes the given code only if targeting 32-bit
#define ONLY32(code) if (is32) (code)
/// Executes the given code only if targeting 64-bit
#define ONLY64(code) if (is64) (code)
/// Combines temporary with either dst64 on 64-bit or src32 on 32-bit
/// Follow up with an ONLY32 save back to src32
#define COMBINE(operation, dst64, temporary, src32) \
	if (is32) \
		operation(temporary, ptr[src32]); \
	else \
		operation(dst64, temporary)
/// On x64, does a 3-operand move, on x86 uses a two-operand SSE-style
#define MOVE_IF_64(operation, dst, src64, ...) \
	if (is64) \
		v##operation(dst, src64, __VA_ARGS__); \
	else \
		operation(dst, __VA_ARGS__)

enum class RegsUsed {
	A0, A1, A2, A3, T0, T1
};

using namespace Xbyak;

#ifdef XBYAK_ONE_TRUE_TARGET
class GSDrawScanlineCodeGenerator2 : public Xbyak::SmartCodeGenerator
{
	using Target = XBYAK_ONE_TRUE_TARGET;
	using TargetVec = XBYAK_ONE_TRUE_TARGET_VEC;
	using _parent = Xbyak::SmartCodeGenerator;
#else
template <typename Target, typename TargetVec>
class GSDrawScanlineCodeGenerator2 : public Xbyak::SmartCodeGenerator<Target, TargetVec>
{
	using _parent = Xbyak::SmartCodeGenerator<Target, TargetVec>;
#endif

	/// On x86-64 we reserve a bunch of GPRs for holding addresses of locals that would otherwise be hard to reach
	/// On x86-32 the same values are just raw 32-bit addresses
	using LocalAddr = typename Target::Choose3264<size_t, AddressReg>::type;

	constexpr static bool is64 = Target::is64;
	constexpr static bool is32 = Target::is32;
	constexpr static int wordsize = is64 ? 8 : 4;

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

	/// Note: a2 is only available on x86-64
	AddressReg a0, a1, a2, a3, t0, t1;
	LocalAddr m_test, _m_local, _m_local__gd, _m_local__gd__vm, _m_local__gd__clut;
	/// All but _test are only available on x86-64
	Xmm _rb, _ga, _fm, _zm, _fd, _z, _f, _s, _t, _q, _f_rb, _f_ga, _test;

	/// Marks a register as unavailable in x86-32 by remapping it to a high register
	static int block32(int regID)
	{
		return is64 ? regID : regID | 8;
	}

	/// Returns the first arg on 32-bit, second on 64-bit
	static LocalAddr chooseLocal(const void *addr32, AddressReg reg64)
	{
		return Target::choose3264((size_t)addr32, reg64);
	}

public:
	GSDrawScanlineCodeGenerator2(Xbyak::CodeGenerator* base, bool hasFMA, void* param, uint64 key)
		: _parent(base, hasFMA)
		, m_local(*(GSScanlineLocalData*)param)
		, m_rip(false)
#ifdef _WIN32
		, a0(rcx) , a1(rdx)
		, a2(r8)  , a3(is64 ? r9 : rbx)
		, t0(rdi) , t1(rsi)
#else
		, a0(is64 ? rdi : rcx), a1(is64 ? rsi : rdx)
		, a2(is64 ? rdx : r8),  a3(is64 ? rcx : rbx)
		, t0(is64 ? r8  : rdi), t1(is64 ? r9  : rsi)
#endif
		, m_test(chooseLocal(g_const->m_test_128b[0], _64_m_test))
		, _m_local(chooseLocal(&m_local, _64_m_local))
		, _m_local__gd(chooseLocal(m_local.gd, _64_m_local__gd))
		, _m_local__gd__vm(chooseLocal(m_local.gd->vm, _64_m_local__gd__vm))
		, _rb(block32(2)), _ga(block32(3)), _fm(block32(4)), _zm(block32(5)), _fd(block32(6))
		, _z(8), _f(9), _s(10), _t(11), _q(12), _f_rb(13), _f_ga(14), _test(is64 ? 15 : 7)
	{
		m_sel.key = key;

		if(m_sel.breakpoint)
			db(0xCC);

		try {
			Generate();
		} catch (std::exception& e) {
			fprintf(stderr, "ERR:GSDrawScanlineCodeGenerator %s\n", e.what());
		}
	}

private:

// MARK: - Helpers

	/// Loads the given address into the given register if needed, and returns something that can be used in a `ptr[]`
	LocalAddr loadAddress(AddressReg reg, const void *addr)
	{
		if (is64)
			mov(reg, (size_t)addr);
		return Target::choose3264((size_t)addr, reg);
	}

	void modulate16(const Xmm& a, const Operand& f, uint8 shift)
	{
		if (shift == 0 && TargetVec::hasSSE3)
		{
			pmulhrsw(a, f);
		}
		else
		{
			psllw(a, shift + 1);
			pmulhw(a, f);
		}
	}

	void lerp16(const Xmm& a, const Xmm& b, const Xmm& f, uint8 shift)
	{
		psubw(a, b);
		modulate16(a, f, shift);
		paddw(a, b);
	}

	void lerp16_4(const Xmm& a, const Xmm& b, const Xmm& f)
	{
		psubw(a, b);
		pmullw(a, f);
		psraw(a, 4);
		paddw(a, b);
	}

	void mix16(const Xmm& a, const Xmm& b, const Xmm& temp)
	{
		if(TargetVec::hasSSE41)
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

	void clamp16(const Xmm& a, const Xmm& temp)
	{
		if (TargetVec::hasSSE41)
		{
			packuswb(a, a);
			if (TargetVec::hasAVX2)
			{
				// Greg: why ?
				ASSERT(a.isYMM());
				vpermq(Ymm(a.getIdx()), Ymm(a.getIdx()), _MM_SHUFFLE(3, 1, 2, 0)); // this sucks
			}
			pmovzxbw(a, a);
		}
		else
		{
			packuswb(a, a);
			pxor(temp, temp);
			punpcklbw(a, temp);
		}
	}

	void alltrue(const Xmm& test)
	{
		uint32 mask = test.isYMM() ? 0xffffffff : 0xffff;
		pmovmskb(eax, test);
		cmp(eax, mask);
		je("step", GSCodeGenerator::T_NEAR);
	}

	void blend(const Xmm& a, const Xmm& b, const Xmm& mask)
	{
		pand(b, mask);
		pandn(mask, a);
		if (TargetVec::hasAVX)
		{
			vpor(a, b, mask);
		}
		else
		{
			por(b, mask);
			movdqa(a, b);
		}
	}

	void blendr(const Xmm& b, const Xmm& a, const Xmm& mask)
	{
		pand(b, mask);
		pandn(mask, a);
		por(b, mask);
	}

	void blend8(const Xmm& a, const Xmm& b)
	{
		if (TargetVec::hasSSE41)
			pblendvb(a, b /*, xmm0 */);
		else
			blend(a, b, xmm0);
	}

	void blend8r(const Xmm& b, const Xmm& a)
	{
		if (TargetVec::hasAVX)
		{
			vpblendvb(b, a, b, xmm0);
		}
		else if (TargetVec::hasSSE41)
		{
			pblendvb(a, b);
			movdqa(b, a);
		}
		else
		{
			blendr(b, a, xmm0);
		}
	}

	void split16_2x8(const Xmm& l, const Xmm& h, const Xmm& src)
	{
		// l = src & 0xFF; (1 left shift + 1 right shift)
		// h = (src >> 8) & 0xFF; (1 right shift)

		if (TargetVec::hasAVX)
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
	void Generate()
	{
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
#ifdef _WIN64
			push(rbx);
			push(rsi);
			push(rdi);
			push(r12);
			push(r13);

			sub(rsp, _64_win_stack_size);

			for (int i = 0; i < 10; i++)
			{
				movdqa(ptr[rsp + _64_win_xmm_start + 16*i], Xmm(i+6));
			}
#else
			mov(ptr[rsp + _64_rz_rbx], rbx);
#endif
		}
	}

	/// Inputs: a0=pixels, a1=left, a2[x64]=top, a3[x64]=v
	void Init()
	{
		if(!m_sel.notest)
		{
			// int skip = left & 3;

			mov(ebx, a1.cvt32());
			and(a1.cvt32(), 3);

			// left -= skip;

			sub(ebx, a1.cvt32());

			// int steps = pixels + skip - 4;

			lea(a0.cvt32(), ptr[a0 + a1 - 4]);

			// GSVector4i test = m_test[skip] | m_test[7 + (steps & (steps >> 31))];

			shl(a1.cvt32(), 4); // * sizeof(m_test[0])

			movdqa(_test, ptr[a1 + m_test]);

			mov(eax, a0.cvt32());
			sar(eax, 31); // GH: 31 to extract the sign of the register
			and(eax, a0.cvt32());
			shl(eax, 4); // * sizeof(m_test[0])
			ONLY64(cdqe());

			por(_test, ptr[rax + (m_test + 7 * 16)]);
		}
		else
		{
			mov(ebx, a1.cvt32()); // left
			xor(a1.cvt32(), a1.cvt32()); // skip
			lea(a0.cvt32(), ptr[a0 - 4]); // steps
		}

		// a0 = steps
		// a1 = skip
		// rbx = left


		// GSVector2i* fza_base = &m_local.gd->fzbr[top];

		if (is64)
		{
			lea(t1, ptr[_rip_global(fzbr) + a2 * 8]);
		}
		else
		{
			mov(rax, ptr[rsp + _top]);
			lea(t1, ptr[rax * 8 + (size_t)&m_local.gd->fzbr]);
		}

		// GSVector2i* fza_offset = &m_local.gd->fzbc[left >> 2];

		lea(t0, ptr[_rip_global(fzbc) + rbx * 2]);

		if(m_sel.prim != GS_SPRITE_CLASS && (m_sel.fwrite && m_sel.fge || m_sel.zb) || m_sel.fb && (m_sel.edge || m_sel.tfx != TFX_NONE || m_sel.iip))
		{
			// a1 = &m_local.d[skip] // note a1 was (skip << 4)

			lea(a1, ptr[_rip_local(d) + a1 * 8]);

			// a3 starts on the stack in x86, we want it in a register
			if (Target::is32)
				mov(a3, ptr[rsp + _v]);
		}

		Xmm f = is64 ? _f : xmm1;

		if(m_sel.prim != GS_SPRITE_CLASS)
		{
			if(m_sel.fwrite && m_sel.fge || m_sel.zb)
			{
				movaps(xmm0, ptr[a3 + offsetof(GSVertexSW, p)]); // v.p

				if(m_sel.fwrite && m_sel.fge)
				{
					// f = GSVector4i(vp).zzzzh().zzzz().add16(m_local.d[skip].f);

					cvttps2dq(f, xmm0);
					pshufhw(f, f, _MM_SHUFFLE(2, 2, 2, 2));
					pshufd(f, f, _MM_SHUFFLE(2, 2, 2, 2));
					paddw(f, ptr[a1 + offsetof(GSScanlineLocalData::skip, f)]);

					if (Target::is32) // _f is shared on x86
						movdqa(ptr[&m_local.temp.f], f);
				}

				if(m_sel.zb)
				{
					// z = vp.zzzz() + m_local.d[skip].z;
					if (Target::is64)
					{
						// TODO: x64 non-avx
						vshufps(_z, xmm0, xmm0, _MM_SHUFFLE(2, 2, 2, 2));
						addps(_z, ptr[a1]);
					}
					else
					{
						shufps(xmm0, xmm0, _MM_SHUFFLE(2, 2, 2, 2));
						movaps(ptr[&m_local.temp.z], xmm0);
						movaps(xmm2, ptr[edx + offsetof(GSScanlineLocalData::skip, z)]);
						movaps(ptr[&m_local.temp.zo], xmm2);
						addps(xmm0, xmm2);
					}
				}
			}
		}
		else
		{
			if(m_sel.ztest)
			{
				movdqa(is64 ? _z : xmm0, ptr[_rip_local(p.z)]);
			}

			if(m_sel.fwrite && m_sel.fge && is64)
				movdqa(_f, ptr[_rip_local(p.f)]);
		}

		Xmm xt0 = is64 ? xmm0 : xmm4, xt1 = is64 ? xmm1 : xmm3;

		if(m_sel.fb)
		{
			if(m_sel.edge || m_sel.tfx != TFX_NONE)
			{
				movaps(xt0, ptr[a3 + offsetof(GSVertexSW, t)]); // v.t
			}

			if(m_sel.edge)
			{
				// m_local.temp.cov = GSVector4i::cast(v.t).zzzzh().wwww().srl16(9);

				pshufhw(xt1, xt0, _MM_SHUFFLE(2, 2, 2, 2));
				pshufd(xt1, xt1, _MM_SHUFFLE(3, 3, 3, 3));
				psrlw(xt1, 9);

				movdqa(ptr[_rip_local(temp.cov)], xt1);
//	#ifdef _WIN64
//				vmovdqa(_rip_local(temp.cov), xmm1);
//	#else
//				vmovdqa(ptr[rsp + _rz_cov], xmm1);
//	#endif
			}

			if(m_sel.tfx != TFX_NONE)
			{
				// a1 = &m_local.d[skip]

				if(m_sel.fst)
				{
					// GSVector4i vti(vt);

					Xmm vti = is64 ? xmm0 : xmm6;
					Xmm s = is64 ? _s : xmm2, t = is64 ? _t : xmm3;

					cvttps2dq(vti, xt0);

					// s = vti.xxxx() + m_local.d[skip].s;
					// t = vti.yyyy(); if(!sprite) t += m_local.d[skip].t;

					pshufd(s, vti, _MM_SHUFFLE(0, 0, 0, 0));
					pshufd(t, vti, _MM_SHUFFLE(1, 1, 1, 1));

					paddd(s, ptr[a1 + offsetof(GSScanlineLocalData::skip, s)]);

					if(m_sel.prim != GS_SPRITE_CLASS || m_sel.mmin)
					{
						paddd(t, ptr[a1 + offsetof(GSScanlineLocalData::skip, t)]);
					}
					else if(m_sel.ltf)
					{
						Xmm vf = is64 ? xmm7 : xmm6;
						pshuflw(vf, t, _MM_SHUFFLE(2, 2, 0, 0));
						pshufhw(vf, vf, _MM_SHUFFLE(2, 2, 0, 0));
						psrlw(vf, 12);
						if (is32)
							movdqa(ptr[&m_local.temp.vf], vf);
					}

					if (is32)
					{
						movdqa(ptr[&m_local.temp.s], s);
						movdqa(ptr[&m_local.temp.t], t);
					}
				}
				else
				{
					Xmm s = is64 ? _s : xmm2, t = is64 ? _t : xmm3, q = is64 ? _q : xmm4;

					// s = vt.xxxx() + m_local.d[skip].s;
					// t = vt.yyyy() + m_local.d[skip].t;
					// q = vt.zzzz() + m_local.d[skip].q;

					vshufps(s, xt0, xt0, _MM_SHUFFLE(0, 0, 0, 0));
					vshufps(t, xt0, xt0, _MM_SHUFFLE(1, 1, 1, 1));
					vshufps(q, xt0, xt0, _MM_SHUFFLE(2, 2, 2, 2));

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
				Xmm f_rb = is64 ? _f_rb : xmm5, f_ga = is64 ? _f_ga : xmm6;
				if(m_sel.iip)
				{
					// GSVector4i vc = GSVector4i(v.c);
					Xmm vc = is64 ? xmm0 : xmm6, tmp = is64 ? xmm1 : xmm5;


					cvttps2dq(vc, ptr[a3 + offsetof(GSVertexSW, c)]); // v.c

					// vc = vc.upl16(vc.zwxy());

					pshufd(tmp, vc, _MM_SHUFFLE(1, 0, 3, 2));
					punpcklwd(vc, tmp);

					// rb = vc.xxxx().add16(m_local.d[skip].rb);
					// ga = vc.zzzz().add16(m_local.d[skip].ga);

					pshufd(f_rb, vc, _MM_SHUFFLE(0, 0, 0, 0));
					pshufd(f_ga, vc, _MM_SHUFFLE(2, 2, 2, 2));

					paddw(f_rb, ptr[a1 + offsetof(GSScanlineLocalData::skip, rb)]);
					paddw(f_ga, ptr[a1 + offsetof(GSScanlineLocalData::skip, ga)]);

					ONLY32(movdqa(ptr[&m_local.temp.rb], f_rb));
					ONLY32(movdqa(ptr[&m_local.temp.ga], f_ga));
				}
				else if (is64 || m_sel.tfx == TFX_NONE)
				{
					movdqa(f_rb, ptr[_rip_local(c.rb)]);
					movdqa(f_ga, ptr[_rip_local(c.ga)]);
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

			mov(_64_m_local__gd__vm, ptr[_rip_global(vm)]);
			if(m_sel.fb && m_sel.tfx != TFX_NONE)
			{
				if (m_sel.mmin)
					lea(_64_m_local__gd__tex, ptr[_rip_global(tex)]);
				else
					mov(_64_m_local__gd__tex, ptr[_rip_global(tex)]);
			}
		}
	}

	/// Inputs: a0=steps, t0=fza_offset
	/// Destroys[x86]: all
	/// Destroys[x64]: xmm0, xmm1, xmm2, xmm3
	void Step()
	{
		// steps -= 4;

		sub(a0.cvt32(), 4);

		// fza_offset++;

		add(t0, 8);

		Xmm z = is64 ? _z : xmm0, f = is64 ? _f : xmm1;

		if(m_sel.prim != GS_SPRITE_CLASS)
		{
			// z += m_local.d4.z;

			if(m_sel.zb)
			{
				ONLY32(movaps(z, ptr[_rip_local(temp.zo)]));
				addps(z, ptr[_rip_local(d4.z)]);
				ONLY32(movaps(ptr[_rip_local(temp.zo)], z));
				ONLY32(addps(z, ptr[_rip_local(temp.z)]));
			}

			// f = f.add16(m_local.d4.f);

			if(m_sel.fwrite && m_sel.fge)
			{
				ONLY32(movdqa(f, ptr[_rip_local(temp.f)]));
				paddw(f, ptr[_rip_local(d4.f)]);
				ONLY32(movdqa(ptr[_rip_local(temp.f)], f));
			}
		}
		else
		{
			if(m_sel.ztest)
			{
				ONLY32(movdqa(z, ptr[_rip_local(p.z)]));
			}
		}

		if(m_sel.fb)
		{
			if(m_sel.tfx != TFX_NONE)
			{
				Xmm stq = is64 ? xmm0 : xmm4;
				if(m_sel.fst)
				{
					// GSVector4i stq = m_local.d4.stq;

					// s += stq.xxxx();
					// if(!sprite) t += st.yyyy();

					movdqa(stq, ptr[_rip_local(d4.stq)]);

					Xmm s = is64 ? xmm1 : xmm2;
					pshufd(s, stq, _MM_SHUFFLE(0, 0, 0, 0));
					COMBINE(paddd, _s, s, _rip_local(temp.s));
					ONLY32(movdqa(ptr[_rip_local(temp.s)], s));

					Xmm t = is64 ? xmm1 : xmm3;
					if(m_sel.prim != GS_SPRITE_CLASS || m_sel.mmin)
					{
						pshufd(t, stq, _MM_SHUFFLE(1, 1, 1, 1));
						COMBINE(paddd, _t, t, _rip_local(temp.t));
						ONLY32(movdqa(ptr[_rip_local(temp.t)], t));
					}
					else
					{
						ONLY32(movdqa(t, ptr[_rip_local(temp.t)]));
					}
				}
				else
				{
					Xmm s = xmm2, t = xmm3, q = is64 ? xmm1 : xmm4;
					// GSVector4 stq = m_local.d4.stq;

					// s += stq.xxxx();
					// t += stq.yyyy();
					// q += stq.zzzz();

					movaps(stq, ptr[_rip_local(d4.stq)]);

					vshufps(s, stq, stq, _MM_SHUFFLE(0, 0, 0, 0));
					vshufps(t, stq, stq, _MM_SHUFFLE(1, 1, 1, 1));
					vshufps(q, stq, stq, _MM_SHUFFLE(2, 2, 2, 2));

					COMBINE(addps, _s, s, _rip_local(temp.s));
					COMBINE(addps, _t, t, _rip_local(temp.t));
					COMBINE(addps, _q, q, _rip_local(temp.q));

					ONLY32(movaps(ptr[_rip_local(temp.s)], s));
					ONLY32(movaps(ptr[_rip_local(temp.t)], t));
					ONLY32(movaps(ptr[_rip_local(temp.q)], q));
				}
			}

			if(!(m_sel.tfx == TFX_DECAL && m_sel.tcc))
			{
				Xmm rb = is64 ? xmm1 : xmm5, ga = is64 ? xmm2 : xmm6;
				if(m_sel.iip)
				{
					Xmm c = is64 ? xmm0 : xmm7;
					// GSVector4i c = m_local.d4.c;

					// rb = rb.add16(c.xxxx());
					// ga = ga.add16(c.yyyy());

					movdqa(c, ptr[_rip_local(d4.c)]);

					pshufd(rb, c, _MM_SHUFFLE(0, 0, 0, 0));
					pshufd(ga, c, _MM_SHUFFLE(1, 1, 1, 1));

					COMBINE(paddw, _f_rb, rb, _rip_local(temp.rb));
					COMBINE(paddw, _f_ga, ga, _rip_local(temp.ga));

					// FIXME: color may underflow and roll over at the end of the line, if decreasing

					pxor(c, c);
					pmaxsw(is64 ? _f_rb : rb, c);
					pmaxsw(is64 ? _f_ga : ga, c);

					ONLY32(movdqa(ptr[_rip_local(temp.rb)], rb));
					ONLY32(movdqa(ptr[_rip_local(temp.ga)], ga));
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
			shl(eax, 4);
			ONLY64(cdqe());

			movdqa(_test, ptr[rax + m_test + 7 * 16]);
		}
	}

	/// Inputs: xmm0[x86]=z, t1=fza_base, t0=fza_offset
	/// Destroys: eax, ebp, xmm0, temp1, temp2
	void TestZ(const Xmm& temp1, const Xmm& temp2)
	{
		if(!m_sel.zb)
		{
			return;
		}

		Xmm z = is64 ? _z : xmm0;

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

				vbroadcastss(temp1, ptr[m_half]);
				mulps(temp1, z);
				cvttps2dq(temp1, temp1);
				pslld(temp1, 1);

				cvttps2dq(xmm0, z);
				pcmpeqd(temp2, temp2);
				psrld(temp2, 31);
				pand(xmm0, temp2);

				por(xmm0, temp1);
			}
			else
			{
				// zs = GSVector4i(z);

				cvttps2dq(xmm0, z);
			}

			if (is32 && m_sel.zclamp) // TODO:TKR: Why only 32?
			{
				pcmpeqd(temp1, temp1);
				psrld(temp1, (uint8)((m_sel.zpsm & 0x3) * 8));
				pminsd(xmm0, temp1);
			}

			if(m_sel.zwrite)
			{
//	#ifdef _WIN64
				movdqa(ptr[_rip_local(temp.zs)], xmm0);
//	#else
//				movdqa(ptr[rsp + _rz_zs], ztmp);
//	#endif
			}
		}
		else
		{
			ONLY64(movdqa(xmm0, _z));
		}

		if(m_sel.ztest)
		{
			ReadPixel(temp2, rbp);

			if(m_sel.zwrite && m_sel.zpsm < 2)
			{
//	#ifdef _WIN64
				movdqa(ptr[_rip_local(temp.zd)], temp2);
//	#else
//				movdqa(ptr[rsp + _rz_zd], xmm1);
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

				psubd(xmm0, temp1);
				psubd(temp2, temp1);
			}

			switch(m_sel.ztst)
			{
			case ZTST_GEQUAL:
				// test |= zso < zdo; // ~(zso >= zdo)
				pcmpgtd(temp2, xmm0);
				por(_test, temp2);
				break;

			case ZTST_GREATER: // TODO: tidus hair and chocobo wings only appear fully when this is tested as ZTST_GEQUAL
				// test |= zso <= zdo; // ~(zso > zdo)
				pcmpgtd(xmm0, temp2);
				pcmpeqd(temp1, temp1);
				pxor(xmm0, temp1);
				por(_test, xmm0);
				break;
			}

			alltrue(_test);
		}
	}

	/// Input[x86]: xmm4=q, xmm2=s, xmm3=t
	/// Output[x86]: xmm5=rb, xmm6=ga
	/// Output[x64]: xmm2=rb, xmm3=ga
	/// Destroys everything except xmm7[x86]
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

		// x86-64 and i386 want the output of this function in different places :(
		// Solve by remapping registers
		const Xmm
			&xtm0 = xmm0, // Temp register
			&xtm1 = xmm1, // Temp register
			&xtm2 = is64 ? xmm5 : xmm2, // Input register for s on i386
			&xtm3 = is64 ? xmm6 : xmm3, // Input register for t on i386
			&xtm4 = xmm4, // Temp register
			&xtm5 = is64 ? xmm2 : xmm5, // Output register for rb
			&xtm6 = is64 ? xmm3 : xmm6, // Output register for ga
			&xtm7 = xmm7; // Available on x64 only

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
				movd(xtm2, eax);
				pshufd(xtm1, xtm1, _MM_SHUFFLE(0, 0, 0, 0));

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
			Xmm vf = is64 ? xtm7 : xtm0;

			// GSVector4i uf = u.xxzzlh().srl16(12);

			pshuflw(xtm4, xtm2, _MM_SHUFFLE(2, 2, 0, 0));
			pshufhw(xtm4, xtm4, _MM_SHUFFLE(2, 2, 0, 0));
			psrlw(xtm4, 12);
			ONLY32(movdqa(ptr[_rip_local(temp.uf)], xtm4));

			if(m_sel.prim != GS_SPRITE_CLASS)
			{
				// GSVector4i vf = v.xxzzlh().srl16(12);

				pshuflw(vf, xtm3, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(vf, vf, _MM_SHUFFLE(2, 2, 0, 0));
				psrlw(vf, 12);
				ONLY32(movdqa(ptr[_rip_local(temp.vf)], vf));
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
			vpaddw(xtm3, xtm2, xtm0);

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
		// xtm4 = uf
		// xtm7 = used[x86] vf[x64]
		// Free: xtm0, xtm1, xtm5, xtm6

		// GSVector4i x0 = uv0.upl16();
		// GSVector4i y0 = uv0.uph16() << tw;

		pxor(xtm0, xtm0);

		vpunpcklwd(xtm5, xtm2, xtm0);
		vpunpckhwd(xtm2, xtm2, xtm0);
		pslld(xtm2, static_cast<uint8>(m_sel.tw + 3));

		// xtm0 = 0
		// xtm2 = y0
		// xtm3 = uv1 (ltf)
		// xtm4 = uf
		// xtm5 = x0
		// xtm7 = used[x86] vf[x64]
		// Free: xtm1, xtm6

		if(m_sel.ltf)
		{
			// GSVector4i x1 = uv1.upl16();
			// GSVector4i y1 = uv1.uph16() << tw;

			vpunpcklwd(xtm1, xtm3, xtm0);
			vpunpckhwd(xtm3, xtm3, xtm0);
			pslld(xtm3, static_cast<uint8>(m_sel.tw + 3));

			// xtm1 = x1
			// xtm2 = y0
			// xtm3 = y1
			// xtm4 = uf
			// xtm5 = x0
			// xtm7 = used[x86] vf[x64]
			// Free: xtm0, xtm6

			// GSVector4i addr00 = y0 + x0;
			// GSVector4i addr01 = y0 + x1;
			// GSVector4i addr10 = y1 + x0;
			// GSVector4i addr11 = y1 + x1;

			vpaddd(xtm0, xtm3, xtm1); // addr11
			vpaddd(xtm1, xtm2, xtm1); // addr01
			vpaddd(xtm2, xtm2, xtm5); // addr00
			vpaddd(xtm3, xtm3, xtm5); // addr10

			// xtm0 = addr11
			// xtm1 = addr01
			// xtm2 = addr00
			// xtm3 = addr10
			// xtm6 = uf
			// xtm7 = used[x86] vf[x64]
			// Free: xtm4, xtm5

			// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);
			// c01 = addr01.gather32_32((const uint32/uint8*)tex[, clut]);
			// c10 = addr10.gather32_32((const uint32/uint8*)tex[, clut]);
			// c11 = addr11.gather32_32((const uint32/uint8*)tex[, clut]);

			Xmm regIn[]  = { xtm0, xtm2, xtm1, xtm3 };
			Xmm regOut[] = { xtm5, xtm6, xtm0, xtm2 };

			ReadTexel(regOut, regIn, 4, 0);

			// xtm0 = c01
			// xtm2 = c10
			// xtm4 = uf
			// xtm5 = c11
			// xtm6 = c00
			// xtm7 = used[x86] vf[x64]



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
			// xtm7 = used[x86] vf[x64]

			// rb00 = rb00.lerp16_4(rb01, uf);
			// ga00 = ga00.lerp16_4(ga01, uf);

			lerp16_4(xtm0, xtm3, xtm4);
			lerp16_4(xtm1, xtm6, xtm4);

			// xtm0 = rb00
			// xtm1 = ga00
			// xtm2 = c10
			// xtm4 = uf
			// xtm5 = c11
			// xtm7 = used[x86] vf[x64]

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
			// xtm7 = used[x86] vf[x64]

			// rb10 = rb10.lerp16_4(rb11, uf);
			// ga10 = ga10.lerp16_4(ga11, uf);

			lerp16_4(xtm5, xtm2, xtm4);
			lerp16_4(xtm6, xtm3, xtm4);

			// xtm0 = rb00
			// xtm1 = ga00
			// xtm5 = rb10
			// xtm6 = ga10
			// xtm7 = used[x86] vf[x64]

			// rb00 = rb00.lerp16_4(rb10, vf);
			// ga00 = ga00.lerp16_4(ga10, vf);

			Xmm vf = is64 ? xtm7 : xtm6;
			ONLY32(movdqa(vf, ptr[_rip_local(temp.vf)]));

			lerp16_4(xtm5, xtm0, vf);
			lerp16_4(xtm6, xtm1, vf);
		}
		else
		{
			// GSVector4i addr00 = y0 + x0;

			vpaddd(xtm2, xtm2, xtm5);

			// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);

			ReadTexel(&xtm5, &xtm2, 1, 0);

			// GSVector4i mask = GSVector4i::x00ff();

			// c[0] = c00 & mask;
			// c[1] = (c00 >> 8) & mask;

			split16_2x8(xtm5, xtm6, xtm5);
		}

		// xtm5 = rb (xmm5[x86], xmm2[x64])
		// xtm6 = ga (xmm6[x86], xmm3[x64])
	}

	void Wrap(const Xmm& uv)
	{
		// Registers free from SampleTexture
		const Xmm
			&mask = xmm0,
			&min = xmm1,
			&max = is64 ? xmm2 : xmm5,
			&tmp = is64 ? xmm3 : xmm6;

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					pmaxsw(uv, ptr[_rip_global(t.min)]);
				}
				else
				{
					pxor(tmp, tmp);
					pmaxsw(uv, tmp);
				}

				pminsw(uv, ptr[_rip_global(t.max)]);
			}
			else
			{
				pand(uv, ptr[_rip_global(t.min)]);

				if(region)
				{
					por(uv, ptr[_rip_global(t.max)]);
				}
			}
		}
		else
		{
			movdqa(min, ptr[_rip_global(t.min)]);
			movdqa(max, ptr[_rip_global(t.max)]);
			movdqa(mask, ptr[_rip_global(t.mask)]);

			// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;
			vpand(tmp, uv, min);
			if(region)
				por(tmp, max);
			// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);
			pmaxsw(uv, min);
			pminsw(uv, max);
			// clamp.blend8(repeat, m_local.gd->t.mask);
			pblendvb(uv, tmp /*, xmm0==mask */);
		}
	}

	void Wrap(const Xmm& uv0, const Xmm& uv1)
	{
		// Registers free from SampleTexture
		const Xmm
			&mask = xmm0,
			&min = xmm1,
			&max = is64 ? xmm2 : xmm5,
			&tmp = is64 ? xmm3 : xmm6;

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					movdqa(min, ptr[_rip_global(t.min)]);
					pmaxsw(uv0, min);
					pmaxsw(uv1, min);
				}
				else
				{
					pxor(tmp, tmp);
					pmaxsw(uv0, tmp);
					pmaxsw(uv1, tmp);
				}

				movdqa(max, ptr[_rip_global(t.max)]);
				pminsw(uv0, max);
				pminsw(uv1, max);
			}
			else
			{
				movdqa(min, ptr[_rip_global(t.min)]);
				pand(uv0, min);
				pand(uv1, min);

				if(region)
				{
					movdqa(max, ptr[_rip_global(t.max)]);
					por(uv0, max);
					por(uv1, max);
				}
			}
		}
		else
		{
			movdqa(min, ptr[_rip_global(t.min)]);
			movdqa(max, ptr[_rip_global(t.max)]);
			movdqa(mask, ptr[_rip_global(t.mask)]);

			for (const Xmm& uv : {uv0, uv1})
			{
				// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;
				vpand(tmp, uv, min);
				if(region)
					por(tmp, max);
				// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);
				pmaxsw(uv, min);
				pminsw(uv, max);
				// clamp.blend8(repeat, m_local.gd->t.mask);
				pblendvb(uv, tmp /*, xmm0==mask*/);
			}
		}
	}

	void SampleTextureLOD()
	{
		// TODO: x64
		if(!m_sel.fb || m_sel.tfx == TFX_NONE)
		{
			return;
		}

		push(ebp);

		mov(ebp, (size_t)m_local.gd->tex);

		if(m_sel.tlu)
		{
			mov(edx, ptr[&m_local.gd->clut]);
		}

		if(!m_sel.fst)
		{
			rcpps(xmm0, xmm4);

			mulps(xmm2, xmm0);
			mulps(xmm3, xmm0);

			cvttps2dq(xmm2, xmm2);
			cvttps2dq(xmm3, xmm3);
		}

		// xmm2 = u
		// xmm3 = v
		// xmm4 = q
		// xmm0 = xmm1 = xmm5 = xmm6 = free

		// TODO: if the fractional part is not needed in round-off mode then there is a faster integer log2 (just take the exp) (but can we round it?)

		if(!m_sel.lcm)
		{
			// lod = -log2(Q) * (1 << L) + K

			pcmpeqd(xmm1, xmm1);
			vpsrld(xmm1, xmm1, 25);
			vpslld(xmm0, xmm4, 1);
			vpsrld(xmm0, xmm0, 24);
			psubd(xmm0, xmm1);
			cvtdq2ps(xmm0, xmm0);

			// xmm0 = (float)(exp(q) - 127)

			vpslld(xmm4, xmm4, 9);
			vpsrld(xmm4, xmm4, 9);
			orps(xmm4, ptr[g_const->m_log2_coef_128b[3]]);

			// xmm4 = mant(q) | 1.0f

			if(hasFMA)
			{
				movaps(xmm5, ptr[g_const->m_log2_coef_128b[0]]); // c0
				vfmadd213ps(xmm5, xmm4, ptr[g_const->m_log2_coef_128b[1]]); // c0 * xmm4 + c1
				vfmadd213ps(xmm5, xmm4, ptr[g_const->m_log2_coef_128b[2]]); // (c0 * xmm4 + c1) * xmm4 + c2
				subps(xmm4, ptr[g_const->m_log2_coef_128b[3]]); // xmm4 - 1.0f
				vfmadd213ps(xmm4, xmm5, xmm0); // ((c0 * xmm4 + c1) * xmm4 + c2) * (xmm4 - 1.0f) + xmm0
			}
			else
			{
				vmulps(xmm5, xmm4, ptr[g_const->m_log2_coef_128b[0]]);
				addps(xmm5, ptr[g_const->m_log2_coef_128b[1]]);
				mulps(xmm5, xmm4);
				subps(xmm4, ptr[g_const->m_log2_coef_128b[3]]);
				addps(xmm5, ptr[g_const->m_log2_coef_128b[2]]);
				mulps(xmm4, xmm5);
				addps(xmm4, xmm0);
			}

			// xmm4 = log2(Q) = ((((c0 * xmm4) + c1) * xmm4) + c2) * (xmm4 - 1.0f) + xmm0

			if(hasFMA)
			{
				movaps(xmm5, ptr[&m_local.gd->l]);
				vfmadd213ps(xmm4, xmm5, ptr[&m_local.gd->k]);
			}
			else
			{
				mulps(xmm4, ptr[&m_local.gd->l]);
				addps(xmm4, ptr[&m_local.gd->k]);
			}

			// xmm4 = (-log2(Q) * (1 << L) + K) * 0x10000

			xorps(xmm0, xmm0);
			minps(xmm4, ptr[&m_local.gd->mxl]);
			maxps(xmm4, xmm0);
			cvtps2dq(xmm4, xmm4);

			if(m_sel.mmin == 1) // round-off mode
			{
				mov(eax, 0x8000);
				movd(xmm0, eax);
				pshufd(xmm0, xmm0, _MM_SHUFFLE(0, 0, 0, 0));
				paddd(xmm4, xmm0);
			}

			vpsrld(xmm0, xmm4, 16);

			movdqa(ptr[&m_local.temp.lod.i], xmm0);
			/*
			 vpslld(xmm5, xmm0, 6);
			 vpslld(xmm6, xmm4, 16);
			 vpsrld(xmm6, xmm6, 24);
			 return;
			 */
			if(m_sel.mmin == 2) // trilinear mode
			{
				pshuflw(xmm1, xmm4, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(xmm1, xmm1, _MM_SHUFFLE(2, 2, 0, 0));
				movdqa(ptr[&m_local.temp.lod.f], xmm1);
			}

			// shift u/v/minmax by (int)lod

			if(TargetVec::hasAVX2)
			{
				vpsravd(xmm2, xmm2, xmm0);
				vpsravd(xmm3, xmm3, xmm0);

				movdqa(ptr[&m_local.temp.uv[0]], xmm2);
				movdqa(ptr[&m_local.temp.uv[1]], xmm3);

				// m_local.gd->t.minmax => m_local.temp.uv_minmax[0/1]

				pxor(xmm1, xmm1);

				movdqa(xmm4, ptr[&m_local.gd->t.min]);
				vpunpcklwd(xmm5, xmm4, xmm1); // minu
				vpunpckhwd(xmm6, xmm4, xmm1); // minv
				vpsrlvd(xmm5, xmm5, xmm0);
				vpsrlvd(xmm6, xmm6, xmm0);
				packusdw(xmm5, xmm6);

				movdqa(xmm4, ptr[&m_local.gd->t.max]);
				vpunpcklwd(xmm6, xmm4, xmm1); // maxu
				vpunpckhwd(xmm4, xmm4, xmm1); // maxv
				vpsrlvd(xmm6, xmm6, xmm0);
				vpsrlvd(xmm4, xmm4, xmm0);
				packusdw(xmm6, xmm4);

				movdqa(ptr[&m_local.temp.uv_minmax[0]], xmm5);
				movdqa(ptr[&m_local.temp.uv_minmax[1]], xmm6);
			}
			else
			{
				movq(xmm4, ptr[&m_local.gd->t.minmax]);

				vpunpckldq(xmm5, xmm2, xmm3);
				vpunpckhdq(xmm6, xmm2, xmm3);
				movdqa(xmm2, xmm5);
				movdqa(xmm3, xmm6);

				movd(xmm0, ptr[&m_local.temp.lod.i.u32[0]]);
				psrad(xmm2, xmm0);
				vpsrlw(xmm1, xmm4, xmm0);
				movq(ptr[&m_local.temp.uv_minmax[0].u32[0]], xmm1);

				movd(xmm0, ptr[&m_local.temp.lod.i.u32[1]]);
				psrad(xmm5, xmm0);
				vpsrlw(xmm1, xmm4, xmm0);
				movq(ptr[&m_local.temp.uv_minmax[1].u32[0]], xmm1);

				movd(xmm0, ptr[&m_local.temp.lod.i.u32[2]]);
				psrad(xmm3, xmm0);
				vpsrlw(xmm1, xmm4, xmm0);
				movq(ptr[&m_local.temp.uv_minmax[0].u32[2]], xmm1);

				movd(xmm0, ptr[&m_local.temp.lod.i.u32[3]]);
				psrad(xmm6, xmm0);
				vpsrlw(xmm1, xmm4, xmm0);
				movq(ptr[&m_local.temp.uv_minmax[1].u32[2]], xmm1);

				punpckldq(xmm2, xmm3);
				punpckhdq(xmm5, xmm6);
				vpunpckhdq(xmm3, xmm2, xmm5);
				punpckldq(xmm2, xmm5);

				movdqa(ptr[&m_local.temp.uv[0]], xmm2);
				movdqa(ptr[&m_local.temp.uv[1]], xmm3);

				movdqa(xmm5, ptr[&m_local.temp.uv_minmax[0]]);
				movdqa(xmm6, ptr[&m_local.temp.uv_minmax[1]]);

				vpunpcklwd(xmm0, xmm5, xmm6);
				vpunpckhwd(xmm1, xmm5, xmm6);
				vpunpckldq(xmm5, xmm0, xmm1);
				vpunpckhdq(xmm6, xmm0, xmm1);

				movdqa(ptr[&m_local.temp.uv_minmax[0]], xmm5);
				movdqa(ptr[&m_local.temp.uv_minmax[1]], xmm6);
			}
		}
		else
		{
			// lod = K

			movd(xmm0, ptr[&m_local.gd->lod.i.u32[0]]);

			psrad(xmm2, xmm0);
			psrad(xmm3, xmm0);

			movdqa(ptr[&m_local.temp.uv[0]], xmm2);
			movdqa(ptr[&m_local.temp.uv[1]], xmm3);

			movdqa(xmm5, ptr[&m_local.temp.uv_minmax[0]]);
			movdqa(xmm6, ptr[&m_local.temp.uv_minmax[1]]);
		}

		// xmm2 = m_local.temp.uv[0] = u (level m)
		// xmm3 = m_local.temp.uv[1] = v (level m)
		// xmm5 = minuv
		// xmm6 = maxuv

		if(m_sel.ltf)
		{
			// u -= 0x8000;
			// v -= 0x8000;

			mov(eax, 0x8000);
			movd(xmm4, eax);
			pshufd(xmm4, xmm4, _MM_SHUFFLE(0, 0, 0, 0));

			psubd(xmm2, xmm4);
			psubd(xmm3, xmm4);

			// GSVector4i uf = u.xxzzlh().srl16(1);

			pshuflw(xmm0, xmm2, _MM_SHUFFLE(2, 2, 0, 0));
			pshufhw(xmm0, xmm0, _MM_SHUFFLE(2, 2, 0, 0));
			psrlw(xmm0, 12);
			movdqa(ptr[&m_local.temp.uf], xmm0);

			// GSVector4i vf = v.xxzzlh().srl16(1);

			pshuflw(xmm0, xmm3, _MM_SHUFFLE(2, 2, 0, 0));
			pshufhw(xmm0, xmm0, _MM_SHUFFLE(2, 2, 0, 0));
			psrlw(xmm0, 12);
			movdqa(ptr[&m_local.temp.vf], xmm0);
		}

		// GSVector4i uv0 = u.sra32(16).ps32(v.sra32(16));

		psrad(xmm2, 16);
		psrad(xmm3, 16);
		packssdw(xmm2, xmm3);

		if(m_sel.ltf)
		{
			// GSVector4i uv1 = uv0.add16(GSVector4i::x0001());

			pcmpeqd(xmm1, xmm1);
			psrlw(xmm1, 15);
			vpaddw(xmm3, xmm2, xmm1);

			// uv0 = Wrap(uv0);
			// uv1 = Wrap(uv1);

			WrapLOD(xmm2, xmm3);
		}
		else
		{
			// uv0 = Wrap(uv0);

			WrapLOD(xmm2);
		}

		// xmm2 = uv0
		// xmm3 = uv1 (ltf)
		// xmm0, xmm1, xmm4, xmm5, xmm6 = free
		// xmm7 = used

		// GSVector4i x0 = uv0.upl16();
		// GSVector4i y0 = uv0.uph16() << tw;

		pxor(xmm0, xmm0);

		vpunpcklwd(xmm4, xmm2, xmm0);
		vpunpckhwd(xmm2, xmm2, xmm0);
		pslld(xmm2, static_cast<uint8>(m_sel.tw + 3));

		// xmm0 = 0
		// xmm2 = y0
		// xmm3 = uv1 (ltf)
		// xmm4 = x0
		// xmm1, xmm5, xmm6 = free
		// xmm7 = used

		if(m_sel.ltf)
		{
			// GSVector4i x1 = uv1.upl16();
			// GSVector4i y1 = uv1.uph16() << tw;

			vpunpcklwd(xmm6, xmm3, xmm0);
			vpunpckhwd(xmm3, xmm3, xmm0);
			pslld(xmm3, static_cast<uint8>(m_sel.tw + 3));

			// xmm2 = y0
			// xmm3 = y1
			// xmm4 = x0
			// xmm6 = x1
			// xmm0, xmm5, xmm6 = free
			// xmm7 = used

			// GSVector4i addr00 = y0 + x0;
			// GSVector4i addr01 = y0 + x1;
			// GSVector4i addr10 = y1 + x0;
			// GSVector4i addr11 = y1 + x1;

			vpaddd(xmm5, xmm2, xmm4);
			vpaddd(xmm2, xmm2, xmm6);
			vpaddd(xmm0, xmm3, xmm4);
			vpaddd(xmm3, xmm3, xmm6);

			// xmm5 = addr00
			// xmm2 = addr01
			// xmm0 = addr10
			// xmm3 = addr11
			// xmm1, xmm4, xmm6 = free
			// xmm7 = used

			// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);
			// c01 = addr01.gather32_32((const uint32/uint8*)tex[, clut]);
			// c10 = addr10.gather32_32((const uint32/uint8*)tex[, clut]);
			// c11 = addr11.gather32_32((const uint32/uint8*)tex[, clut]);

			const Xmm regSrc[] = { xmm5, xmm2, xmm0, xmm3 };
			const Xmm regDst[] = { xmm6, xmm4, xmm1, xmm5 };
			ReadTexel(regDst, regSrc, 4, 0);

			// xmm6 = c00
			// xmm4 = c01
			// xmm1 = c10
			// xmm5 = c11
			// xmm0, xmm2, xmm3 = free
			// xmm7 = used

			movdqa(xmm0, ptr[&m_local.temp.uf]);

			// GSVector4i rb00 = c00 & mask;
			// GSVector4i ga00 = (c00 >> 8) & mask;

			split16_2x8(xmm2, xmm6, xmm6);

			// GSVector4i rb01 = c01 & mask;
			// GSVector4i ga01 = (c01 >> 8) & mask;

			split16_2x8(xmm3, xmm4, xmm4);

			// xmm0 = uf
			// xmm2 = rb00
			// xmm3 = rb01
			// xmm6 = ga00
			// xmm4 = ga01
			// xmm1 = c10
			// xmm5 = c11
			// xmm7 = used

			// rb00 = rb00.lerp16_4(rb01, uf);
			// ga00 = ga00.lerp16_4(ga01, uf);

			lerp16_4(xmm3, xmm2, xmm0);
			lerp16_4(xmm4, xmm6, xmm0);

			// xmm0 = uf
			// xmm3 = rb00
			// xmm4 = ga00
			// xmm1 = c10
			// xmm5 = c11
			// xmm2, xmm6 = free
			// xmm7 = used

			// GSVector4i rb10 = c10 & mask;
			// GSVector4i ga10 = (c10 >> 8) & mask;

			split16_2x8(xmm1, xmm2, xmm1);

			// GSVector4i rb11 = c11 & mask;
			// GSVector4i ga11 = (c11 >> 8) & mask;

			split16_2x8(xmm5, xmm6, xmm5);

			// xmm0 = uf
			// xmm3 = rb00
			// xmm4 = ga00
			// xmm1 = rb10
			// xmm5 = rb11
			// xmm2 = ga10
			// xmm6 = ga11
			// xmm7 = used

			// rb10 = rb10.lerp16_4(rb11, uf);
			// ga10 = ga10.lerp16_4(ga11, uf);

			lerp16_4(xmm5, xmm1, xmm0);
			lerp16_4(xmm6, xmm2, xmm0);

			// xmm3 = rb00
			// xmm4 = ga00
			// xmm5 = rb10
			// xmm6 = ga10
			// xmm0, xmm1, xmm2 = free
			// xmm7 = used

			// rb00 = rb00.lerp16_4(rb10, vf);
			// ga00 = ga00.lerp16_4(ga10, vf);

			movdqa(xmm0, ptr[&m_local.temp.vf]);

			lerp16_4(xmm5, xmm3, xmm0);
			lerp16_4(xmm6, xmm4, xmm0);
		}
		else
		{
			// GSVector4i addr00 = y0 + x0;

			vpaddd(xmm5, xmm2, xmm4);

			// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);

			ReadTexel(&xmm6, &xmm5, 1, 0);

			// GSVector4i mask = GSVector4i::x00ff();

			// c[0] = c00 & mask;
			// c[1] = (c00 >> 8) & mask;

			split16_2x8(xmm5, xmm6, xmm6);
		}

		if(m_sel.mmin != 1) // !round-off mode
		{
			movdqa(ptr[&m_local.temp.trb], xmm5);
			movdqa(ptr[&m_local.temp.tga], xmm6);

			movdqa(xmm2, ptr[&m_local.temp.uv[0]]);
			movdqa(xmm3, ptr[&m_local.temp.uv[1]]);

			psrad(xmm2, 1);
			psrad(xmm3, 1);

			movdqa(xmm5, ptr[&m_local.temp.uv_minmax[0]]);
			movdqa(xmm6, ptr[&m_local.temp.uv_minmax[1]]);

			psrlw(xmm5, 1);
			psrlw(xmm6, 1);

			if(m_sel.ltf)
			{
				// u -= 0x8000;
				// v -= 0x8000;

				mov(eax, 0x8000);
				movd(xmm4, eax);
				pshufd(xmm4, xmm4, _MM_SHUFFLE(0, 0, 0, 0));

				psubd(xmm2, xmm4);
				psubd(xmm3, xmm4);

				// GSVector4i uf = u.xxzzlh().srl16(1);

				pshuflw(xmm0, xmm2, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(xmm0, xmm0, _MM_SHUFFLE(2, 2, 0, 0));
				psrlw(xmm0, 12);
				movdqa(ptr[&m_local.temp.uf], xmm0);

				// GSVector4i vf = v.xxzzlh().srl16(1);

				pshuflw(xmm0, xmm3, _MM_SHUFFLE(2, 2, 0, 0));
				pshufhw(xmm0, xmm0, _MM_SHUFFLE(2, 2, 0, 0));
				psrlw(xmm0, 12);
				movdqa(ptr[&m_local.temp.vf], xmm0);
			}

			// GSVector4i uv0 = u.sra32(16).ps32(v.sra32(16));

			psrad(xmm2, 16);
			psrad(xmm3, 16);
			packssdw(xmm2, xmm3);

			if(m_sel.ltf)
			{
				// GSVector4i uv1 = uv0.add16(GSVector4i::x0001());

				pcmpeqd(xmm1, xmm1);
				psrlw(xmm1, 15);
				vpaddw(xmm3, xmm2, xmm1);

				// uv0 = Wrap(uv0);
				// uv1 = Wrap(uv1);

				WrapLOD(xmm2, xmm3);
			}
			else
			{
				// uv0 = Wrap(uv0);

				WrapLOD(xmm2);
			}

			// xmm2 = uv0
			// xmm3 = uv1 (ltf)
			// xmm0, xmm1, xmm4, xmm5, xmm6 = free
			// xmm7 = used

			// GSVector4i x0 = uv0.upl16();
			// GSVector4i y0 = uv0.uph16() << tw;

			pxor(xmm0, xmm0);

			vpunpcklwd(xmm4, xmm2, xmm0);
			vpunpckhwd(xmm2, xmm2, xmm0);
			pslld(xmm2, static_cast<uint8>(m_sel.tw + 3));

			// xmm0 = 0
			// xmm2 = y0
			// xmm3 = uv1 (ltf)
			// xmm4 = x0
			// xmm1, xmm5, xmm6 = free
			// xmm7 = used

			if(m_sel.ltf)
			{
				// GSVector4i x1 = uv1.upl16();
				// GSVector4i y1 = uv1.uph16() << tw;

				vpunpcklwd(xmm6, xmm3, xmm0);
				vpunpckhwd(xmm3, xmm3, xmm0);
				pslld(xmm3, static_cast<uint8>(m_sel.tw + 3));

				// xmm2 = y0
				// xmm3 = y1
				// xmm4 = x0
				// xmm6 = x1
				// xmm0, xmm5, xmm6 = free
				// xmm7 = used

				// GSVector4i addr00 = y0 + x0;
				// GSVector4i addr01 = y0 + x1;
				// GSVector4i addr10 = y1 + x0;
				// GSVector4i addr11 = y1 + x1;

				vpaddd(xmm5, xmm2, xmm4);
				vpaddd(xmm2, xmm2, xmm6);
				vpaddd(xmm0, xmm3, xmm4);
				vpaddd(xmm3, xmm3, xmm6);

				// xmm5 = addr00
				// xmm2 = addr01
				// xmm0 = addr10
				// xmm3 = addr11
				// xmm1, xmm4, xmm6 = free
				// xmm7 = used

				// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);
				// c01 = addr01.gather32_32((const uint32/uint8*)tex[, clut]);
				// c10 = addr10.gather32_32((const uint32/uint8*)tex[, clut]);
				// c11 = addr11.gather32_32((const uint32/uint8*)tex[, clut]);

				const Xmm regSrc[] = { xmm5, xmm2, xmm0, xmm3 };
				const Xmm regDst[] = { xmm6, xmm4, xmm1, xmm5 };

				ReadTexel(regDst, regSrc, 4, 1);

				// xmm6 = c00
				// xmm4 = c01
				// xmm1 = c10
				// xmm5 = c11
				// xmm0, xmm2, xmm3 = free
				// xmm7 = used

				movdqa(xmm0, ptr[&m_local.temp.uf]);

				// GSVector4i rb00 = c00 & mask;
				// GSVector4i ga00 = (c00 >> 8) & mask;

				split16_2x8(xmm2, xmm6, xmm6);

				// GSVector4i rb01 = c01 & mask;
				// GSVector4i ga01 = (c01 >> 8) & mask;

				split16_2x8(xmm3, xmm4, xmm4);

				// xmm0 = uf
				// xmm2 = rb00
				// xmm3 = rb01
				// xmm6 = ga00
				// xmm4 = ga01
				// xmm1 = c10
				// xmm5 = c11
				// xmm7 = used

				// rb00 = rb00.lerp16_4(rb01, uf);
				// ga00 = ga00.lerp16_4(ga01, uf);

				lerp16_4(xmm3, xmm2, xmm0);
				lerp16_4(xmm4, xmm6, xmm0);

				// xmm0 = uf
				// xmm3 = rb00
				// xmm4 = ga00
				// xmm1 = c10
				// xmm5 = c11
				// xmm2, xmm6 = free
				// xmm7 = used

				// GSVector4i rb10 = c10 & mask;
				// GSVector4i ga10 = (c10 >> 8) & mask;

				split16_2x8(xmm1, xmm2, xmm1);

				// GSVector4i rb11 = c11 & mask;
				// GSVector4i ga11 = (c11 >> 8) & mask;

				split16_2x8(xmm5, xmm6, xmm5);

				// xmm0 = uf
				// xmm3 = rb00
				// xmm4 = ga00
				// xmm1 = rb10
				// xmm5 = rb11
				// xmm2 = ga10
				// xmm6 = ga11
				// xmm7 = used

				// rb10 = rb10.lerp16_4(rb11, uf);
				// ga10 = ga10.lerp16_4(ga11, uf);

				lerp16_4(xmm5, xmm1, xmm0);
				lerp16_4(xmm6, xmm2, xmm0);

				// xmm3 = rb00
				// xmm4 = ga00
				// xmm5 = rb10
				// xmm6 = ga10
				// xmm0, xmm1, xmm2 = free
				// xmm7 = used

				// rb00 = rb00.lerp16_4(rb10, vf);
				// ga00 = ga00.lerp16_4(ga10, vf);

				movdqa(xmm0, ptr[&m_local.temp.vf]);

				lerp16_4(xmm5, xmm3, xmm0);
				lerp16_4(xmm6, xmm4, xmm0);
			}
			else
			{
				// GSVector4i addr00 = y0 + x0;

				vpaddd(xmm5, xmm2, xmm4);

				// c00 = addr00.gather32_32((const uint32/uint8*)tex[, clut]);

				ReadTexel(&xmm6, &xmm5, 1, 1);

				// GSVector4i mask = GSVector4i::x00ff();

				// c[0] = c00 & mask;
				// c[1] = (c00 >> 8) & mask;

				split16_2x8(xmm5, xmm6, xmm6);
			}

			movdqa(xmm0, ptr[m_sel.lcm ? &m_local.gd->lod.f : &m_local.temp.lod.f]);
			vpsrlw(xmm0, xmm0, 1);

			movdqa(xmm2, ptr[&m_local.temp.trb]);
			movdqa(xmm3, ptr[&m_local.temp.tga]);

			lerp16(xmm5, xmm2, xmm0, 0);
			lerp16(xmm6, xmm3, xmm0, 0);
		}

		pop(ebp);
	}

	void WrapLOD(const Xmm& uv)
	{
		// xmm5 = minuv
		// xmm6 = maxuv
		// xmm0, xmm1, xmm4 = free

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					pmaxsw(uv, xmm5);
				}
				else
				{
					pxor(xmm0, xmm0);
					pmaxsw(uv, xmm0);
				}

				pminsw(uv, xmm6);
			}
			else
			{
				pand(uv, xmm5);

				if(region)
				{
					por(uv, xmm6);
				}
			}
		}
		else
		{
			movdqa(xmm0, ptr[&m_local.gd->t.mask]);

			// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;

			vpand(xmm1, uv, xmm5);

			if(region)
			{
				por(xmm1, xmm6);
			}

			// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);

			pmaxsw(uv, xmm5);
			pminsw(uv, xmm6);

			// clamp.blend8(repeat, m_local.gd->t.mask);

			pblendvb(uv, xmm1 /*, xmm0 */);
		}
	}

	void WrapLOD(const Xmm& uv0, const Xmm& uv1)
	{
		// xmm5 = minuv
		// xmm6 = maxuv
		// xmm0, xmm1, xmm4 = free

		int wms_clamp = ((m_sel.wms + 1) >> 1) & 1;
		int wmt_clamp = ((m_sel.wmt + 1) >> 1) & 1;

		int region = ((m_sel.wms | m_sel.wmt) >> 1) & 1;

		if(wms_clamp == wmt_clamp)
		{
			if(wms_clamp)
			{
				if(region)
				{
					pmaxsw(uv0, xmm5);
					pmaxsw(uv1, xmm5);
				}
				else
				{
					pxor(xmm0, xmm0);
					pmaxsw(uv0, xmm0);
					pmaxsw(uv1, xmm0);
				}

				pminsw(uv0, xmm6);
				pminsw(uv1, xmm6);
			}
			else
			{
				pand(uv0, xmm5);
				pand(uv1, xmm5);

				if(region)
				{
					por(uv0, xmm6);
					por(uv1, xmm6);
				}
			}
		}
		else
		{
			movdqa(xmm0, ptr[&m_local.gd->t.mask]);

			// uv0

			// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;

			vpand(xmm1, uv0, xmm5);

			if(region)
			{
				por(xmm1, xmm6);
			}

			// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);

			pmaxsw(uv0, xmm5);
			pminsw(uv0, xmm6);

			// clamp.blend8(repeat, m_local.gd->t.mask);*

			pblendvb(uv0, xmm1 /*, xmm0 */);

			// uv1

			// GSVector4i repeat = (t & m_local.gd->t.min) | m_local.gd->t.max;

			vpand(xmm1, uv1, xmm5);

			if(region)
			{
				por(xmm1, xmm6);
			}

			// GSVector4i clamp = t.sat_i16(m_local.gd->t.min, m_local.gd->t.max);

			pmaxsw(uv1, xmm5);
			pminsw(uv1, xmm6);

			// clamp.blend8(repeat, m_local.gd->t.mask);

			pblendvb(uv1, xmm1 /*, xmm0 */);
		}
	}

	/// Output: xmm6[x86]=ga, _ga[x64]=ga
	/// Destroys[x86]: xmm3, xmm4
	/// Destroys[x64]: xmm0, xmm1
	void AlphaTFX()
	{
		if(!m_sel.fb)
		{
			return;
		}

		const Xmm& ga    = is64 ? _ga   : xmm6;
		const Xmm& f_ga  = is64 ? _f_ga : xmm4;
		const Xmm& tmpga = is64 ? xmm1  : f_ga;
		const Xmm& tmp   = is64 ? xmm0  : xmm3;
		Address _32_gaptr = ptr[m_sel.iip ? _rip_local(temp.ga) : _rip_local(c.ga)];

		switch(m_sel.tfx)
		{
			case TFX_MODULATE:

				// GSVector4i ga = iip ? gaf : m_local.c.ga;

				ONLY32(movdqa(f_ga, _32_gaptr));

				// gat = gat.modulate16<1>(ga).clamp8();

				modulate16(ga, f_ga, 1);

				clamp16(ga, tmp);

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
				ONLY32(movdqa(xmm2, f_ga)); // WHY

				// gat = gat.mix16(!tcc ? ga.srl16(7) : gat.addus8(ga.srl16(7)));

				MOVE_IF_64(psrlw, tmpga, f_ga, 7);

				if(m_sel.tcc)
				{
					paddusb(tmpga, ga);
				}

				mix16(ga, tmpga, tmp);

				break;

			case TFX_HIGHLIGHT2:

				// if(!tcc) gat = gat.mix16(ga.srl16(7));

				if(!m_sel.tcc)
				{
					// GSVector4i ga = iip ? gaf : m_local.c.ga;

					ONLY32(movdqa(f_ga, _32_gaptr));
					ONLY32(movdqa(xmm2, f_ga));

					MOVE_IF_64(psrlw, tmpga, f_ga, 7);

					mix16(ga, tmpga, tmp);
				}

				break;

			case TFX_NONE:

				// gat = iip ? ga.srl16(7) : ga;

				if(m_sel.iip)
				{
					MOVE_IF_64(psrlw, ga, f_ga, 7);
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
					movdqa(xmm0, ptr[_rip_local(temp.cov)]);
//#else
//					movdqa(xmm0, ptr[rsp + _rz_cov]);
//#endif
				}
				else
				{
					pcmpeqd(xmm0, xmm0);
					psllw(xmm0, 15);
					psrlw(xmm0, 8);
				}

				mix16(ga, xmm0, xmm1);
			}
			else
			{
				// a = a == 0x80 ? cov : a

				pcmpeqd(xmm0, xmm0);
				psllw(xmm0, 15);
				psrlw(xmm0, 8);

				if(m_sel.edge)
				{
//#ifdef _WIN64
					movdqa(xmm1, ptr[_rip_local(temp.cov)]);
//#else
//					vmovdqa(xmm1, ptr[rsp + _rz_cov]);
//#endif
				}
				else
				{
					movdqa(xmm1, xmm0);
				}

				pcmpeqw(xmm0, ga);
				psrld(xmm0, 16);
				pslld(xmm0, 16);

				pblendvb(ga, xmm1 /*, xmm0 */);
			}
		}
	}

	/// Output[x86]: xmm3=fm, xmm4=zm
	void ReadMask()
	{
		const Xmm& fm = is64 ? _fm : xmm3;
		const Xmm& zm = is64 ? _zm : xmm4;

		if(m_sel.fwrite)
		{
			movdqa(fm, ptr[_rip_global(fm)]);
		}

		if(m_sel.zwrite)
		{
			movdqa(zm, ptr[_rip_global(zm)]);
		}
	}

	/// Input[x86]: xmm3=fm, xmm4=zm, xmm6=ga
	/// Input[x64]: _ga, _fm, _zm
	/// Destroys: xmm0, xmm1
	void TestAlpha()
	{
		const Xmm& ga = is64 ? _ga : xmm6;
		switch(m_sel.atst)
		{
			case ATST_NEVER:
				// t = GSVector4i::xffffffff();
				pcmpeqd(xmm1, xmm1);
				break;

			case ATST_ALWAYS:
				return;

			case ATST_LESS:
			case ATST_LEQUAL:
				// t = (ga >> 16) > m_local.gd->aref;
				vpsrld(xmm1, ga, 16);
				pcmpgtd(xmm1, ptr[_rip_global(aref)]);
				break;

			case ATST_EQUAL:
				// t = (ga >> 16) != m_local.gd->aref;
				vpsrld(xmm1, ga, 16);
				pcmpeqd(xmm1, ptr[_rip_global(aref)]);
				pcmpeqd(xmm0, xmm0);
				pxor(xmm1, xmm0);
				break;

			case ATST_GEQUAL:
			case ATST_GREATER:
				// t = (ga >> 16) < m_local.gd->aref;
				vpsrld(xmm0, ga, 16);
				movdqa(xmm1, ptr[_rip_global(aref)]);
				pcmpgtd(xmm1, xmm0);
				break;

			case ATST_NOTEQUAL:
				// t = (ga >> 16) == m_local.gd->aref;
				vpsrld(xmm1, ga, 16);
				pcmpeqd(xmm1, ptr[_rip_global(aref)]);
				break;
		}

		const Xmm& zm = is64 ? _zm : xmm4;
		const Xmm& fm = is64 ? _fm : xmm3;

		switch(m_sel.afail)
		{
			case AFAIL_KEEP:
				// test |= t;
				por(_test, xmm1);
				alltrue(_test);
				break;

			case AFAIL_FB_ONLY:
				// zm |= t;
				por(zm, xmm1);
				break;

			case AFAIL_ZB_ONLY:
				// fm |= t;
				por(fm, xmm1);
				break;

			case AFAIL_RGB_ONLY:
				// zm |= t;
				por(zm, xmm1);
				// fm |= t & GSVector4i::xff000000();
				psrld(xmm1, 24);
				pslld(xmm1, 24);
				por(fm, xmm1);
				break;
		}
	}

	void ColorTFX()
	{
		if(!m_sel.fwrite)
		{
			return;
		}

		const Xmm& rb    = is64 ? _rb   : xmm5;
		const Xmm& ga    = is64 ? _ga   : xmm6;
		const Xmm& f_ga  = is64 ? _f_ga : xmm2;
		const Xmm& tmpga = is64 ? xmm6  : f_ga;

		auto modulate16_1_rb = [&]{
			// GSVector4i rb = iip ? rbf : m_local.c.rb;
			if (is64)
				modulate16(rb, _f_rb, 1);
			else
				modulate16(rb, ptr[m_sel.iip ? _rip_local(temp.rb) : _rip_local(c.rb)], 1);
		};

		switch(m_sel.tfx)
		{
			case TFX_MODULATE:

				// GSVector4i rb = iip ? rbf : m_local.c.rb;

				// rbt = rbt.modulate16<1>(rb).clamp8();

				modulate16_1_rb();

				clamp16(rb, xmm0);

				break;

			case TFX_DECAL:

				break;

			case TFX_HIGHLIGHT:
			case TFX_HIGHLIGHT2:

				if(m_sel.tfx == TFX_HIGHLIGHT2 && m_sel.tcc)
				{
					// GSVector4i ga = iip ? gaf : m_local.c.ga;

					ONLY32(movdqa(f_ga, ptr[m_sel.iip ? _rip_local(temp.ga) : _rip_local(c.ga)]));
				}

				// gat = gat.modulate16<1>(ga).add16(af).clamp8().mix16(gat);

				movdqa(xmm1, ga);

				modulate16(ga, f_ga, 1);

				pshuflw(tmpga, f_ga, _MM_SHUFFLE(3, 3, 1, 1));
				pshufhw(tmpga, tmpga, _MM_SHUFFLE(3, 3, 1, 1));
				psrlw(tmpga, 7);

				paddw(ga, tmpga);

				clamp16(ga, xmm0);

				mix16(ga, xmm1, xmm0);

				// rbt = rbt.modulate16<1>(rb).add16(af).clamp8();

				modulate16_1_rb();

				paddw(rb, tmpga);

				clamp16(rb, xmm0);

				break;

			case TFX_NONE:

				// rbt = iip ? rb.srl16(7) : rb;

				if(m_sel.iip)
				{
					MOVE_IF_64(psrlw, rb, _f_rb, 7);
				}

				break;
		}
	}

	/// Input[x86] xmm5=rb, xmm6=ga
	/// Input[x64] _rb, _ga
	/// Destroys: xmm0[x86], xmm1, xmm2
	void Fog()
	{
		if(!m_sel.fwrite || !m_sel.fge)
		{
			return;
		}

		const Xmm& f   = is64 ? _f   : xmm0;
		const Xmm& tmp = is64 ? xmm0 : xmm2;
		const Xmm& rb  = is64 ? _rb  : xmm5;
		const Xmm& ga  = is64 ? _ga  : xmm6;

		// rb = m_local.gd->frb.lerp16<0>(rb, f);
		// ga = m_local.gd->fga.lerp16<0>(ga, f).mix16(ga);

		ONLY32(movdqa(f, ptr[m_sel.prim != GS_SPRITE_CLASS ? _rip_local(temp.f) : _rip_local(p.f)]));
		movdqa(xmm1, ga);

		movdqa(tmp, ptr[_rip_global(frb)]);
		lerp16(rb, tmp, f, 0);

		movdqa(tmp, ptr[_rip_global(fga)]);
		lerp16(ga, tmp, f, 0);

		mix16(ga, xmm1, f);
	}

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

		const Xmm& fd = is64 ? _fd : xmm2;
		ReadPixel(fd, rbx);
	}

	/// Input: _fd
	/// Destroys: xmm0, xmm1
	void TestDestAlpha()
	{
		if(!m_sel.date || m_sel.fpsm != 0 && m_sel.fpsm != 2)
		{
			return;
		}

		const Xmm& fd = is64 ? _fd : xmm2;

		// test |= ((fd [<< 16]) ^ m_local.gd->datm).sra32(31);

		if(m_sel.datm)
		{
			if(m_sel.fpsm == 2)
			{
				pxor(xmm0, xmm0);
				//vpsrld(xmm1, fd, 15);
				vpslld(xmm1, fd, 16);
				psrad(xmm1, 31);
				pcmpeqd(xmm1, xmm0);
			}
			else
			{
				pcmpeqd(xmm0, xmm0);
				vpxor(xmm1, fd, xmm0);
				psrad(xmm1, 31);
			}
		}
		else
		{
			if(m_sel.fpsm == 2)
			{
				vpslld(xmm1, fd, 16);
				psrad(xmm1, 31);
			}
			else
			{
				vpsrad(xmm1, fd, 31);
			}
		}

		por(_test, xmm1);

		alltrue(_test);
	}

	/// Input: _fm, _zm, _test
	/// Output: edx=???
	/// Destroys: xmm0, xmm1
	void WriteMask()
	{
		if(m_sel.notest)
		{
			return;
		}

		const Xmm& fm = is64 ? _fm : xmm3;
		const Xmm& zm = is64 ? _zm : xmm4;

		// fm |= test;
		// zm |= test;

		if(m_sel.fwrite)
		{
			por(fm, _test);
		}

		if(m_sel.zwrite)
		{
			por(zm, _test);
		}

		// int fzm = ~(fm == GSVector4i::xffffffff()).ps32(zm == GSVector4i::xffffffff()).mask();

		pcmpeqd(xmm1, xmm1);

		if(m_sel.fwrite && m_sel.zwrite)
		{
			vpcmpeqd(xmm0, xmm1, zm);
			pcmpeqd(xmm1, fm);
			packssdw(xmm1, xmm0);
		}
		else if(m_sel.fwrite)
		{
			pcmpeqd(xmm1, fm);
			packssdw(xmm1, xmm1);
		}
		else if(m_sel.zwrite)
		{
			pcmpeqd(xmm1, zm);
			packssdw(xmm1, xmm1);
		}

		pmovmskb(edx, xmm1);

		not(edx);
	}

	void WriteZBuf()
	{
		if(!m_sel.zwrite)
		{
			return;
		}

		const Xmm& zm = is64 ? _zm : xmm4;

		if (m_sel.prim != GS_SPRITE_CLASS)
//#ifdef _WIN64
			movdqa(xmm1, ptr[_rip_local(temp.zs)]);
//#else
//			movdqa(xmm1, ptr[rsp + _rz_zs]);
//#endif
		else
			movdqa(xmm1, ptr[_rip_local(p.z)]);

		if(m_sel.ztest && m_sel.zpsm < 2)
		{
			// zs = zs.blend8(zd, zm);

//#ifdef _WIN64
			vpblendvb(xmm1, xmm1, ptr[_rip_local(temp.zd)], zm);
//#else
//			pblendvb(xmm1, ptr[rsp + _rz_zd], _zm);
//#endif
		}

		// TODO: Why x86 only?
		// Clamp Z to ZPSM_FMT_MAX
		if (is32 && m_sel.zclamp)
		{
			pcmpeqd(xmm7, xmm7);
			psrld(xmm7, (uint8)((m_sel.zpsm & 0x3) * 8));
			pminsd(xmm1, xmm7);
		}

		bool fast = m_sel.ztest ? m_sel.zpsm < 2 : m_sel.zpsm == 0 && m_sel.notest;

		WritePixel(xmm1, rbp, dh, fast, m_sel.zpsm, 1);
	}

	/// Input[x86]: xmm2=fd, xmm5=rb, xmm6=ga
	/// Input[x64]: _fd, _rb, _ga
	/// Output: xmm0=rb, xmm1=ga
	/// Destroys[x86]: xmm4, xmm7
	/// Destroys[x64]: xmm5, xmm15
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

		const Xmm& _dst_rb = xmm0;
		const Xmm& _dst_ga = xmm1;
		const Xmm& fd = is64 ? _fd : xmm2;
		const Xmm& tmp1 = _test;
		const Xmm& tmp2 = is64 ? xmm5 : xmm4;

		if((m_sel.aba != m_sel.abb) && (m_sel.aba == 1 || m_sel.abb == 1 || m_sel.abc == 1) || m_sel.abd == 1)
		{
			switch(m_sel.fpsm)
			{
				case 0:
				case 1:

					// c[2] = fd & mask;
					// c[3] = (fd >> 8) & mask;

					split16_2x8(_dst_rb, _dst_ga, fd);

					break;

				case 2:

					// c[2] = ((fd & 0x7c00) << 9) | ((fd & 0x001f) << 3);
					// c[3] = ((fd & 0x8000) << 8) | ((fd & 0x03e0) >> 2);

					pcmpeqd(tmp1, tmp1);

					psrld(tmp1, 27); // 0x0000001f
					vpand(_dst_rb, fd, tmp1);
					pslld(_dst_rb, 3);

					pslld(tmp1, 10); // 0x00007c00
					vpand(tmp2, fd, tmp1);
					pslld(tmp2, 9);

					por(_dst_rb, tmp2);

					psrld(tmp1, 5); // 0x000003e0
					vpand(_dst_ga, fd, tmp1);
					psrld(_dst_ga, 2);

					psllw(tmp1, 10); // 0x00008000
					vpand(tmp2, fd, tmp1);
					pslld(tmp2, 8);

					por(_dst_ga, tmp2);

					break;
			}
		}

		const Xmm& rb = is64 ? _rb : xmm5;
		const Xmm& ga = is64 ? _ga : xmm6;

		// rb,   ga   = src rb, ga
		// xmm0, xmm1 = dst rb, ga
		// tmp1, tmp2 = free

		if(m_sel.pabe || (m_sel.aba != m_sel.abb) && (m_sel.abb == 0 || m_sel.abd == 0))
		{
			movdqa(tmp2, rb);
		}

		if(m_sel.aba != m_sel.abb)
		{
			// rb = c[aba * 2 + 0];

			switch(m_sel.aba)
			{
				case 0: break;
				case 1: movdqa(rb, _dst_rb); break;
				case 2: pxor(rb, rb); break;
			}

			// rb = rb.sub16(c[abb * 2 + 0]);

			switch(m_sel.abb)
			{
				case 0: psubw(rb, tmp2); break;
				case 1: psubw(rb, _dst_rb); break;
				case 2: break;
			}

			if(!(m_sel.fpsm == 1 && m_sel.abc == 1))
			{
				// GSVector4i a = abc < 2 ? c[abc * 2 + 1].yywwlh().sll16(7) : m_local.gd->afix;

				switch(m_sel.abc)
				{
					case 0:
					case 1:
						pshuflw(tmp1, m_sel.abc ? _dst_ga : ga, _MM_SHUFFLE(3, 3, 1, 1));
						pshufhw(tmp1, tmp1, _MM_SHUFFLE(3, 3, 1, 1));
						psllw(tmp1, 7);
						break;
					case 2:
						movdqa(tmp1, ptr[_rip_global(afix)]);
						break;
				}

				// rb = rb.modulate16<1>(a);

				modulate16(rb, tmp1, 1);
			}

			// rb = rb.add16(c[abd * 2 + 0]);

			switch(m_sel.abd)
			{
				case 0: paddw(rb, tmp2); break;
				case 1: paddw(rb, _dst_rb); break;
				case 2: break;
			}
		}
		else
		{
			// rb = c[abd * 2 + 0];

			switch(m_sel.abd)
			{
				case 0: break;
				case 1: movdqa(rb, _dst_rb); break;
				case 2: pxor(rb, rb); break;
			}
		}

		if(m_sel.pabe)
		{
			// mask = (c[1] << 8).sra32(31);

			vpslld(xmm0, ga, 8);
			psrad(xmm0, 31);

			// rb = c[0].blend8(rb, mask);

			vpblendvb(rb, tmp1, rb, xmm0);
		}

		// xmm0 = pabe mask
		// ga   = src ga
		// xmm1 = dst ga
		// rb   = rb
		// tmp1 = a
		// tmp2 = free

		movdqa(tmp2, ga);

		if(m_sel.aba != m_sel.abb)
		{
			// ga = c[aba * 2 + 1];

			switch(m_sel.aba)
			{
				case 0: break;
				case 1: movdqa(ga, _dst_ga); break;
				case 2: pxor(ga, ga); break;
			}

			// ga = ga.sub16(c[abeb * 2 + 1]);

			switch(m_sel.abb)
			{
				case 0: psubw(ga, tmp2); break;
				case 1: psubw(ga, _dst_ga); break;
				case 2: break;
			}

			if(!(m_sel.fpsm == 1 && m_sel.abc == 1))
			{
				// ga = ga.modulate16<1>(a);

				modulate16(ga, tmp1, 1);
			}

			// ga = ga.add16(c[abd * 2 + 1]);

			switch(m_sel.abd)
			{
				case 0: paddw(ga, tmp2); break;
				case 1: paddw(ga, _dst_ga); break;
				case 2: break;
			}
		}
		else
		{
			// ga = c[abd * 2 + 1];

			switch(m_sel.abd)
			{
				case 0: break;
				case 1: movdqa(ga, _dst_ga); break;
				case 2: pxor(ga, ga); break;
			}
		}

		// xmm0 = pabe mask
		// tmp2 = src ga
		// rb = rb
		// ga = ga
		// xmm1, tmp1 = free

		if(m_sel.pabe)
		{
			psrld(xmm0, 16); // zero out high words to select the source alpha in blend (so it also does mix16)

			// ga = c[1].blend8(ga, mask).mix16(c[1]);

			vpblendvb(ga, tmp2, ga, xmm0);
		}
		else
		{
			if(m_sel.fpsm != 1) // TODO: fm == 0xffxxxxxx
			{
				mix16(ga, tmp2, tmp1);
			}
		}
	}

	void WriteFrame()
	{
		if(!m_sel.fwrite)
		{
			return;
		}


		const Xmm& tmp0 = is64 ? xmm15 : xmm7;
		const Xmm& tmp1 = is64 ? xmm2 : xmm5;
		const Xmm& tmp2 = is64 ? xmm3 : xmm6;

		if(m_sel.fpsm == 2 && m_sel.dthe)
		{
			// y = (top & 3) << 5

			mov(eax, ptr[rsp + _top]);
			and(eax, 3);
			shl(eax, 5);

			// rb = rb.add16(m_global.dimx[0 + y]);
			// ga = ga.add16(m_global.dimx[1 + y]);

			add(rax, ptr[_rip_global(dimx)]);

			paddw(tmp1, ptr[rax + sizeof(GSVector4i) * 0]);
			paddw(tmp2, ptr[rax + sizeof(GSVector4i) * 1]);

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

		vpunpckhwd(tmp0, tmp1, tmp2);
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
		// xmm4 = fm
		// xmm6 = fd

		const Xmm& fm = is64 ? _fm : xmm3;
		const Xmm& fd = is64 ? _fd : xmm2;

		if(m_sel.fpsm == 2)
		{
			// TODO:TKR: Why do we randomly switch register mappings between x86 and x64?

			if (is64)
			{
				// GSVector4i rb = fs & 0x00f800f8;
				// GSVector4i ga = fs & 0x8000f800;

				mov(eax, 0x00f800f8);
				movd(xmm0, eax);
				pshufd(xmm0, xmm0, _MM_SHUFFLE(0, 0, 0, 0));

				mov(eax, 0x8000f800);
				movd(xmm1, eax);
				pshufd(xmm1, xmm1, _MM_SHUFFLE(0, 0, 0, 0));

				pand(xmm0, tmp1);
				pand(xmm1, tmp1);

				// fs = (ga >> 16) | (rb >> 9) | (ga >> 6) | (rb >> 3);

				vpsrld(tmp1, xmm0, 9);
				psrld(xmm0, 3);
				vpsrld(xmm3, xmm1, 16);
				psrld(xmm1, 6);

				por(xmm0, xmm1);
				por(tmp1, xmm3);
				por(tmp1, xmm0);
			}
			else
			{
				// GSVector4i rb = fs & 0x00f800f8;
				// GSVector4i ga = fs & 0x8000f800;

				mov(eax, 0x00f800f8);
				movd(xmm6, eax);
				pshufd(xmm6, xmm6, _MM_SHUFFLE(0, 0, 0, 0));

				mov(eax, 0x8000f800);
				movd(xmm7, eax);
				pshufd(xmm7, xmm7, _MM_SHUFFLE(0, 0, 0, 0));

				vpand(xmm4, tmp1, xmm6);
				pand(tmp1, xmm7);

				// fs = (ga >> 16) | (rb >> 9) | (ga >> 6) | (rb >> 3);

				vpsrld(xmm6, xmm4, 9);
				psrld(xmm4, 3);
				vpsrld(xmm7, tmp1, 16);
				psrld(xmm5, 6);

				por(tmp1, xmm4);
				por(xmm7, xmm6);
				por(tmp1, xmm7);
			}
		}

		if(m_sel.rfb)
		{
			// fs = fs.blend(fd, fm);

			blend(tmp1, fd, fm); // TODO: could be skipped in certain cases, depending on fpsm and fm
		}

		bool fast = m_sel.rfb ? m_sel.fpsm < 2 : m_sel.fpsm == 0 && m_sel.notest;

		WritePixel(tmp1, rbx, dl, fast, m_sel.fpsm, 0);
	}

	void ReadPixel(const Xmm& dst, const AddressReg& addr)
	{
		movq(dst, qword[_m_local__gd__vm + addr*2]);
		movhps(dst, qword[_m_local__gd__vm + addr*2 + 8*2]);
	}

	void WritePixel(const Xmm& src, const AddressReg& addr, const Reg8& mask, bool fast, int psm, int fz)
	{
		if(m_sel.notest)
		{
			if(fast)
			{
				movq(qword[_m_local__gd__vm + addr * 2], src);
				movhps(qword[_m_local__gd__vm + addr * 2 + 8 * 2], src);
			}
			else
			{
				WritePixel(src, addr, 0, psm);
				WritePixel(src, addr, 1, psm);
				WritePixel(src, addr, 2, psm);
				WritePixel(src, addr, 3, psm);
			}
		}
		else
		{
			if(fast)
			{
				// if(fzm & 0x0f) GSVector4i::storel(&vm16[addr + 0], fs);
				// if(fzm & 0xf0) GSVector4i::storeh(&vm16[addr + 8], fs);

				test(mask, 0x0f);
				je("@f");
				movq(qword[_m_local__gd__vm + addr * 2], src);
				L("@@");

				test(mask, 0xf0);
				je("@f");
				movhps(qword[_m_local__gd__vm + addr * 2 + 8 * 2], src);
				L("@@");

				// vmaskmovps?
			}
			else
			{
				// if(fzm & 0x03) WritePixel(fpsm, &vm16[addr + 0], fs.extract32<0>());
				// if(fzm & 0x0c) WritePixel(fpsm, &vm16[addr + 2], fs.extract32<1>());
				// if(fzm & 0x30) WritePixel(fpsm, &vm16[addr + 8], fs.extract32<2>());
				// if(fzm & 0xc0) WritePixel(fpsm, &vm16[addr + 10], fs.extract32<3>());

				test(mask, 0x03);
				je("@f");
				WritePixel(src, addr, 0, psm);
				L("@@");

				test(mask, 0x0c);
				je("@f");
				WritePixel(src, addr, 1, psm);
				L("@@");

				test(mask, 0x30);
				je("@f");
				WritePixel(src, addr, 2, psm);
				L("@@");

				test(mask, 0xc0);
				je("@f");
				WritePixel(src, addr, 3, psm);
				L("@@");
			}
		}
	}

	void WritePixel(const Xmm& src, const AddressReg& addr, uint8 i, int psm)
	{
		constexpr int s_offsets[4] = {0, 2, 8, 10};

		Address dst = ptr[_m_local__gd__vm + addr * 2 + s_offsets[i] * 2];

		switch(psm)
		{
			case 0:
				if(i == 0) movd(dst, src);
				else pextrd(dst, src, i);
				break;
			case 1:
				if(i == 0) movd(eax, src);
				else pextrd(eax, src, i);
				xor(eax, dst);
				and(eax, 0xffffff);
				xor(dst, eax);
				break;
			case 2:
				if(i == 0) movd(eax, src);
				else pextrw(eax, src, i * 2);
				mov(dst, ax);
				break;
		}
	}

	/// Will process `pixels` items from regIn into regOut in order
	/// Input:
	///  rbx = m_local.tex[0]  (x86 && !m_sel.mmin)
	///  a3  = m_local.tex (x86 && m_sel.mmin)
	///  a1  = m_local.clut (x86 && m_sel.tlu)
	/// Destroys: rax, xmm0
	/// Destroys a3 (!m_sel.mmin)
	void ReadTexel(const Xmm* regOut, const Xmm* regIn, int pixels, int mip_offset)
	{
		mip_offset *= wordsize;

		AddressReg texIn = is64 ? _64_m_local__gd__tex : rbp;
		RegExp lod_i = m_sel.lcm ? _rip_global(lod.i) : _rip_local(temp.lod.i);

		if (m_sel.mmin && !m_sel.lcm)
		{
			bool preserve = false;
			for (int i = 0; i < pixels; i++)
				for (int j = 0; j < pixels; j++)
					if (regIn[i] == regOut[j])
						preserve = true;
			bool texInA3 = true;
			for (int j = 0; j < 4; j++)
			{
				mov(a3.cvt32(), ptr[lod_i + offsetof(GSVector4i, u32[j])]);
				mov(a3.cvt32(), ptr[texIn + a3*wordsize + mip_offset]);

				for (int i = 0; i < pixels; i++)
				{
					ReadTexel(regOut[i], regIn[i], j, texInA3, preserve);
				}
			}
		}
		else
		{
			bool preserve = false;
			bool texInA3 = is32;

			if (m_sel.mmin && m_sel.lcm)
			{
				mov(a3.cvt32(), ptr[lod_i + offsetof(GSVector4i, u32[0])]);
				mov(a3.cvt32(), ptr[texIn + a3*wordsize + mip_offset]);
				texInA3 = true;
			}

			for (int i = 0; i < pixels; i++)
			{
				for (int j = 0; j < 4; j++)
				{
					ReadTexel(regOut[i], regIn[i], j, texInA3, preserve);
				}
			}
		}
	}

	void ReadTexel(const Xmm& dst, const Xmm& addr, uint8 i, bool texInA3, bool preserveDst)
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
