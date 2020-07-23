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
	GSDrawScanlineCodeGenerator2(Xbyak::CodeGenerator* base, void* param, uint64 key)
		: _parent(base)
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
		if (TargetVec::hasAVX)
			vpblendvb(a, a, b, xmm0);
		if (TargetVec::hasSSE41)
			pblendvb(a, b);
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
	void Generate();

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

			if (is64)
			{
				vmulps(xtm2, _s, xtm0);
				vmulps(xtm3, _t, xtm0);
			}
			else
			{
				mulps(xtm2, xtm0);
				mulps(xtm3, xtm0);
			}

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
			vpblendvb(uv, uv, tmp, mask);
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
				vpblendvb(uv, uv, tmp, mask);
			}
		}
	}

	void ReadPixel(const Xmm& dst, const AddressReg& addr)
	{
		movq(dst, qword[_m_local__gd__vm + addr*2]);
		movhps(dst, qword[_m_local__gd__vm + addr*2 + 8*2]);
	}

	/// Will move from regIn into regOut *in order*
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
