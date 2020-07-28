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
#include "xbyak/xbyak_smart.h"

#undef a0
#undef a1
#undef a2
#undef a3
#undef t0
#undef t1

#if _M_SSE >= 0x501
# define DRAW_SCANLINE_VECTOR_REGISTER Xbyak::Ymm
# define DRAW_SCANLINE_USING_XMM 0
# define DRAW_SCANLINE_USING_YMM 1
#else
# define DRAW_SCANLINE_VECTOR_REGISTER Xbyak::Xmm
# define DRAW_SCANLINE_USING_XMM 1
# define DRAW_SCANLINE_USING_YMM 0
#endif

class GSDrawScanlineCodeGenerator2 : public Xbyak::SmartCodeGenerator
{
	using _parent = Xbyak::SmartCodeGenerator;
	using XYm = DRAW_SCANLINE_VECTOR_REGISTER;

	using Xmm = Xbyak::Xmm;
	using Ymm = Xbyak::Ymm;

	/// On x86-64 we reserve a bunch of GPRs for holding addresses of locals that would otherwise be hard to reach
	/// On x86-32 the same values are just raw 32-bit addresses
	using LocalAddr = Choose3264<size_t, AddressReg>::type;

	constexpr static bool isXmm = std::is_same<XYm, Xbyak::Xmm>::value;
	constexpr static bool isYmm = std::is_same<XYm, Xbyak::Ymm>::value;
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
	bool use_lod;

	const XYm xym0, xym1, xym2, xym3, xym4, xym5, xym6, xym7, xym8, xym9, xym10, xym11, xym12, xym13, xym14, xym15;
	/// Note: a2 is only available on x86-64
	const AddressReg a0, a1, a2, a3, t0, t1;
	const LocalAddr _g_const, _m_local, _m_local__gd, _m_local__gd__vm;
	/// Available on both x86 and x64, not always valid
	const XYm _rb, _ga, _fm, _zm, _fd, _test;
	/// Always valid if needed, x64 only
	const XYm _z, _f, _s, _t, _q, _f_rb, _f_ga;

	/// Returns the first arg on 32-bit, second on 64-bit
	static LocalAddr chooseLocal(const void *addr32, AddressReg reg64)
	{
		return choose3264((size_t)addr32, reg64);
	}

public:
	GSDrawScanlineCodeGenerator2(Xbyak::CodeGenerator* base, Xbyak::SSEVersion::SSEVersion sseVersion, bool hasFMA, void* param, uint64 key);
	void Generate();

private:
	/// Loads the given address into the given register if needed, and returns something that can be used in a `ptr[]`
	LocalAddr loadAddress(AddressReg reg, const void *addr);
	void modulate16(const XYm& a, const Xbyak::Operand& f, uint8 shift);
	void lerp16(const XYm& a, const XYm& b, const XYm& f, uint8 shift);
	void lerp16_4(const XYm& a, const XYm& b, const XYm& f);
	void mix16(const XYm& a, const XYm& b, const XYm& temp);
	void clamp16(const XYm& a, const XYm& temp);
	void alltrue(const XYm& test);
	void blend(const XYm& a, const XYm& b, const XYm& mask);
	void blendr(const XYm& b, const XYm& a, const XYm& mask);
	void blend8(const XYm& a, const XYm& b);
	void blend8r(const XYm& b, const XYm& a);
	void split16_2x8(const XYm& l, const XYm& h, const XYm& src);

	void Init();
	void Step();
	void TestZ(const XYm& temp1, const XYm& temp2);
	void SampleTexture();
	void SampleTexture_TexelReadHelper();
	void Wrap(const XYm& uv);
	void Wrap(const XYm& uv0, const XYm& uv1);
	void SampleTextureLOD();
	void WrapLOD(const XYm& uv);
	void WrapLOD(const XYm& uv0, const XYm& uv1);
	void AlphaTFX();
	void ReadMask();
	void TestAlpha();
	void ColorTFX();
	void Fog();
	void ReadFrame();
	void TestDestAlpha();
	void WriteMask();
	void WriteZBuf();
	void AlphaBlend();
	void WriteFrame();
	void ReadPixel(const XYm& dst, const XYm& tmp, const AddressReg& addr);
#if DRAW_SCANLINE_USING_XMM
	void WritePixel(const XYm& src_, const AddressReg& addr, const Xbyak::Reg8& mask, bool fast, int psm, int fz);
#else
	void WritePixel(const XYm& src_, const AddressReg& addr, const Xbyak::Reg32& mask, bool fast, int psm, int fz);
#endif
	void WritePixel(const Xmm& src, const AddressReg& addr, uint8 i, uint8 j, int psm);
	void ReadTexel1(const XYm& dst, const XYm& src, const XYm& tmp1, const XYm& tmp2, int mip_offset);
	void ReadTexel4(
		const XYm& d0,   const XYm& d1,
		const XYm& d2s0, const XYm& d3s1,
		const XYm& s2,   const XYm& s3,
		const XYm& tmp1, const XYm& tmp2,
		int mip_offset);
	void ReadTexelImpl(
		const XYm& d0,   const XYm& d1,
		const XYm& d2s0, const XYm& d3s1,
		const XYm& s2,   const XYm& s3,
		const XYm& tmp1, const XYm& tmp2,
		int pixels,      int mip_offset);
	void ReadTexelImplLoadTexLOD(int lod, int mip_offset);
	void ReadTexelImplYmm(
		const Ymm& d0,   const Ymm& d1,
		const Ymm& d2s0, const Ymm& d3s1,
		const Ymm& s2,   const Ymm& s3,
		const Ymm& tmp,
		int pixels,      int mip_offset);
	void ReadTexelImplSSE4(
		const Xmm& d0,   const Xmm& d1,
		const Xmm& d2s0, const Xmm& d3s1,
		const Xmm& s2,   const Xmm& s3,
		int pixels,      int mip_offset);
	void ReadTexelImplSSE3(
		const Xmm& d0,   const Xmm& d1,
		const Xmm& d2s0, const Xmm& d3s1,
		const Xmm& s2,   const Xmm& s3,
		const Xmm& tmp1, const Xmm& tmp2,
		int pixels,      int mip_offset);
	void ReadTexelImpl(const Xmm& dst, const Xmm& addr, uint8 i, bool texInA3, bool preserveDst);
};
