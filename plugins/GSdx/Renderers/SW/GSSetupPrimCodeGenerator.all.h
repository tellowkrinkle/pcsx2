/*
 *	Copyright (C) 2007-2009 Gabest
 *	http://www.gabest.org
 *	Copyright (C) 2020 PCSX2 Dev Team
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
# define SETUP_PRIM_VECTOR_REGISTER Xbyak::Ymm
# define SETUP_PRIM_USING_XMM 0
# define SETUP_PRIM_USING_YMM 1
#else
# define SETUP_PRIM_VECTOR_REGISTER Xbyak::Xmm
# define SETUP_PRIM_USING_XMM 1
# define SETUP_PRIM_USING_YMM 0
#endif

class GSSetupPrimCodeGenerator2 : public Xbyak::SmartCodeGenerator
{
	using _parent = Xbyak::SmartCodeGenerator;
	using XYm = SETUP_PRIM_VECTOR_REGISTER;

	using Xmm = Xbyak::Xmm;
	using Ymm = Xbyak::Ymm;

	/// On x86-64 we reserve a bunch of GPRs for holding addresses of locals that would otherwise be hard to reach
	/// On x86-32 the same values are just raw 32-bit addresses
	using LocalAddr = Choose3264<size_t, AddressReg>::type;

	constexpr static bool isXmm = std::is_same<XYm, Xbyak::Xmm>::value;
	constexpr static bool isYmm = std::is_same<XYm, Xbyak::Ymm>::value;
	constexpr static int vecsize = isXmm ? 16 : 32;

	constexpr static int dsize = isXmm ? 4 : 8;

	constexpr static int _32_args = 0;
	constexpr static int _invalid = 0xaaaaaaaa;
	constexpr static int _32_vertex = is64 ? _invalid : _32_args + 4;
	constexpr static int _32_index  = is64 ? _invalid : _32_args + 8;
	constexpr static int _32_dscan  = is64 ? _invalid : _32_args + 12;

	GSScanlineSelector m_sel;
	GSScanlineLocalData& m_local;
	bool m_rip;
	bool many_regs;

	struct {uint32 z:1, f:1, t:1, c:1;} m_en;

	const XYm xym0{0}, xym1{1}, xym2{2}, xym3{3}, xym4{4}, xym5{5}, xym6{6}, xym7{7}, xym8{8}, xym9{9}, xym10{10}, xym11{11}, xym12{12}, xym13{13}, xym14{14}, xym15{15};
	const AddressReg _64_vertex, _index, _dscan, _64_t0, t1;
	const LocalAddr _m_local;
	/// Returns the first arg on 32-bit, second on 64-bit
	static LocalAddr chooseLocal(const void *addr32, AddressReg reg64)
	{
		return choose3264((size_t)addr32, reg64);
	}

public:
	GSSetupPrimCodeGenerator2(Xbyak::CodeGenerator* base, Xbyak::CPUInfo cpu, void* param, uint64 key);
	void Generate();

private:
	void Depth_XMM();
	void Depth_YMM();
	void Texture();
	void Color();
};
