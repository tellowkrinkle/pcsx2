/*
 *	Copyright (C) 2007-2009 Gabest
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

#include "stdafx.h"
#include "GSDrawScanlineCodeGenerator.h"
#include "GSDrawScanlineCodeGenerator.all.h"

void GSDrawScanlineCodeGenerator::Generate()
{
	auto version = SSEVersion::SSE2;
	if (m_cpu.has(util::Cpu::tSSE3))
		version = SSEVersion::SSE3;
	if (m_cpu.has(util::Cpu::tSSE41))
		version = SSEVersion::SSE41;
	if (m_cpu.has(util::Cpu::tAVX))
		version = SSEVersion::AVX;
	if (m_cpu.has(util::Cpu::tAVX2))
		version = SSEVersion::AVX2;

	GSDrawScanlineCodeGenerator2(this, version, m_cpu.has(util::Cpu::tFMA), (void*)&m_local, m_sel.key).Generate();
}

GSDrawScanlineCodeGenerator::GSDrawScanlineCodeGenerator(void* param, uint64 key, void* code, size_t maxsize)
	: GSCodeGenerator(code, maxsize)
	, m_local(*(GSScanlineLocalData*)param)
	, m_rip(false)
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
