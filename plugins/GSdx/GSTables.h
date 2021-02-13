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

#pragma once

/// Table for storing swizzling of blocks within a page
struct alignas(64) GSBlockSwizzleTable
{
	// Some swizzles are 4x8 and others are 8x4.  An 8x8 table can store either at the cost of 2x size
	uint8 value[8][8];

	constexpr uint8 lookup(int x, int y) const
	{
		return value[y & 7][x & 7];
	}
};

extern const GSBlockSwizzleTable blockTable32;
extern const GSBlockSwizzleTable blockTable32Z;
extern const GSBlockSwizzleTable blockTable16;
extern const GSBlockSwizzleTable blockTable16S;
extern const GSBlockSwizzleTable blockTable16Z;
extern const GSBlockSwizzleTable blockTable16SZ;
extern const GSBlockSwizzleTable blockTable8;
extern const GSBlockSwizzleTable blockTable4;
extern const uint8 columnTable32[8][8];
extern const uint8 columnTable16[8][16];
extern const uint8 columnTable8[16][16];
extern const uint16 columnTable4[16][32];
extern const uint8 clutTableT32I8[128];
extern const uint8 clutTableT32I4[16];
extern const uint8 clutTableT16I8[32];
extern const uint8 clutTableT16I4[16];
