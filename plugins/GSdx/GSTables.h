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

/// Adds sizes to GSBlockSwizzleTable for to feel better about not making mistakes
template <int Height, int Width>
struct GSSizedBlockSwizzleTable : public GSBlockSwizzleTable {};

/// Table for storing offsets of x = 0 pixels from the beginning of the page
/// Add values from a GSPixelRowOffsetTable to get the pixels for x != 0
template <int Height>
struct alignas(128) GSPixelColOffsetTable
{
	int value[Height] = {};

	int operator[](int y) const
	{
		return value[y % Height];
	}
};

/// Table for storing offsets of x != 0 pixels from the pixel at the same y where x = 0
/// Unlike ColOffsets, this table stretches to the maximum size of a texture so no masking is needed
struct alignas(128) GSPixelRowOffsetTable
{
	int value[2048] = {};

	int operator[](size_t x) const
	{
		ASSERT(x < 2048);
		return value[x];
	}
};

/// Adds size to GSPixelRowOffsetTable to feel better about not making mistakes
template <int PageWidth>
struct GSSizedPixelRowOffsetTable : public GSPixelRowOffsetTable {};

/// List of row offset tables
/// Some swizzlings (PSMT8 and PSMT4) have different row offsets depending on which column they're a part of
/// The ones that do use an a a b b b b a a pattern that repeats every 8 rows.
/// You can always look up the correct row in this list with y % 7, but if you use y % Mask where Mask is known at compile time, the compiler should be able to optimize better
template <int PageWidth, int Mask>
struct alignas(sizeof(void*) * 8) GSPixelRowOffsetTableList
{
	const GSPixelRowOffsetTable* rows[8];

	const GSPixelRowOffsetTable& operator[](int y) const
	{
		return *rows[y % Mask];
	}
};

/// Full pixel offset table
/// Template values are for objects constructing from one of these tables
template <int PageHeight, int PageWidth, int BlockHeight, int BlockWidth, int RowMask>
struct GSSwizzleTableList
{
	const GSSizedBlockSwizzleTable<BlockHeight, BlockWidth>& block;
	const GSPixelColOffsetTable<PageHeight>& col;
	const GSPixelRowOffsetTableList<PageWidth, RowMask>& row;
};

/// List of all tables for a given swizzle for easy setup
template <int PageHeight, int PageWidth, int BlockHeight, int BlockWidth, int RowMask>
constexpr GSSwizzleTableList<PageHeight, PageWidth, BlockHeight, BlockWidth, RowMask>
makeSwizzleTableList(
	const GSSizedBlockSwizzleTable<BlockHeight, BlockWidth>& block,
	const GSPixelColOffsetTable<PageHeight>& col,
	const GSPixelRowOffsetTableList<PageWidth, RowMask>& row)
{
	return { block, col, row };
}

extern const GSSizedBlockSwizzleTable<4, 8> blockTable32;
extern const GSSizedBlockSwizzleTable<4, 8> blockTable32Z;
extern const GSSizedBlockSwizzleTable<8, 4> blockTable16;
extern const GSSizedBlockSwizzleTable<8, 4> blockTable16S;
extern const GSSizedBlockSwizzleTable<8, 4> blockTable16Z;
extern const GSSizedBlockSwizzleTable<8, 4> blockTable16SZ;
extern const GSSizedBlockSwizzleTable<4, 8> blockTable8;
extern const GSSizedBlockSwizzleTable<8, 4> blockTable4;
extern const uint8 columnTable32[8][8];
extern const uint8 columnTable16[8][16];
extern const uint8 columnTable8[16][16];
extern const uint16 columnTable4[16][32];
extern const uint8 clutTableT32I8[128];
extern const uint8 clutTableT32I4[16];
extern const uint8 clutTableT16I8[32];
extern const uint8 clutTableT16I4[16];
extern const GSPixelColOffsetTable< 32> pixelColOffset32;
extern const GSPixelColOffsetTable< 32> pixelColOffset32Z;
extern const GSPixelColOffsetTable< 64> pixelColOffset16;
extern const GSPixelColOffsetTable< 64> pixelColOffset16S;
extern const GSPixelColOffsetTable< 64> pixelColOffset16Z;
extern const GSPixelColOffsetTable< 64> pixelColOffset16SZ;
extern const GSPixelColOffsetTable< 64> pixelColOffset8;
extern const GSPixelColOffsetTable<128> pixelColOffset4;

template <int PageWidth>
constexpr GSPixelRowOffsetTableList<PageWidth, 0> makeRowOffsetTableList(
	const GSSizedPixelRowOffsetTable<PageWidth>* a)
{
	return {{ a, a, a, a, a, a, a, a }};
}

template <int PageWidth>
constexpr GSPixelRowOffsetTableList<PageWidth, 7> makeRowOffsetTableList(
	const GSSizedPixelRowOffsetTable<PageWidth>* a,
	const GSSizedPixelRowOffsetTable<PageWidth>* b)
{
	return {{ a, a, b, b, b, b, a, a }};
}

extern const GSSizedPixelRowOffsetTable< 64> _pixelRowOffset32;
extern const GSSizedPixelRowOffsetTable< 64> _pixelRowOffset32Z;
extern const GSSizedPixelRowOffsetTable< 64> _pixelRowOffset16;
extern const GSSizedPixelRowOffsetTable< 64> _pixelRowOffset16S;
extern const GSSizedPixelRowOffsetTable< 64> _pixelRowOffset16Z;
extern const GSSizedPixelRowOffsetTable< 64> _pixelRowOffset16SZ;
extern const GSSizedPixelRowOffsetTable<128> _pixelRowOffset8[2];
extern const GSSizedPixelRowOffsetTable<128> _pixelRowOffset4[2];

constexpr auto pixelRowOffset32   = makeRowOffsetTableList(&_pixelRowOffset32);
constexpr auto pixelRowOffset32Z  = makeRowOffsetTableList(&_pixelRowOffset32Z);
constexpr auto pixelRowOffset16   = makeRowOffsetTableList(&_pixelRowOffset16);
constexpr auto pixelRowOffset16S  = makeRowOffsetTableList(&_pixelRowOffset16S);
constexpr auto pixelRowOffset16Z  = makeRowOffsetTableList(&_pixelRowOffset16Z);
constexpr auto pixelRowOffset16SZ = makeRowOffsetTableList(&_pixelRowOffset16SZ);
constexpr auto pixelRowOffset8 = makeRowOffsetTableList(&_pixelRowOffset8[0], &_pixelRowOffset8[1]);
constexpr auto pixelRowOffset4 = makeRowOffsetTableList(&_pixelRowOffset4[0], &_pixelRowOffset4[1]);

constexpr auto swizzleTables32   = makeSwizzleTableList(blockTable32,   pixelColOffset32,   pixelRowOffset32  );
constexpr auto swizzleTables32Z  = makeSwizzleTableList(blockTable32Z,  pixelColOffset32Z,  pixelRowOffset32Z );
constexpr auto swizzleTables16   = makeSwizzleTableList(blockTable16,   pixelColOffset16,   pixelRowOffset16  );
constexpr auto swizzleTables16Z  = makeSwizzleTableList(blockTable16Z,  pixelColOffset16Z,  pixelRowOffset16Z );
constexpr auto swizzleTables16S  = makeSwizzleTableList(blockTable16S,  pixelColOffset16S,  pixelRowOffset16S );
constexpr auto swizzleTables16SZ = makeSwizzleTableList(blockTable16SZ, pixelColOffset16SZ, pixelRowOffset16SZ);
constexpr auto swizzleTables8    = makeSwizzleTableList(blockTable8,    pixelColOffset8,    pixelRowOffset8   );
constexpr auto swizzleTables4    = makeSwizzleTableList(blockTable4,    pixelColOffset4,    pixelRowOffset4   );
