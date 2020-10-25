/*
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

#include "stdafx.h"
#include "GSTextureMTL.h"

#if ! __has_feature(objc_arc)
#error "Compile this with -fobjc-arc"
#endif

#ifdef __APPLE__

GSTextureMTL::GSTextureMTL(id<MTLTexture> texture, Type type): m_texture(texture)
{
	m_type = type;
	m_size.x = m_texture.width;
	m_size.y = m_texture.height;
	m_format = m_texture.pixelFormat;
	m_max_layer = m_texture.mipmapLevelCount;
}
GSTextureMTL::~GSTextureMTL()
{
}

void GSTextureMTL::RequestColorClear(GSVector4 color)
{
	m_needs_color_clear = true;
	m_clear_color = color;
}
void GSTextureMTL::RequestDepthClear(float depth)
{
	m_needs_depth_clear = true;
	m_clear_depth = depth;
}
void GSTextureMTL::RequestStencilClear(int stencil)
{
	m_needs_stencil_clear;
	m_clear_stencil = stencil;
}
bool GSTextureMTL::GetResetNeedsColorClear(GSVector4& colorOut)
{
	if (m_needs_color_clear)
	{
		m_needs_color_clear = false;
		colorOut = m_clear_color;
		return true;
	}
	return false;
}
bool GSTextureMTL::GetResetNeedsDepthClear(float& depthOut)
{
	if (m_needs_depth_clear)
	{
		m_needs_depth_clear = false;
		depthOut = m_clear_depth;
		return true;
	}
	return false;
}
bool GSTextureMTL::GetResetNeedsStencilClear(int& stencilOut)
{
	if (m_needs_stencil_clear)
	{
		m_needs_stencil_clear = false;
		stencilOut = m_clear_stencil;
		return true;
	}
	return false;
}

bool GSTextureMTL::Update(const GSVector4i& r, const void* data, int pitch, int layer)
{
	if(layer >= m_max_layer)
		return true;

	MTLRegion box = MTLRegionMake2D(r.left, r.top, r.width(), r.height());

	[m_texture replaceRegion:box
	             mipmapLevel:layer
	               withBytes:data
	             bytesPerRow:pitch];

	return true;
}
bool GSTextureMTL::Map(GSMap& m, const GSVector4i* r, int layer)
{
	if (!r)
		return false;

	id<MTLBuffer> buffer = m_texture.buffer;
	if (!buffer)
		return false;

	m.bits = (uint8_t*)buffer.contents + m_texture.bufferOffset;
	m.pitch = m_texture.bufferBytesPerRow;
	return true;
}

void GSTextureMTL::Unmap()
{
	id<MTLBuffer> buffer = m_texture.buffer;
	NSRange range = NSMakeRange(m_texture.bufferOffset, m_texture.bufferBytesPerRow * m_size.y);
	[buffer didModifyRange:range];
}

bool GSTextureMTL::Save(const std::string& fn)
{
	return false;
}

#endif

