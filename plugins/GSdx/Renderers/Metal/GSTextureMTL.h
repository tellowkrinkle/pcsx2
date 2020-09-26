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

#pragma once

#include "Renderers/Common/GSTexture.h"

#ifndef __OBJC__
#error "This header is for use with Objective-C++ only.
#endif

#ifdef __APPLE__

#include <Metal/Metal.h>

class GSTextureMTL : public GSTexture
{
	id<MTLTexture> m_texture;
	int m_max_layer;

public:
	GSTextureMTL(id<MTLTexture> texture, int type);
	~GSTextureMTL();

	bool Update(const GSVector4i& r, const void* data, int pitch, int layer = 0) override;
	bool Map(GSMap& m, const GSVector4i* r = NULL, int layer = 0) override;
	void Unmap() override;
//	void GenerateMipmap() override;
	bool Save(const std::string& fn) override;
//	uint32 GetID() override;
};

#endif
