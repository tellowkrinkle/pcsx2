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

	// In Metal clears happen as a part of render passes instead of as separate steps, but the GSDevice API has it as a separate step
	// To deal with that, store the fact that a clear was requested here and it'll be applied on the next render pass
	bool m_needs_color_clear = false;
	bool m_needs_depth_clear = false;
	bool m_needs_stencil_clear = false;
	GSVector4 m_clear_color;
	float m_clear_depth;
	int m_clear_stencil;

public:
	/// Requests the texture be cleared the next time a color render is done
	void RequestColorClear(GSVector4 color);
	/// Requests the texture be cleared the next time a depth render is done
	void RequestDepthClear(float depth);
	/// Requests the texture be cleared the next time a stencil render is done
	void RequestStencilClear(int stencil);
	/// Reads whether a color clear was requested, then clears the request
	bool GetResetNeedsColorClear(GSVector4& colorOut);
	/// Reads whether a depth clear was requested, then clears the request
	bool GetResetNeedsDepthClear(float& depthOut);
	/// Reads whether a stencil clear was requested, then clears the request
	bool GetResetNeedsStencilClear(int& stencilOut);

	/// Applies a clear load action if necessary, otherwise `base`, to desc's first color attachment, then resets the clear request
	void ApplyColorLoadAction(MTLRenderPassDescriptor* desc, MTLLoadAction base);
	/// Applies a clear load action if necessary, otherwise `base`, to desc's depth attachment, then resets the clear request
	void ApplyDepthLoadAction(MTLRenderPassDescriptor* desc, MTLLoadAction base);

	GSTextureMTL(id<MTLTexture> texture, Type type);
	~GSTextureMTL();

	bool Update(const GSVector4i& r, const void* data, int pitch, int layer = 0) override;
	bool Map(GSMap& m, const GSVector4i* r = NULL, int layer = 0) override;
	void Unmap() override;
//	void GenerateMipmap() override;
	bool Save(const std::string& fn) override;
//	uint32 GetID() override;
	id<MTLTexture> GetTexture() { return m_texture; }
};

#endif
