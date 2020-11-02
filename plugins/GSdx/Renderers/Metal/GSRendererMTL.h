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

#include "Renderers/HW/GSRendererHW.h"
#include "Renderers/Metal/GSDeviceMTL.h"
#include "Renderers/Metal/GSTextureMTL.h"

#ifndef __OBJC__
#error "This header is for use with Objective-C++ only.  You probably wanted GSMetalShims.h"
#endif

#ifdef __APPLE__

#include <Metal/Metal.h>
#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <Dispatch/Dispatch.h>

class GSRendererMTL final : public GSRendererHW
{
	enum ACC_BLEND
	{
		ACC_BLEND_NONE   = 0,
		ACC_BLEND_BASIC  = 1,
		ACC_BLEND_MEDIUM = 2,
		ACC_BLEND_HIGH   = 3,
		ACC_BLEND_FULL   = 4,
		ACC_BLEND_ULTRA  = 5
	};

	struct Selector
	{
		GSDeviceMTL::VSSelector vs;
		GSDeviceMTL::PSSelector ps;
		GSDeviceMTL::DepthSelector ds;
		GSDeviceMTL::ColorMaskSelector c;
		Selector() = default;
	};


	GSDeviceMTL::PSSamplerSelector m_sampler_sel;
	GSMTLMainVSUniform m_vs_cb = {0};
	GSMTLMainPSUniform m_ps_cb = {0};
	Selector m_sel;
	MTLRenderPassDescriptor* m_rdesc;
	MTLRenderPassDescriptor* m_rdesc_interlock;
	MTLRenderPassDescriptor* m_rdesc_interlock_depth;
	MTLPrimitiveType m_primclass;
	id<MTLTexture> m_dummy_tex;

	TriFiltering UserHacks_tri_filter;

private:
	void ResetStates();
	void SetupIA(float sx, float sy);
	void EmulateTextureShuffleAndFbmask();
	void EmulateChannelShuffle(GSTexture** rt, const GSTextureCache::Source* tex);
	void EmulateBlending();
	void EmulateTextureSampler(const GSTextureCache::Source* tex);
	void EmulateAtst(int pass, const GSTextureCache::Source* tex);
	void EmulateZbuffer();

	void SendDraw(GSTextureMTL* rt, GSTextureMTL* ds, GSTextureCache::Source* tex);

protected:
	void DrawPrims(GSTexture* rt, GSTexture* ds, GSTextureCache::Source* tex) override;

public:
	GSRendererMTL();
};

#endif
