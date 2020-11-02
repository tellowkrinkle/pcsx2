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

#include "Renderers/Common/GSDevice.h"

#ifndef __OBJC__
#error "This header is for use with Objective-C++ only.  You probably wanted GSMetalShims.h"
#endif

#ifdef __APPLE__

#include <Metal/Metal.h>
#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <Dispatch/Dispatch.h>
#include <mutex>
#include <vector>
#include <unordered_map>
#include "res/metal/uniforms.h"

class GSScopedDebugGroupMTL
{
	id<MTLCommandBuffer> m_buffer;
public:
	GSScopedDebugGroupMTL(id<MTLCommandBuffer> buffer, NSString* name): m_buffer(buffer)
	{
		if (@available(macOS 10.13, *))
			[m_buffer pushDebugGroup:name];
	}
	~GSScopedDebugGroupMTL()
	{
		if (@available(macOS 10.13, *))
			[m_buffer popDebugGroup];
	}
};

/// Holds onto a pool of `MTLBuffer`s
class GSBufferPoolMTL
{
	std::mutex mutex;
	std::vector<id<MTLBuffer>> buffers;
	dispatch_group_t inflight;

public:
	GSBufferPoolMTL();
	~GSBufferPoolMTL();
	GSBufferPoolMTL(GSBufferPoolMTL&&) = delete;
	id<MTLBuffer> getBuffer(id<MTLCommandBuffer> target, size_t size);
};

/// Holds the information required to make a MTLRenderPipelineState, and caches the most-recently-used one
class GSRenderPipelineMTL
{
	MTLRenderPipelineDescriptor* m_pipelineDescriptor;
	MTLRenderPassDescriptor* m_renderDescriptor;
	id<MTLRenderPipelineState> m_cachedPipeline;
	uint8 m_invalidationCount = 0;
	uint8 m_currentBlendIndex = 0;
	uint8 m_currentBlendFactor;
	bool m_currentIsAccumulation;
	bool m_targetsDepth;
	bool m_isOpaque;

	void invalidateCachedPipeline();

public:
	GSRenderPipelineMTL() = default;
	GSRenderPipelineMTL(NSString* name, id<MTLFunction> vs, id<MTLFunction> ps, bool targets_depth, bool is_opaque);

	bool TargetsDepth() { return m_targetsDepth; }
	bool IsOpaque() { return m_isOpaque; }
	bool IsBlendingEnabled() { return m_currentBlendIndex == 0; }

	void SetLoadActions(MTLLoadAction color, MTLLoadAction depth);
	void SetClearConstants(MTLClearColor color, double depth);
	void SetPixelFormats(MTLPixelFormat color, MTLPixelFormat depth);
	void SetTargets(id<MTLTexture> color, id<MTLTexture> depth);
	void SetBlend(GSDevice& dev, uint8 index, uint8 factor, bool is_constant, bool accumulation_blend);
	void SetColorMask(MTLColorWriteMask mask);

	MTLPixelFormat ColorPixelFormat() { return m_pipelineDescriptor.colorAttachments[0].pixelFormat; }
	MTLPixelFormat DepthPixelFormat() { return m_pipelineDescriptor.depthAttachmentPixelFormat; }

	id<MTLRenderCommandEncoder> CreateCommandEncoder(id<MTLCommandBuffer> buffer);
	id<MTLRenderPipelineState> Pipeline(id<MTLDevice> dev);
};

class GSDeviceMTL final : public GSDevice
{
public:
	struct VSSelector
	{
		union
		{
			uint32 key;
			struct
			{
				uint32 fst:1;
				uint32 iip:1;
			};
		};
		constexpr VSSelector(): key(0) {}
	};

	struct PSSelector
	{
		union
		{
			uint64 key;
			struct
			{
				// *** Word 1
				// Format
				uint32 tex_fmt:4;
				uint32 dfmt:2;
				uint32 depth_fmt:2;
				// Alpha extension/Correction
				uint32 aem:1;
				uint32 fba:1;
				// Fog
				uint32 fog:1;
				// Flat/goround shading
				uint32 iip:1;
				// Pixel test
				uint32 date:3;
				uint32 atst:3;
				// Color sampling
				uint32 fst:1; // Investigate to do it on the VS
				uint32 tfx:3;
				uint32 tcc:1;
				uint32 wms:2;
				uint32 wmt:2;
				uint32 ltf:1;
				// Shuffle and fbmask effect
				uint32 shuffle:1;
				uint32 read_ba:1;
				uint32 write_rg:1;
				uint32 fbmask:1;

				//uint32 _free1:0;

				// *** Word 2
				// Blend and Colclip
				uint32 blend_a:2;
				uint32 blend_b:2;
				uint32 blend_c:2;
				uint32 blend_d:2;
				uint32 clr1:1; // useful?
				uint32 hdr:1;
				uint32 colclip:1;
				// uint32 pabe:1;

				// Others ways to fetch the texture
				uint32 channel:3;

				// Dithering
				uint32 dither:2;

				// Depth clamp
				uint32 zclamp:1;

				// Hack
				uint32 tcoffsethack:1;
				uint32 urban_chaos_hle:1;
				uint32 tales_of_abyss_hle:1;
				uint32 tex_is_fb:1; // Jak Shadows
				uint32 automatic_lod:1;
				uint32 manual_lod:1;
				uint32 point_sampler:1;
				uint32 invalid_tex0:1; // Lupin the 3rd

				uint32 sw_blend:1;
				uint32 interlock:1;

				uint32 _free2:6;
			};
		};

		constexpr PSSelector(): key(0) {}
	};

	struct DepthSelector
	{
		union
		{
			uint32 key;
			struct
			{
				uint32 ztst:2;
				uint32 zwe:1;
			};
		};
		constexpr DepthSelector(): key(0) {}
	};

	struct ColorMaskSelector
	{
		union
		{
			struct
			{
				uint32 wr:1;
				uint32 wg:1;
				uint32 wb:1;
				uint32 wa:1;
			};
			struct
			{
				uint32 wrgba:4;
			};
		};
		operator MTLColorWriteMask()
		{
			MTLColorWriteMask w = MTLColorWriteMaskNone;
			if (wr) w |= MTLColorWriteMaskRed;
			if (wg) w |= MTLColorWriteMaskGreen;
			if (wb) w |= MTLColorWriteMaskBlue;
			if (wa) w |= MTLColorWriteMaskAlpha;
			return w;
		}
		constexpr ColorMaskSelector(): wrgba(0xF) {}
		constexpr ColorMaskSelector(uint32 c): wrgba(c) {}
	};

	struct PSSamplerSelector
	{
		union
		{
			uint32 key;
			struct
			{
				uint32 tau:1;
				uint32 tav:1;
				uint32 biln:1;
				uint32 triln:3;
				uint32 aniso:1;

				uint32 _free:25;
			};
		};

		constexpr PSSamplerSelector(): key(0) {}
		constexpr PSSamplerSelector(uint32 k): key(k) {}
	};

private:
	id<MTLDevice> m_dev = nil;
	id<MTLCommandQueue> m_queue = nil;
	id<MTLCommandBuffer> m_cmdBuffer = nil;
	CAMetalLayer* m_layer = nil;
	id<MTLLibrary> m_shaders = nil;
	int m_max_texsize;
	int m_mipmap;

	id<MTLSamplerState> m_sampler_pt = nil;
	id<MTLSamplerState> m_sampler_ln = nil;
	GSRenderPipelineMTL m_convert[(int)ShaderConvert::Count];
	GSRenderPipelineMTL m_interlace[4];
	GSRenderPipelineMTL m_merge[2];

	id<MTLFunction> m_vs[1<<2];
	std::unordered_map<uint64, id<MTLRenderPipelineState>> m_pipelines;

	GSBufferPoolMTL m_osd_vertex_buffers;

	std::unique_ptr<GSTexture> m_font;

	id<MTLFunction> CompileVS(VSSelector sel);
	id<MTLFunction> CompilePS(PSSelector sel);

public:
	id<MTLRenderPipelineState> GetPipeline(VSSelector vs, PSSelector ps);
	id<MTLCommandBuffer> CmdBuffer() { return m_cmdBuffer; }
	id<MTLDevice> MTLDevice() { return m_dev; }

private:
	id<MTLFunction> loadShader(NSString* name);

	GSTexture* CreateSurface(GSTexture::Type type, int w, int h, int format) override;
	GSTexture* FetchSurface(GSTexture::Type type, int w, int h, int format) override;

	void DoMerge(GSTexture* sTex[3], GSVector4* sRect, GSTexture* dTex, GSVector4* dRect, const GSRegPMODE& PMODE, const GSRegEXTBUF& EXTBUF, const GSVector4& c) override;
	void DoInterlace(GSTexture* sTex, GSTexture* dTex, int shader, bool linear, float yoffset) override;
//	void DoFXAA(GSTexture* sTex, GSTexture* dTex) override;
//	void DoShadeBoost(GSTexture* sTex, GSTexture* dTex) override;
//	void DoExternalFX(GSTexture* sTex, GSTexture* dTex) override;
	uint16 ConvertBlendEnum(uint16 generic) override;

public:
	GSDeviceMTL();
	virtual ~GSDeviceMTL();

	bool Create(const std::shared_ptr<GSWnd> &wnd) override;
	bool Reset(int w, int h) override;
	void Present(const GSVector4i& r, int shader) override;
	void Flip() override;

	void SetVSync(int vsync) override;

	void BeginScene() override;
	void DrawPrimitive() override;
	void DrawIndexedPrimitive() override;
	void DrawIndexedPrimitive(int offset, int count) override;
	void EndScene() override;

	bool HasDepthSparse() override;
	bool HasColorSparse() override;

	void ClearRenderTarget(GSTexture* t, const GSVector4& c) override;
	void ClearRenderTarget(GSTexture* t, uint32 c) override;
	void ClearDepth(GSTexture* t) override;
	void ClearStencil(GSTexture* t, uint8 c) override;

	GSTexture* CopyOffscreen(GSTexture* src, const GSVector4& sRect, int w, int h, int format = 0, ShaderConvert ps_shader = ShaderConvert::COPY) override;

	void CopyRect(GSTexture* sTex, GSTexture* dTex, const GSVector4i& r) override;
	void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, ShaderConvert shader = ShaderConvert::COPY, bool linear = true) override;
	void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, GSRenderPipelineMTL& pipeline, bool linear = true, int bs = m_NO_BLEND, MTLColorWriteMask cms = MTLColorWriteMaskAll, void* fragUniform = nil, size_t fragUniformLen = 0);
	void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, bool red, bool green, bool blue, bool alpha) override;

	void RenderOsd(GSTexture* dt) override;

	void PSSetShaderResources(GSTexture* sr0, GSTexture* sr1) override;
	void PSSetShaderResource(int i, GSTexture* sRect) override;
	void OMSetRenderTargets(GSTexture* rt, GSTexture* ds, const GSVector4i* scissor = NULL) override;
};

#endif
