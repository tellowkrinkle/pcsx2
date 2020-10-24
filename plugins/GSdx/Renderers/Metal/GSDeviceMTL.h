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

	std::unique_ptr<GSTexture> m_font;

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
