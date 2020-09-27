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
#include "GSDeviceMTL.h"
#include "GSTextureMTL.h"

#if ! __has_feature(objc_arc)
#error "Compile this with -fobjc-arc"
#endif

#ifdef __APPLE__

#define THROWING_ASSERT(value, ...) \
	do { \
		if (__builtin_expect(!(value), 0)) \
		{ \
			NSString* msg = [NSString stringWithFormat:__VA_ARGS__]; \
			fputs([msg cStringUsingEncoding:[NSString defaultCStringEncoding]], stderr); \
			throw GSDXRecoverableError(); \
		} \
	} while (0)

#define NSERROR_CHECK(err, ...) THROWING_ASSERT(!(err), __VA_ARGS__)

GSRenderPipelineMTL::GSRenderPipelineMTL(NSString* name, id<MTLFunction> vs, id<MTLFunction> ps)
{
	m_pipelineDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
	m_pipelineDescriptor.label = name;
	m_pipelineDescriptor.vertexFunction = vs;
	m_pipelineDescriptor.fragmentFunction = ps;
	m_renderDescriptor = [MTLRenderPassDescriptor new];
	m_renderDescriptor.colorAttachments[0].storeAction = MTLStoreActionStore;
	m_renderDescriptor.depthAttachment.storeAction = MTLStoreActionStore;
	m_pipelineDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
	m_pipelineDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
	m_pipelineDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorZero;
}

void GSRenderPipelineMTL::invalidateCachedPipeline()
{
	if (!m_cachedPipeline)
		return;
	m_cachedPipeline = nil;
	if (m_invalidationCount == 100)
	{
		fprintf(stderr, "Metal: shader pipeline %s is getting invalidated a lot\n", [m_pipelineDescriptor.label cStringUsingEncoding:[NSString defaultCStringEncoding]]);
	}
	if (m_invalidationCount <= 100)
		m_invalidationCount++;
}

void GSRenderPipelineMTL::SetLoadActions(MTLLoadAction color, MTLLoadAction depth)
{
	m_renderDescriptor.colorAttachments[0].loadAction = color;
	m_renderDescriptor.depthAttachment.loadAction = depth;
}

void GSRenderPipelineMTL::SetPixelFormats(MTLPixelFormat color, MTLPixelFormat depth)
{
	auto existing_color = m_pipelineDescriptor.colorAttachments[0].pixelFormat;
	if (existing_color != color)
	{
		invalidateCachedPipeline();
		m_pipelineDescriptor.colorAttachments[0].pixelFormat = color;
	}

	auto existing_depth = m_pipelineDescriptor.depthAttachmentPixelFormat;
	if (existing_depth != depth)
	{
		invalidateCachedPipeline();
		m_pipelineDescriptor.depthAttachmentPixelFormat = depth;
	}
}

void GSRenderPipelineMTL::SetTargets(id<MTLTexture> color, id<MTLTexture> depth)
{
	m_renderDescriptor.colorAttachments[0].texture = color;
	m_renderDescriptor.depthAttachment.texture = depth;

	SetPixelFormats(color.pixelFormat, depth.pixelFormat);
}

void GSRenderPipelineMTL::SetBlend(GSDevice& dev, uint8 index, uint8 factor, bool is_constant, bool accumulation_blend)
{
	if (index)
	{
		bool needs_update = m_currentBlendIndex != index;
		needs_update |= accumulation_blend != m_currentIsAccumulation;
		needs_update |= (is_constant && m_currentBlendFactor != factor);
		if (!needs_update)
			return;

		invalidateCachedPipeline();
		m_currentIsAccumulation = accumulation_blend;
		m_currentBlendFactor = factor;
		m_currentBlendIndex = index;

		HWBlend b = dev.GetBlend(index);
		if (accumulation_blend)
		{
			b.src = MTLBlendFactorOne;
			b.dst = MTLBlendFactorOne;
		}

		m_pipelineDescriptor.colorAttachments[0].rgbBlendOperation = (MTLBlendOperation)b.op;
		m_pipelineDescriptor.colorAttachments[0].sourceRGBBlendFactor = (MTLBlendFactor)b.src;
		m_pipelineDescriptor.colorAttachments[0].destinationRGBBlendFactor = (MTLBlendFactor)b.dst;
	}
	else if (m_currentBlendIndex)
	{
		invalidateCachedPipeline();
		m_currentBlendIndex = 0;
		m_pipelineDescriptor.colorAttachments[0].blendingEnabled = false;
	}
}

id<MTLRenderPipelineState> GSRenderPipelineMTL::Pipeline(id<MTLDevice>& dev)
{
	if (!m_cachedPipeline)
	{
		NSError* err = nil;
		m_cachedPipeline = [dev newRenderPipelineStateWithDescriptor:m_pipelineDescriptor error:&err];
		NSERROR_CHECK(err, @"Metal: Failed to create pipeline %@: %@", m_pipelineDescriptor.label, err.localizedDescription);
	}

	return m_cachedPipeline;
}

GSDeviceMTL::GSDeviceMTL()
{
}
GSDeviceMTL::~GSDeviceMTL()
{
}

id<MTLFunction> GSDeviceMTL::loadShader(NSString* name)
{
	id<MTLFunction> output = [m_shaders newFunctionWithName:name];
	THROWING_ASSERT(output, @"Failed to load shader %@", name);
	return output;
}

bool GSDeviceMTL::Create(const std::shared_ptr<GSWnd> &wnd)
{
	if (!GSDevice::Create(wnd))
		return false;

	if (m_dev)
		fprintf(stderr, "Metal: Create called but already had a device\n");
	m_dev = nil;

	std::string adapter_id = theApp.GetConfigS("Adapter");
	NSString* ns_adapter_id = [NSString
		stringWithCString:adapter_id.c_str()
		         encoding:[NSString defaultCStringEncoding]];
	for (id<MTLDevice> dev : MTLCopyAllDevices())
	{
		if ([[dev name] isEqualToString:ns_adapter_id])
		{
			m_dev = dev;
			break;
		}
	}
	if (!m_dev)
	{
		m_dev = MTLCreateSystemDefaultDevice();
		if (adapter_id != "default")
			fprintf(stderr, "Metal: Missing device %s, using default\n", adapter_id.c_str());
	}

	m_layer = (__bridge CAMetalLayer*)m_wnd->GetHandle();
	ASSERT([m_layer isKindOfClass:[CAMetalLayer class]]);
	m_layer.device = m_dev;

	m_queue = [m_dev newCommandQueue];
	THROWING_ASSERT(m_queue, @"Metal: Failed to create command queue\n");

	// TODO: Load from resource
	m_shaders = [m_dev newDefaultLibrary];
	THROWING_ASSERT(m_shaders, @"Metal: Failed to load shaders\n");

	auto vs_convert = loadShader(@"vs_convert");

	for (size_t i = 0; i < countof(m_interlace); i++)
	{
		NSString* name = [NSString stringWithFormat:@"ps_interlace%zu", i];

		auto ps = loadShader(name);

		m_interlace[i] = GSRenderPipelineMTL(name, vs_convert, ps);
		m_interlace[i].SetLoadActions(i < 2 ? MTLLoadActionLoad : MTLLoadActionDontCare, MTLLoadActionDontCare);
		m_interlace[i].SetPixelFormats(MTLPixelFormatRGBA8Uint, MTLPixelFormatInvalid);
	}

	return true;
}

void GSDeviceMTL::StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, GSRenderPipelineMTL& pipeline, bool linear)
{
	bool draw_in_depth = pipeline.DepthPixelFormat() != MTLPixelFormatInvalid;

	@autoreleasepool {
		BeginScene();

		GSTextureMTL* sT = static_cast<GSTextureMTL*>(sTex);
		GSTextureMTL* dT = static_cast<GSTextureMTL*>(dTex);

		if (draw_in_depth)
			pipeline.SetTargets(nil, dT->GetTexture());
		else
			pipeline.SetTargets(dT->GetTexture(), nil);

		auto buffer = [m_queue commandBuffer];

		GSVector2i ds = dT->GetSize();

		float left = dRect.x * 2 / ds.x - 1.0f;
		float right = dRect.z * 2 / ds.x - 1.0f;
		float top = 1.0f - dRect.y * 2 / ds.y;
		float bottom = 1.0f - dRect.w * 2 / ds.y;

		ConvertShaderVertex vertices[] =
		{
			{{left, top}, {0.f, 0.f}, {}}
		};

		EndScene();
	}
}

//GSTexture* CreateSurface(int type, int w, int h, int format) override;
//
//void DoMerge(GSTexture* sTex[3], GSVector4* sRect, GSTexture* dTex, GSVector4* dRect, const GSRegPMODE& PMODE, const GSRegEXTBUF& EXTBUF, const GSVector4& c) override;
//void DoInterlace(GSTexture* sTex, GSTexture* dTex, int shader, bool linear, float yoffset) override;

uint16 GSDeviceMTL::ConvertBlendEnum(uint16 generic)
{
	switch (generic)
	{
	case SRC_COLOR       : return MTLBlendFactorSourceColor;
	case INV_SRC_COLOR   : return MTLBlendFactorOneMinusSourceColor;
	case DST_COLOR       : return MTLBlendFactorDestinationColor;
	case INV_DST_COLOR   : return MTLBlendFactorOneMinusBlendColor;
	case SRC1_COLOR      : return MTLBlendFactorSource1Color;
	case INV_SRC1_COLOR  : return MTLBlendFactorOneMinusSource1Color;
	case SRC_ALPHA       : return MTLBlendFactorSourceAlpha;
	case INV_SRC_ALPHA   : return MTLBlendFactorOneMinusSourceAlpha;
	case DST_ALPHA       : return MTLBlendFactorDestinationAlpha;
	case INV_DST_ALPHA   : return MTLBlendFactorOneMinusDestinationAlpha;
	case SRC1_ALPHA      : return MTLBlendFactorSource1Alpha;
	case INV_SRC1_ALPHA  : return MTLBlendFactorOneMinusSource1Alpha;
	case CONST_COLOR     : return MTLBlendFactorBlendColor;
	case INV_CONST_COLOR : return MTLBlendFactorOneMinusBlendColor;
	case CONST_ONE       : return MTLBlendFactorOne;
	case CONST_ZERO      : return MTLBlendFactorZero;
	case OP_ADD          : return MTLBlendOperationAdd;
	case OP_SUBTRACT     : return MTLBlendOperationSubtract;
	case OP_REV_SUBTRACT : return MTLBlendOperationReverseSubtract;
	default              : ASSERT(0); return 0;
	}
}

//bool Create(const std::shared_ptr<GSWnd> &wnd) override;
//bool Reset(int w, int h) override;
//bool IsLost(bool update = false) override;
//void Present(const GSVector4i& r, int shader) override;
//void Present(GSTexture* sTex, GSTexture* dTex, const GSVector4& dRect, ShaderConvert shader = ShaderConvert::COPY) override;
//void Flip() override;
//
//void SetVSync(int vsync) override;
//
//void BeginScene() override;
//void DrawPrimitive() override;
//void DrawIndexedPrimitive() override;
//void DrawIndexedPrimitive(int offset, int count) override;
//void EndScene() override;
//
//bool HasDepthSparse() override;
//bool HasColorSparse() override;
//
//void ClearRenderTarget(GSTexture* t, const GSVector4& c) override;
//void ClearRenderTarget(GSTexture* t, uint32 c) override;
//void ClearDepth(GSTexture* t) override;
//void ClearStencil(GSTexture* t, uint8 c) override;
//
//GSTexture* CopyOffscreen(GSTexture* src, const GSVector4& sRect, int w, int h, int format = 0, ShaderConvert ps_shader = ShaderConvert::COPY) override;
//
//void CopyRect(GSTexture* sTex, GSTexture* dTex, const GSVector4i& r) override;
//void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, ShaderConvert shader = ShaderConvert::COPY, bool linear = true) override;
//void StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, bool red, bool green, bool blue, bool alpha) override;
//
//void StretchRect(GSTexture* sTex, GSTexture* dTex, const GSVector4& dRect, int shader = 0, bool linear = true);
//
//void PSSetShaderResources(GSTexture* sr0, GSTexture* sr1) override;
//void PSSetShaderResource(int i, GSTexture* sRect) override;
//void OMSetRenderTargets(GSTexture* rt, GSTexture* ds, const GSVector4i* scissor = NULL) override;

#endif
