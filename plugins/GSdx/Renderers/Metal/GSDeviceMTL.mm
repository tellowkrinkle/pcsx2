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

GSRenderPipelineMTL::GSRenderPipelineMTL(NSString* name, id<MTLFunction> vs, id<MTLFunction> ps, bool targets_depth, bool is_opaque)
	: m_targetsDepth(targets_depth), m_isOpaque(is_opaque)
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

void GSRenderPipelineMTL::SetClearConstants(MTLClearColor color, double depth)
{
	m_renderDescriptor.colorAttachments[0].clearColor = color;
	m_renderDescriptor.depthAttachment.clearDepth = depth;
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

void GSRenderPipelineMTL::SetColorMask(MTLColorWriteMask mask)
{
	if (m_pipelineDescriptor.colorAttachments[0].writeMask != mask)
	{
		invalidateCachedPipeline();
		m_pipelineDescriptor.colorAttachments[0].writeMask = mask;
	}
}

id<MTLRenderCommandEncoder> GSRenderPipelineMTL::CreateCommandEncoder(id<MTLCommandBuffer> buffer)
{
	return [buffer renderCommandEncoderWithDescriptor:m_renderDescriptor];
}

id<MTLRenderPipelineState> GSRenderPipelineMTL::Pipeline(id<MTLDevice> dev)
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
	m_mipmap = theApp.GetConfigI("mipmap");
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
		if (!m_dev)
		{
			fprintf(stderr, "Metal: No supported devices available!\n");
			throw GSDXRecoverableError();
		}
		if (adapter_id != "default")
			fprintf(stderr, "Metal: Missing device %s, using default\n", adapter_id.c_str());
	}


	if ([m_dev supportsFeatureSet:MTLFeatureSet_macOS_GPUFamily1_v1])
		m_max_texsize = 16384;
	else
		m_max_texsize = 8192;

	m_layer = (__bridge CAMetalLayer*)m_wnd->GetHandle();
	ASSERT([m_layer isKindOfClass:[CAMetalLayer class]]);
	m_layer.device = m_dev;

	m_queue = [m_dev newCommandQueue];
	m_cmdBuffer = [m_queue commandBuffer];
	THROWING_ASSERT(m_queue, @"Metal: Failed to create command queue\n");

	// TODO: Load from resource
	m_shaders = [m_dev newDefaultLibrary];
	THROWING_ASSERT(m_shaders, @"Metal: Failed to load shaders\n");

	auto vs_convert = loadShader(@"vs_convert");

	for (size_t i = 0; i < countof(m_interlace); i++)
	{
		NSString* name = [NSString stringWithFormat:@"ps_interlace%zu", i];

		auto ps = loadShader(name);

		m_interlace[i] = GSRenderPipelineMTL(name, vs_convert, ps, /*targets_depth=*/false, /*is_opaque=*/i >= 2);
	}

	for (size_t i = 0; i < countof(m_convert); i++)
	{
		auto shader = static_cast<ShaderConvert>(i);
		NSString* name = [NSString stringWithCString:shaderName(shader) encoding:[NSString defaultCStringEncoding]];

		auto ps = loadShader(name);

		bool depth = false;
		bool opaque = true;

		switch (shader)
		{
			case ShaderConvert::RGBA8_TO_FLOAT32:
			case ShaderConvert::RGBA8_TO_FLOAT24:
			case ShaderConvert::RGBA8_TO_FLOAT16:
			case ShaderConvert::RGB5A1_TO_FLOAT16:
				depth = true;
				break;
			case ShaderConvert::DATM_1:
			case ShaderConvert::DATM_0:
				opaque = false;
				break;
			default:
				break;
		}

		m_convert[i] = GSRenderPipelineMTL(name, vs_convert, ps, /*targets_depth=*/depth, /*is_opaque=*/true);
	}

	MTLSamplerDescriptor* sampler = [MTLSamplerDescriptor new];
	sampler.minFilter = MTLSamplerMinMagFilterNearest;
	sampler.magFilter = MTLSamplerMinMagFilterNearest;
	sampler.sAddressMode = MTLSamplerAddressModeClampToEdge;
	sampler.tAddressMode = MTLSamplerAddressModeClampToEdge;
	m_sampler_pt = [m_dev newSamplerStateWithDescriptor:sampler];
	sampler.minFilter = MTLSamplerMinMagFilterLinear;
	sampler.magFilter = MTLSamplerMinMagFilterLinear;
	m_sampler_ln = [m_dev newSamplerStateWithDescriptor:sampler];

	// Texture Font (OSD)
	GSVector2i tex_font = m_osd.get_texture_font_size();
	m_font = std::unique_ptr<GSTexture>(
		CreateSurface(GSTexture::Type::Texture, tex_font.x, tex_font.y, MTLPixelFormatR8Unorm)
	);

	return true;
}

bool GSDeviceMTL::Reset(int w, int h)
{
	if(!GSDevice::Reset(w, h))
		return false;

	// Note: We let m_layer figure out its own size from the view it's a part of for now
	//[m_layer setDrawableSize:CGSizeMake(w, h)];

	return true;
}

void GSDeviceMTL::Present(const GSVector4i& r, int shader)
{
	// Note: We let m_layer figure out its own size from the view it's a part of for now

	@autoreleasepool {
		GSScopedDebugGroupMTL(m_cmdBuffer, @"Present");

		id<CAMetalDrawable> drawable = [m_layer nextDrawable];

		if (m_current)
		{
			GSTextureMTL backbuffer(drawable.texture, GSTexture::Type::Backbuffer);
			GSDevice::Present(m_current, &backbuffer, GSVector4(r), PRESENT_SHADERS[shader]);
			RenderOsd(&backbuffer);
		}

		[m_cmdBuffer presentDrawable:drawable];
	}

	[m_cmdBuffer commit];

	m_cmdBuffer = [m_queue commandBuffer];
}

void GSDeviceMTL::Flip()
{
	fprintf(stderr, "Metal: Flip called, use present instead!\n");
}

void GSDeviceMTL::SetVSync(int vsync)
{
	if (@available(macOS 10.13, *)) {
		m_layer.displaySyncEnabled = vsync ? YES : NO;
	}
}

bool GSDeviceMTL::HasDepthSparse()
{
	// TODO: Implement
	return false;
}

bool GSDeviceMTL::HasColorSparse()
{
	// TODO: Implement
	return false;
}

//void BeginScene() override;
//void DrawPrimitive() override;
//void DrawIndexedPrimitive() override;
//void DrawIndexedPrimitive(int offset, int count) override;
//void EndScene() override;

void GSDeviceMTL::ClearRenderTarget(GSTexture* t, const GSVector4& c)
{
	if (!t) return;
	static_cast<GSTextureMTL*>(t)->RequestColorClear(c);
}

void GSDeviceMTL::ClearRenderTarget(GSTexture* t, uint32 c)
{
	GSVector4 color = GSVector4::rgba32(c) * (1.f / 255.f);
	ClearRenderTarget(t, color);
}

void GSDeviceMTL::ClearDepth(GSTexture* t)
{
	if (!t) return;
	static_cast<GSTextureMTL*>(t)->RequestDepthClear(0);
}

void GSDeviceMTL::ClearStencil(GSTexture* t, uint8 c)
{
	if (!t) return;
	static_cast<GSTextureMTL*>(t)->RequestStencilClear(c);
}

GSTexture* GSDeviceMTL::CreateSurface(GSTexture::Type type, int w, int h, int format)
{
	int layers = m_mipmap && format == MTLPixelFormatRGBA8Unorm ? (int)log2(std::max(w,h)) : 1;

	MTLTextureDescriptor* desc = [MTLTextureDescriptor
		texture2DDescriptorWithPixelFormat:(MTLPixelFormat)format
		                             width:std::max(1, std::min(w, m_max_texsize))
		                            height:std::max(1, std::min(h, m_max_texsize))
		                         mipmapped:layers > 1];
	switch (type)
	{
		case GSTexture::Type::Texture:
			desc.usage = MTLTextureUsageShaderRead;
			desc.mipmapLevelCount = layers;
			break;
		default:
			desc.usage = MTLTextureUsageShaderRead | MTLTextureUsageRenderTarget;
	}

	id<MTLTexture> tex = [m_dev newTextureWithDescriptor:desc];
	if (tex)
	{
		GSTextureMTL* t = new GSTextureMTL(tex, type);
		switch (type)
		{
			case GSTexture::Type::RenderTarget:
				ClearRenderTarget(t, 0);
				break;
			case GSTexture::Type::DepthStencil:
				ClearDepth(t);
				break;
			default:
				break;
		}
		return t;
	}
	else
	{
		throw std::bad_alloc();
	}
}

GSTexture* GSDeviceMTL::FetchSurface(GSTexture::Type type, int w, int h, int format)
{
	if (format == 0)
	{
		switch (type)
		{
			case GSTexture::Type::DepthStencil:
			case GSTexture::Type::SparseDepthStencil:
				format = MTLPixelFormatDepth32Float_Stencil8;
				break;
			default:
				format = MTLPixelFormatRGBA8Unorm;
		}
	}

	return GSDevice::FetchSurface(type, w, h, format);
}

GSTexture* GSDeviceMTL::CopyOffscreen(GSTexture* src, const GSVector4& sRect, int w, int h, int format, ShaderConvert ps_shader)
{
	if (format == 0)
		format = MTLPixelFormatRGBA8Unorm;

	ASSERT(format == MTLPixelFormatRGBA8Unorm || format == MTLPixelFormatR16Uint || format == MTLPixelFormatR32Uint);

	if (GSTexture* dst = CreateRenderTarget(w, h, format))
	{
		GSVector4 dRect(0, 0, w, h);
		StretchRect(src, sRect, dst, dRect, m_convert[(int)ps_shader]);
	}

	return nullptr;
}

void GSDeviceMTL::CopyRect(GSTexture* sTex, GSTexture* dTex, const GSVector4i& r)
{
	if (!sTex || !dTex)
	{
		ASSERT(0);
		return;
	}

	auto sT = static_cast<GSTextureMTL*>(sTex);
	auto dT = static_cast<GSTextureMTL*>(dTex);

	auto blit = [m_cmdBuffer blitCommandEncoder];
	blit.label = @"CopyRect";
	[blit copyFromTexture:sT->GetTexture()
	          sourceSlice:0
	          sourceLevel:0
	         sourceOrigin:MTLOriginMake(r.x, r.y, 0)
	           sourceSize:MTLSizeMake(r.width(), r.height(), 1)
	            toTexture:dT->GetTexture()
	     destinationSlice:0
	     destinationLevel:0
	    destinationOrigin:MTLOriginMake(0, 0, 0)];

	[blit endEncoding];
}

void GSDeviceMTL::StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, ShaderConvert shader, bool linear)
{
	StretchRect(sTex, sRect, dTex, dRect, m_convert[(int)shader], linear);
}

void GSDeviceMTL::StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, bool red, bool green, bool blue, bool alpha)
{
	MTLColorWriteMask mask = MTLColorWriteMaskNone;
	if (red)   mask |= MTLColorWriteMaskRed;
	if (green) mask |= MTLColorWriteMaskGreen;
	if (blue)  mask |= MTLColorWriteMaskBlue;
	if (alpha) mask |= MTLColorWriteMaskAlpha;

	StretchRect(sTex, sRect, dTex, dRect, m_convert[(int)ShaderConvert::COPY], /*linear=*/false, mask);
}

void GSDeviceMTL::StretchRect(GSTexture* sTex, const GSVector4& sRect, GSTexture* dTex, const GSVector4& dRect, GSRenderPipelineMTL& pipeline, bool linear, int bs, MTLColorWriteMask cms, void* fragUniform, size_t fragUniformLen)
{
	@autoreleasepool {
		BeginScene();

		GSTextureMTL* sT = static_cast<GSTextureMTL*>(sTex);
		GSTextureMTL* dT = static_cast<GSTextureMTL*>(dTex);

		GSVector2i ds = dT->GetSize();
		// If the destination rect completely covers the texture and the fragment shader is opaque, we don't need to load the old texture
		// Note: I think this only affects performance on tile-based GPUs (which is only arm macs), but we might as well do it anyways
		bool covers_target = pipeline.IsOpaque()
			&& bs == 0
			&& (int)dRect.x <= 0
			&& (int)dRect.y <= 0
			&& (int)dRect.z >= ds.x
			&& (int)dRect.w >= ds.y;
		MTLLoadAction action = covers_target ? MTLLoadActionDontCare : MTLLoadActionLoad;

		pipeline.SetColorMask(cms);
		pipeline.SetBlend(*this, bs, 0, false, false);

		if (pipeline.TargetsDepth())
		{
			pipeline.SetTargets(nil, dT->GetTexture());
			float depth;
			if (dT->GetResetNeedsDepthClear(depth))
			{
				pipeline.SetLoadActions(MTLLoadActionDontCare, MTLLoadActionClear);
				pipeline.SetClearConstants({}, depth);
			}
			else
			{
				pipeline.SetLoadActions(MTLLoadActionDontCare, action);
			}
		}
		else
		{
			pipeline.SetTargets(dT->GetTexture(), nil);
			GSVector4 c;
			if (dT->GetResetNeedsColorClear(c))
			{
				pipeline.SetLoadActions(MTLLoadActionClear, MTLLoadActionDontCare);
				pipeline.SetClearConstants(MTLClearColorMake(c.r, c.g, c.b, c.a), 0);
			}
			else
			{
				pipeline.SetLoadActions(action, MTLLoadActionDontCare);
			}
		}

		float left = dRect.x * 2 / ds.x - 1.0f;
		float right = dRect.z * 2 / ds.x - 1.0f;
		float top = 1.0f - dRect.y * 2 / ds.y;
		float bottom = 1.0f - dRect.w * 2 / ds.y;

		ConvertShaderVertex vertices[] =
		{
			{{left,  top},    {sRect.x, sRect.y}, {}},
			{{right, top},    {sRect.z, sRect.y}, {}},
			{{left,  bottom}, {sRect.x, sRect.w}, {}},
			{{right, bottom}, {sRect.z, sRect.w}, {}}
		};

		auto encoder = pipeline.CreateCommandEncoder(m_cmdBuffer);
		encoder.label = @"StretchRect";

		[encoder setRenderPipelineState:pipeline.Pipeline(m_dev)];

		[encoder setFragmentTexture:sT->GetTexture() atIndex:0];

		[encoder setVertexBytes:vertices
		                 length:sizeof(vertices)
		                atIndex:GSMTLIndexVertices];

		if (fragUniform && fragUniformLen)
		{
			[encoder setFragmentBytes:fragUniform
			                   length:fragUniformLen
			                  atIndex:GSMTLIndexUniforms];
		}

		[encoder setFragmentSamplerState:linear ? m_sampler_ln : m_sampler_pt
		                         atIndex:0];

		[encoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
		            vertexStart:0
		            vertexCount:4];

		[encoder endEncoding];

		EndScene();
	}
}

void GSDeviceMTL::RenderOsd(GSTexture* dt)
{
	BeginScene();

	if (m_osd.m_texture_dirty) {
		m_osd.upload_texture_atlas(m_font.get());
	}

	auto& pipeline = m_convert[(int)ShaderConvert::OSD];
	pipeline.SetBlend(*this, m_MERGE_BLEND, 0, false, false);
	pipeline.SetColorMask(MTLColorWriteMaskAll);
	pipeline.SetTargets(static_cast<GSTextureMTL*>(dt)->GetTexture(), nil);
	pipeline.SetLoadActions(MTLLoadActionLoad, MTLLoadActionDontCare);

	size_t count = m_osd.Size();
	GSVertexPT1 tmp[count];
	m_osd.GeneratePrimitives(tmp, count);
	// TODO: Use buffer
	ConvertShaderVertex verts[count];
	for (size_t i = 0; i < count; i++)
	{
		const auto& src = tmp[i];
		verts[i] = { {src.p.x, src.p.y}, {src.t.x, src.t.y}, {src.r, src.g, src.b, src.a} };
	}

	auto encoder = pipeline.CreateCommandEncoder(m_cmdBuffer);
	encoder.label = @"RenderOSD";
	[encoder setRenderPipelineState:pipeline.Pipeline(m_dev)];

	[encoder setVertexBytes:verts
	                 length:sizeof(verts)
	                atIndex:GSMTLIndexVertices];

	[encoder setFragmentSamplerState:m_sampler_pt atIndex:0];

	[encoder drawPrimitives:MTLPrimitiveTypeTriangle
	            vertexStart:0
	            vertexCount:count];

	[encoder endEncoding];

	EndScene();
}

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

//void PSSetShaderResources(GSTexture* sr0, GSTexture* sr1) override;
//void PSSetShaderResource(int i, GSTexture* sRect) override;
//void OMSetRenderTargets(GSTexture* rt, GSTexture* ds, const GSVector4i* scissor = NULL) override;

#endif
