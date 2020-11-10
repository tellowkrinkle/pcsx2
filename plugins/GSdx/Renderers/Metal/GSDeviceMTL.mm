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
#include "GSMetalShims.h"
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

GSDevice* makeGSDeviceMTL()
{
	return new GSDeviceMTL();
}

GSBufferPoolMTL::GSBufferPoolMTL() : inflight(dispatch_group_create())
{
}

GSBufferPoolMTL::~GSBufferPoolMTL()
{
	// Wait for all the buffers to return we don't crash when they try to return to a non-existant pool
	dispatch_group_wait(inflight, DISPATCH_TIME_FOREVER);
}

id<MTLBuffer> GSBufferPoolMTL::getBuffer(id<MTLCommandBuffer> target, size_t size)
{
	id<MTLBuffer> buffer;
	{
		std::lock_guard<std::mutex> guard(mutex);
		if (!buffers.empty())
		{
			buffer = std::move(buffers.back());
			buffers.pop_back();
		}
	}
	if (!buffer || buffer.length < size)
		buffer = [target.device newBufferWithLength:size options:MTLResourceStorageModeManaged];
	dispatch_group_enter(inflight);

	[target addCompletedHandler:[this, buffer](id<MTLCommandBuffer> _) {
		std::lock_guard<std::mutex> guard(mutex);
		buffers.push_back(buffer);
		dispatch_group_leave(inflight);
	}];
	return buffer;
}

GSRenderPipelineMTL::GSRenderPipelineMTL(NSString* name, id<MTLFunction> vs, id<MTLFunction> ps, bool targets_depth, bool is_opaque)
	: m_targetsDepth(targets_depth), m_isOpaque(is_opaque)
{
	m_pipelineDescriptor = [MTLRenderPipelineDescriptor new];
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
		if (is_constant)
			m_currentBlendFactor = factor;
		else
			m_currentBlendFactor = 0;
		if (!needs_update)
			return;

		invalidateCachedPipeline();
		m_currentIsAccumulation = accumulation_blend;
		m_currentBlendIndex = index;

		HWBlend b = dev.GetBlend(index);
		if (accumulation_blend)
		{
			b.src = MTLBlendFactorOne;
			b.dst = MTLBlendFactorOne;
		}

		m_pipelineDescriptor.colorAttachments[0].blendingEnabled = true;
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
	auto enc =  [buffer renderCommandEncoderWithDescriptor:m_renderDescriptor];
	if (m_currentBlendFactor)
	{
		float f = m_currentBlendFactor / 128.f;
		[enc setBlendColorRed:f green:f blue:f alpha:f];
	}
	return enc;
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

	for (size_t i = 0; i < countof(m_merge); i++)
	{
		NSString* name = [NSString stringWithFormat:@"ps_merge%zu", i];

		auto ps = loadShader(name);

		m_merge[i] = GSRenderPipelineMTL(name, vs_convert, ps, /*targets_depth=*/false, /*is_opaque=*/true);
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

		m_convert[i] = GSRenderPipelineMTL(name, vs_convert, ps, /*targets_depth=*/depth, /*is_opaque=*/opaque);
	}

	for (size_t i = 0; i < countof(m_vs); i++)
	{
		VSSelector sel;
		sel.key = i;
		m_vs[i] = CompileVS(sel);
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

	return true;
}

void GSDeviceMTL::Present(const GSVector4i& r, int shader)
{
	GSVector4i cr = m_wnd->GetClientRect();
	[m_layer setDrawableSize:CGSizeMake(cr.width(), cr.height())];

	@autoreleasepool {
		GSScopedDebugGroupMTL dbg(m_cmdBuffer, @"Present");

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
	fprintf(stderr, "Metal: Flip is unsupported, use present instead!\n");
}

void GSDeviceMTL::SetVSync(int vsync)
{
	if (@available(macOS 10.13, *)) {
		dispatch_sync(dispatch_get_main_queue(), [&]{
			m_layer.displaySyncEnabled = vsync ? YES : NO;
		});
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

void GSDeviceMTL::BeginScene()
{
}

void GSDeviceMTL::DrawPrimitive()
{
	fprintf(stderr, "Metal: DrawPrimitive unimplemented\n");
}

void GSDeviceMTL::DrawIndexedPrimitive()
{
	fprintf(stderr, "Metal: DrawIndexedPrimitive unimplemented\n");
}

void GSDeviceMTL::DrawIndexedPrimitive(int offset, int count)
{
	fprintf(stderr, "Metal: DrawIndexedPrimitive unimplemented\n");
}

void GSDeviceMTL::EndScene()
{
}

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

	if (format == MTLPixelFormatDepth32Float_Stencil8)
	{
		// Depth stencils must be private
		desc.storageMode = MTLStorageModePrivate;
	}

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
	size_t len = sizeof(ConvertShaderVertex) * count;
	ConvertShaderVertex* vptr = static_cast<ConvertShaderVertex*>(m_main_buffer.Map(m_cmdBuffer, len));
	for (size_t i = 0; i < count; i++)
	{
		const auto& src = tmp[i];
		vptr[i] = { {src.p.x, src.p.y}, {src.t.x, src.t.y}, {src.r, src.g, src.b, src.a} };
	}
	auto verts = m_main_buffer.Unmap();

	auto encoder = pipeline.CreateCommandEncoder(m_cmdBuffer);
	encoder.label = @"RenderOSD";
	[encoder setRenderPipelineState:pipeline.Pipeline(m_dev)];
	[encoder setFragmentTexture:static_cast<GSTextureMTL*>(&*m_font)->GetTexture() atIndex:0];

	[encoder setVertexBuffer:verts.first offset:verts.second atIndex:GSMTLIndexVertices];

	[encoder setFragmentSamplerState:m_sampler_pt atIndex:0];

	[encoder drawPrimitives:MTLPrimitiveTypeTriangle
	            vertexStart:0
	            vertexCount:count];

	[encoder endEncoding];

	EndScene();
}

void GSDeviceMTL::DoMerge(GSTexture* sTex[3], GSVector4* sRect, GSTexture* dTex, GSVector4* dRect, const GSRegPMODE& PMODE, const GSRegEXTBUF& EXTBUF, const GSVector4& c)
{
	GSScopedDebugGroupMTL dbg(m_cmdBuffer, @"DoMerge");

	GSVector4 full_r(0.0f, 0.0f, 1.0f, 1.0f);
	bool feedback_write_2 = PMODE.EN2 && sTex[2] != nullptr && EXTBUF.FBIN == 1;
	bool feedback_write_1 = PMODE.EN1 && sTex[2] != nullptr && EXTBUF.FBIN == 0;
	bool feedback_write_2_but_blend_bg = feedback_write_2 && PMODE.SLBG == 1;

	ClearRenderTarget(dTex, c);

	vector_float4 cb_c = { c.r, c.g, c.b, c.a };
	ConvertFragShaderUniform cb_yuv = {0};
	cb_yuv.emoda = EXTBUF.EMODA;
	cb_yuv.emodc = EXTBUF.EMODC;

	if (sTex[1] && (PMODE.SLBG == 0 || feedback_write_2_but_blend_bg)) {
		// 2nd output is enabled and selected. Copy it to destination so we can blend it with 1st output
		// Note: value outside of dRect must contains the background color (c)
		StretchRect(sTex[1], sRect[1], dTex, dRect[1], ShaderConvert::COPY);
	}

	// Save 2nd output
	if (feedback_write_2) // FIXME I'm not sure dRect[1] is always correct
		StretchRect(dTex, full_r, sTex[2], dRect[1], m_convert[(int)ShaderConvert::YUV], true, 0, MTLColorWriteMaskAll, &cb_yuv, sizeof(cb_yuv));

	if (feedback_write_2_but_blend_bg)
		ClearRenderTarget(dTex, c);

	if (sTex[0]) {
		if (PMODE.AMOD == 1) {
			// TODO: OpenGL says keep the alpha from the 2nd output but then sets something that gets overwritten by every StretchRect call...
		}

		// 1st output is enabled. It must be blended
		if (PMODE.MMOD == 1) {
			// Blend with a constant alpha
			StretchRect(sTex[0], sRect[0], dTex, dRect[0], m_merge[1], true, m_MERGE_BLEND, MTLColorWriteMaskAll, &cb_c, sizeof(cb_c));
		} else {
			// Blend with 2 * input alpha
			StretchRect(sTex[0], sRect[0], dTex, dRect[0], m_merge[0], true, m_MERGE_BLEND);
		}
	}
}

void GSDeviceMTL::DoInterlace(GSTexture* sTex, GSTexture* dTex, int shader, bool linear, float yoffset)
{
	GSScopedDebugGroupMTL dbg(m_cmdBuffer, @"DoInterlace");

	GSVector4 s = GSVector4(dTex->GetSize());

	GSVector4 sRect(0, 0, 1, 1);
	GSVector4 dRect(0.f, yoffset, s.x, s.y + yoffset);

	InterlaceFragShaderUniform cb = {0};
	cb.ZrH = {0, 1.f / s.y};
	cb.hH = s.y / 2;

	StretchRect(sTex, sRect, dTex, dRect, m_interlace[shader], linear, 0, MTLColorWriteMaskAll, &cb, sizeof(cb));
}

id<MTLFunction> GSDeviceMTL::CompileVS(VSSelector sel)
{
	NSError* err = nil;

	auto constants = [MTLFunctionConstantValues new];
	bool fst = sel.fst;
	bool iip = sel.iip;
	[constants setConstantValue:&fst type:MTLDataTypeBool atIndex:GSMTLConstantIndex_FST];
	[constants setConstantValue:&iip type:MTLDataTypeBool atIndex:GSMTLConstantIndex_IIP];

	id<MTLFunction> shader = [m_shaders newFunctionWithName:@"vs_main" constantValues:constants error:&err];
	NSERROR_CHECK(err, @"Metal: Failed to create vertex shader with selector %x: %@", sel.key, err.localizedDescription);

	return shader;
}

id<MTLFunction> GSDeviceMTL::CompilePS(PSSelector sel)
{
	auto constants = [MTLFunctionConstantValues new];
	auto setI = [&](unsigned int value, GSMTLConstantIndex idx)
	{
		[constants setConstantValue:&value type:MTLDataTypeUInt atIndex:idx];
	};
	auto setB = [&](bool value, GSMTLConstantIndex idx)
	{
		[constants setConstantValue:&value type:MTLDataTypeBool atIndex:idx];
	};

	setI(sel.tex_fmt,   GSMTLConstantIndex_PS_TEX_FMT);
	setI(sel.dfmt,      GSMTLConstantIndex_PS_DFMT);
	setI(sel.depth_fmt, GSMTLConstantIndex_PS_DEPTH_FMT);
	setB(sel.aem,       GSMTLConstantIndex_PS_AEM);
	setB(sel.fba,       GSMTLConstantIndex_PS_FBA);
	setB(sel.fog,       GSMTLConstantIndex_PS_FOG);
	setB(sel.iip,       GSMTLConstantIndex_IIP);
	setI(sel.date,      GSMTLConstantIndex_PS_DATE);
	setI(sel.atst,      GSMTLConstantIndex_PS_ATST);
	setB(sel.fst,       GSMTLConstantIndex_FST);
	setI(sel.tfx,       GSMTLConstantIndex_PS_TFX);
	setB(sel.tcc,       GSMTLConstantIndex_PS_TCC);
	setI(sel.wms,       GSMTLConstantIndex_PS_WMS);
	setI(sel.wmt,       GSMTLConstantIndex_PS_WMT);
	setB(sel.ltf,       GSMTLConstantIndex_PS_LTF);
	setB(sel.shuffle,   GSMTLConstantIndex_PS_SHUFFLE);
	setB(sel.read_ba,   GSMTLConstantIndex_PS_READ_BA);
	setB(sel.write_rg,  GSMTLConstantIndex_PS_WRITE_RG);
	setB(sel.fbmask,    GSMTLConstantIndex_PS_FBMASK);
	setI(sel.blend_a,   GSMTLConstantIndex_PS_BLEND_A);
	setI(sel.blend_b,   GSMTLConstantIndex_PS_BLEND_B);
	setI(sel.blend_c,   GSMTLConstantIndex_PS_BLEND_C);
	setI(sel.blend_d,   GSMTLConstantIndex_PS_BLEND_D);
	setB(sel.clr1,      GSMTLConstantIndex_PS_CLR1);
	setB(sel.colclip,   GSMTLConstantIndex_PS_COLCLIP);
	setI(sel.channel,   GSMTLConstantIndex_PS_CHANNEL_FETCH);
	setI(sel.dither,    GSMTLConstantIndex_PS_DITHER);
	setB(sel.zclamp,    GSMTLConstantIndex_PS_ZCLAMP);
	setB(sel.interlock, GSMTLConstantIndex_PS_INTERLOCK);

	NSError* err = nil;
	id<MTLFunction> shader = [m_shaders newFunctionWithName:@"ps_main" constantValues:constants error:&err];
	NSERROR_CHECK(err, @"Metal: Failed to create fragment shader with selector %llx: %@", sel.key, err.localizedDescription);

	return shader;
}

id<MTLRenderPipelineState> GSDeviceMTL::GetPipeline(VSSelector vs_sel, PSSelector ps_sel)
{
	auto idx = m_pipelines.find(ps_sel.key);
	if (idx != m_pipelines.end())
		return idx->second;

	PSSelector ps_sel_actual;

	if (!ps_sel.sw_blend)
	{
		ps_sel_actual.blend_a = 0;
		ps_sel_actual.blend_b = 0;
		ps_sel_actual.blend_c = 0;
		ps_sel_actual.blend_d = 0;
	}

	id<MTLFunction> vs = m_vs[vs_sel.key];
	id<MTLFunction> ps = CompilePS(ps_sel_actual);

	auto desc = [MTLRenderPipelineDescriptor new];
	desc.label = [NSString stringWithFormat:@"Main PS %llx", ps_sel.key];
	desc.vertexFunction = vs;
	desc.fragmentFunction = ps;
	// TODO: Attachments

	NSError* err = nil;
	auto pipeline = [m_dev newRenderPipelineStateWithDescriptor:desc error:&err];
	NSERROR_CHECK(err, @"Metal: Failed to create main shader pipeline with vs selector %x, ps selector %llx: %@", vs_sel.key, ps_sel.key, err.localizedDescription);

	m_pipelines[ps_sel.key] = pipeline;

	return pipeline;
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

void GSDeviceMTL::PSSetShaderResources(GSTexture* sr0, GSTexture* sr1)
{
	fprintf(stderr, "Metal: PSSetShaderResources unimplemented\n");
}
void GSDeviceMTL::PSSetShaderResource(int i, GSTexture* sRect)
{
	fprintf(stderr, "Metal: PSSetShaderResource unimplemented\n");
}
void GSDeviceMTL::OMSetRenderTargets(GSTexture* rt, GSTexture* ds, const GSVector4i* scissor)
{
	fprintf(stderr, "Metal: OMSetRenderTargets unimplemented\n");
}

#endif
