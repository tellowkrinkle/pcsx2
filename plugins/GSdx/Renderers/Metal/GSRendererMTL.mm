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
#include "GSRendererMTL.h"
#include "GSMetalShims.h"
#include "GSTextureMTL.h"

#if ! __has_feature(objc_arc)
#error "Compile this with -fobjc-arc"
#endif

#ifdef __APPLE__

// TODO: Texture cache
GSRendererMTL::GSRendererMTL(): GSRendererHW(nullptr)
{
	m_rdesc = [MTLRenderPassDescriptor new];

	m_rdesc.colorAttachments[0].loadAction = MTLLoadActionLoad;
	m_rdesc.colorAttachments[0].storeAction = MTLStoreActionStore;
	m_rdesc.depthAttachment.loadAction = MTLLoadActionLoad;
	m_rdesc.depthAttachment.storeAction = MTLStoreActionStore;

	m_rdesc_interlock = [m_rdesc copy];

	// Color attachment is just used to keep Metal happy if there's no depth, we render to a read-write texture
	m_rdesc_interlock.colorAttachments[0].loadAction = MTLLoadActionDontCare;
	m_rdesc_interlock.colorAttachments[0].storeAction = MTLStoreActionDontCare;
	m_rdesc_interlock.depthAttachment.loadAction = MTLLoadActionLoad;
	m_rdesc_interlock.depthAttachment.storeAction = MTLStoreActionStore;

	m_rdesc_interlock_depth = [m_rdesc_interlock copy];



	m_sw_blending = theApp.GetConfigI("accurate_blending_unit");

	if (theApp.GetConfigB("UserHacks"))
		UserHacks_tri_filter = static_cast<TriFiltering>(theApp.GetConfigI("UserHacks_TriFilter"));
	else
		UserHacks_tri_filter = TriFiltering::None;
}

void GSRendererMTL::ResetStates()
{
	m_sel = Selector();
	m_sampler_sel = {};
	m_vs_cb = {0};
	m_ps_cb = {0};
}

void GSRendererMTL::SetupIA(float sx, float sy)
{
	switch (m_vt.m_primclass)
	{
		case GS_POINT_CLASS:
			m_primclass = MTLPrimitiveTypePoint;
			break;
		case GS_LINE_CLASS:
			m_primclass = MTLPrimitiveTypeLine;
			break;
		case GS_SPRITE_CLASS:
			Lines2Sprites();
			m_primclass = MTLPrimitiveTypeTriangle;
			break;
		case GS_TRIANGLE_CLASS:
			m_primclass = MTLPrimitiveTypeTriangle;
			break;
		default:
			__assume(0);
	}
}

void GSRendererMTL::EmulateAtst(int pass, const GSTextureCache::Source* tex)
{
	static constexpr uint32 inverted_atst[] = {ATST_ALWAYS, ATST_NEVER, ATST_GEQUAL, ATST_GREATER, ATST_NOTEQUAL, ATST_LESS, ATST_LEQUAL, ATST_EQUAL};

	int atst = (pass == 2) ? inverted_atst[m_context->TEST.ATST] : m_context->TEST.ATST;

	if (!m_context->TEST.ATE)
		return;

	switch (atst)
	{
		case ATST_LESS:
			m_ps_cb.aref = (float)m_context->TEST.AREF - 0.1f;
			m_sel.ps.atst = 1;
			break;
		case ATST_LEQUAL:
			m_ps_cb.aref = (float)m_context->TEST.AREF - 0.1f + 1.f;
			m_sel.ps.atst = 1;
			break;
		case ATST_GEQUAL:
			m_ps_cb.aref = (float)m_context->TEST.AREF - 0.1f;
			m_sel.ps.atst = 2;
			break;
		case ATST_GREATER:
			m_ps_cb.aref = (float)m_context->TEST.AREF - 0.1f + 1.f;
			m_sel.ps.atst = 2;
			break;
		case ATST_EQUAL:
			m_ps_cb.aref = (float)m_context->TEST.AREF;
			m_sel.ps.atst = 3;
			break;
		case ATST_NOTEQUAL:
			m_ps_cb.aref = (float)m_context->TEST.AREF;
			m_sel.ps.atst = 4;
			break;
		case ATST_NEVER: // Draw won't be done so no need to implement it in shader
		case ATST_ALWAYS:
		default:
			m_sel.ps.atst = 0;
			break;
	}
}

void GSRendererMTL::EmulateZbuffer()
{
	if (m_context->TEST.ZTE)
	{
		m_sel.ds.ztst = m_context->TEST.ZTST;
		m_sel.ds.zwe = !m_context->ZBUF.ZMSK;
	}
	else
	{
		m_sel.ds.ztst = ZTST_ALWAYS;
	}

	// On the real GS we appear to do clamping on the max z value the format allows.
	// Clamping is done after rasterization.
	const uint32 max_z = 0xFFFFFFFF >> (GSLocalMemory::m_psm[m_context->ZBUF.PSM].fmt * 8);
	const bool clamp_z = (uint32)(GSVector4i(m_vt.m_max.p).z) > max_z;

	m_vs_cb.max_depth = 0xFFFFFFFF;
	m_ps_cb.max_depth = 1.f;

	if (clamp_z)
	{
		if (m_vt.m_primclass == GS_SPRITE_CLASS || m_vt.m_primclass == GS_POINT_CLASS)
		{
			m_vs_cb.max_depth = max_z;
		}
		else
		{
			m_ps_cb.max_depth = max_z * 0x1p-32f;
			m_sel.ps.zclamp = 1;
		}
	}

	// TODO: GL corner case optimization?
}

void GSRendererMTL::EmulateTextureShuffleAndFbmask()
{
	m_texture_shuffle = false;
	if (m_texture_shuffle)
	{
		// TODO: Texture shuffle emulation
	}
	else
	{
		m_sel.ps.dfmt = GSLocalMemory::m_psm[m_context->FRAME.PSM].fmt;
		m_ps_cb.fb_mask = m_context->FRAME.FBMSK;
		GSVector4i fbmask_v = GSVector4i::load((int)m_context->FRAME.FBMSK);
		int ff_fbmask = fbmask_v.eq8(GSVector4i::xffffffff()).mask();
		int zero_fbmask = fbmask_v.eq8(GSVector4i::zero()).mask();

		m_sel.c.wrgba = ~ff_fbmask; // Enable channel if at least 1 bit is 0

		if (~ff_fbmask & ~zero_fbmask & 0xF)
		{
			m_sel.ps.sw_blend = 1;
			m_sel.ps.interlock = 1;
		}
	}
}

void GSRendererMTL::EmulateChannelShuffle(GSTexture** rt, const GSTextureCache::Source* tex)
{
	// TODO: Texture shuffle emulation
}

void GSRendererMTL::EmulateBlending()
{
	const GIFRegALPHA& ALPHA = m_context->ALPHA;
	m_ps_cb.alpha_fix = (float)ALPHA.FIX / 128.f;
	if (!(PRIM->ABE || PRIM->AA1 && m_vt.m_primclass == GS_LINE_CLASS))
	{
		m_sel.ps.blend_a = 0;
		m_sel.ps.blend_b = 0;
		m_sel.ps.blend_c = 0;
		m_sel.ps.blend_d = 0;
	}
	else
	{
		m_sel.ps.blend_a = ALPHA.A;
		m_sel.ps.blend_b = ALPHA.B;
		m_sel.ps.blend_c = ALPHA.C;
		m_sel.ps.blend_d = ALPHA.D;
	}

	const uint8 blend_index  = uint8(((ALPHA.A * 3 + ALPHA.B) * 3 + ALPHA.C) * 3 + ALPHA.D);
	const int blend_flag = m_dev->GetBlendFlags(blend_index);

	// SW Blend is (nearly) free. Let's use it.
	const bool impossible_or_free_blend = (blend_flag & (BLEND_NO_REC|BLEND_A_MAX|BLEND_ACCU)) // Blend doesn't requires the costly barrier
		// TODO: prim overlap check
		|| (m_sel.ps.interlock);           // Another effect (for example fbmask) already requires a full barrier

	// TODO: Accumulation blend

	// Blending doesn't require barrier, or sampling of the rt
	const bool blend_non_recursive = !!(blend_flag & BLEND_NO_REC);

	switch (m_sw_blending)
	{
		case ACC_BLEND_ULTRA:
			m_sel.ps.sw_blend = 1;
			[[fallthrough]];
		case ACC_BLEND_FULL:
			if (!m_vt.m_alpha.valid && (ALPHA.C == 0)) GetAlphaMinMax();
			m_sel.ps.sw_blend |= (ALPHA.A != ALPHA.B) && ((ALPHA.C == 0 && m_vt.m_alpha.max > 128) || (ALPHA.C == 2 && ALPHA.FIX > 128u));
			[[fallthrough]];
		case ACC_BLEND_HIGH:
			m_sel.ps.sw_blend |= (ALPHA.C == 1);
			[[fallthrough]];
		case ACC_BLEND_MEDIUM:
			// TODO: Medium
			[[fallthrough]];
		case ACC_BLEND_BASIC:
			m_sel.ps.sw_blend |= impossible_or_free_blend;
			[[fallthrough]];
		default:
			break;
	}

	if (m_env.COLCLAMP.CLAMP == 0)
	{
		m_sel.ps.colclip = 1;
		m_sel.ps.sw_blend = 1;
	}

	if (m_sel.ps.sw_blend)
	{
		m_sel.ps.interlock |= !blend_non_recursive;
	}
	else
	{
		m_sel.ps.clr1 = !!(blend_flag & BLEND_C_CLR);
		if (m_sel.ps.dfmt == 1 && m_sel.ps.blend_c == 1)
		{
			// 24 bits doesn't have an alpha channel so use 1.0f fix factor as equivalent
			m_sel.ps.blend_c = 2;
			m_ps_cb.alpha_fix = 1;
		}
	}
}

void GSRendererMTL::EmulateTextureSampler(const GSTextureCache::Source* tex)
{
	// Warning fetch the texture PSM format rather than the context format. The latter could have been corrected in the texture cache for depth.
	//const GSLocalMemory::psm_t &psm = GSLocalMemory::m_psm[m_context->TEX0.PSM];
	const GSLocalMemory::psm_t &psm = GSLocalMemory::m_psm[tex->m_TEX0.PSM];
	const GSLocalMemory::psm_t &cpsm = psm.pal > 0 ? GSLocalMemory::m_psm[m_context->TEX0.CPSM] : psm;

	const uint8 wms = m_context->CLAMP.WMS;
	const uint8 wmt = m_context->CLAMP.WMT;
	bool complex_wms_wmt = !!((wms | wmt) & 2);

	bool need_mipmap = IsMipMapDraw();
	bool shader_emulated_sampler = tex->m_palette || cpsm.fmt != 0 || complex_wms_wmt || psm.depth;
	bool trilinear_manual = need_mipmap && m_mipmap == 2;

	bool bilinear = m_vt.IsLinear();
	int trilinear = 0;
	bool trilinear_auto = false;
	switch (UserHacks_tri_filter)
	{
		case TriFiltering::Forced:
			trilinear = static_cast<uint8>(GS_MIN_FILTER::Linear_Mipmap_Linear);
			trilinear_auto = m_mipmap != 2;
			break;

		case TriFiltering::PS2:
			if (need_mipmap && m_mipmap != 2) {
				trilinear = m_context->TEX1.MMIN;
				trilinear_auto = true;
			}
			break;

		case TriFiltering::None:
		default:
			break;
	}

	// 1 and 0 are equivalent
	m_sel.ps.wms = (wms & 2) ? wms : 0;
	m_sel.ps.wmt = (wmt & 2) ? wmt : 0;

	// Depth + bilinear filtering isn't done yet (And I'm not sure we need it anyway but a game will prove me wrong)
	// So of course, GTA set the linear mode, but sampling is done at texel center so it is equivalent to nearest sampling
	ASSERT(!(psm.depth && m_vt.IsLinear()));

	if (m_sel.ps.shuffle)
	{
		// TODO: Texture shuffle
	}
	else if (tex->m_target)
	{
		// Use an old target. AEM and index aren't resolved it must be done
		// on the GPU

		// Select the 32/24/16 bits color (AEM)
		m_sel.ps.tex_fmt = cpsm.fmt;
		m_sel.ps.aem     = m_env.TEXA.AEM;

		GSVector4 ta(m_env.TEXA & GSVector4i::x000000ff());
		ta /= 255.f;
		// FIXME rely on compiler for the optimization
		m_ps_cb.ta.x = ta.x;
		m_ps_cb.ta.y = ta.y;

		if (tex->m_palette)
		{
			// FIXME Potentially improve fmt field in GSLocalMemory
			if (m_context->TEX0.PSM == PSM_PSMT4HL)
				m_sel.ps.tex_fmt |= 1 << 2;
			else if (m_context->TEX0.PSM == PSM_PSMT4HH)
				m_sel.ps.tex_fmt |= 2 << 2;
			else
				m_sel.ps.tex_fmt |= 3 << 2;

			// Alpha channel of the RT is reinterpreted as an index. Star
			// Ocean 3 uses it to emulate a stencil buffer.  It is a very
			// bad idea to force bilinear filtering on it.
			bilinear &= m_vt.IsLinear();
		}

		// Depth format
		if (tex->m_texture->GetType() == GSTexture::Type::DepthStencil)
		{
			// Require a float conversion if the texure is a depth format
			m_sel.ps.depth_fmt = (psm.bpp == 16) ? 2 : 1;

			// Don't force interpolation on depth format
			bilinear &= m_vt.IsLinear();
		}
		else if (psm.depth)
		{
			// Use Integral scaling
			m_sel.ps.depth_fmt = 3;

			// Don't force interpolation on depth format
			bilinear &= m_vt.IsLinear();
		}

		auto texoff = RealignTargetTextureCoordinate(tex);
		m_vs_cb.texture_offset = { texoff.x, texoff.y };
	}
	else if (tex->m_palette)
	{
		// Use a standard 8 bits texture. AEM is already done on the CLUT
		// Therefore you only need to set the index
		// m_ps_sel.aem     = 0; // removed as an optimization

		// Note 4 bits indexes are converted to 8 bits
		m_sel.ps.tex_fmt = 3 << 2;
	}
	else
	{
		// Standard texture. Both index and AEM expansion were already done by the CPU.
		// m_ps_sel.tex_fmt = 0; // removed as an optimization
		// m_ps_sel.aem     = 0; // removed as an optimization
	}

	if (m_context->TEX0.TFX == TFX_MODULATE && m_vt.m_eq.rgba == 0xFFFF && m_vt.m_min.c.eq(GSVector4i(128)))
	{
		// Micro optimization that reduces GPU load (removes 5 instructions on the FS program)
		m_sel.ps.tfx = TFX_DECAL;
	}
	else
	{
		m_sel.ps.tfx = m_context->TEX0.TFX;
	}

	m_sel.ps.tcc = m_context->TEX0.TCC;

	m_sel.ps.ltf = bilinear && shader_emulated_sampler;
	m_sel.ps.fst = !!PRIM->FST;
	m_sel.vs.fst = !!PRIM->FST;

	int w = tex->m_texture->GetWidth();
	int h = tex->m_texture->GetHeight();
	int tw = 1 << m_context->TEX0.TW;
	int th = 1 << m_context->TEX0.TH;

	m_ps_cb.wh = simd_make_float4(tw, th, w, h);
	m_ps_cb.half_texel = simd_make_float2(-0.5f, 0.5f).xxyy / m_ps_cb.wh.zwzw;

	if (complex_wms_wmt)
	{
		m_ps_cb.msk_fix = { m_context->CLAMP.MINU, (uint32)m_context->CLAMP.MINV, m_context->CLAMP.MAXU, m_context->CLAMP.MAXV };
		m_ps_cb.min_max = vector_float4(m_ps_cb.msk_fix) / m_ps_cb.wh.xyxy;
	}
	else if (trilinear_manual)
	{
		// Reuse MinMax for mipmap parameter to avoid an extension of the UBO
		m_ps_cb.min_max.x = float(m_context->TEX1.K / 16.f);
		m_ps_cb.min_max.y = float(1 << m_context->TEX1.L);
		m_ps_cb.min_max.z = float(m_lod.x); // Offset because first layer is m_lod, dunno if we can do better
		m_ps_cb.min_max.w = float(m_lod.y);
	}
	else if (trilinear_auto)
	{
		tex->m_texture->GenerateMipmap();
	}

	// TODO: TC Offset Hack

	// Must be done after all coordinates math
	if (m_context->HasFixedTEX0() && !PRIM->FST)
	{
		m_sel.ps.invalid_tex0 = 1;
		// Use invalid size to denormalize ST coordinate
		m_ps_cb.wh.x = float(1 << m_context->stack.TEX0.TW);
		m_ps_cb.wh.y = float(1 << m_context->stack.TEX0.TH);

		// We can't handle m_target with invalid_tex0 atm due to upscaling
		ASSERT(!tex->m_target);
	}

	// Only enable clamping in CLAMP mode. REGION_CLAMP will be done manually in the shader
	m_sampler_sel.tau = (wms != CLAMP_CLAMP);
	m_sampler_sel.tav = (wmt != CLAMP_CLAMP);
	if (shader_emulated_sampler)
	{
		m_sampler_sel.biln  = 0;
		m_sampler_sel.aniso = 0;
		m_sampler_sel.triln = 0;
	}
	else
	{
		m_sampler_sel.biln  = bilinear;
		// Enable aniso only for triangles. Sprites are flat so aniso is likely useless (it would save perf for others primitives).
		m_sampler_sel.aniso = m_vt.m_primclass == GS_TRIANGLE_CLASS ? 1 : 0;
		m_sampler_sel.triln = trilinear;
		if (trilinear_manual)
			m_sel.ps.manual_lod = 1;
		else if (trilinear_auto)
			m_sel.ps.automatic_lod = 1;
	}
}

void GSRendererMTL::DrawPrims(GSTexture* rt, GSTexture* ds, GSTextureCache::Source* tex)
{
	const GSVector2i& rtsize = ds ? ds->GetSize()  : rt->GetSize();
	const GSVector2& rtscale = ds ? ds->GetScale() : rt->GetScale();

	bool DATE = m_context->TEST.DATE && m_context->FRAME.PSM != PSM_PSMCT24;

	ResetStates();
	m_vs_cb.texture_offset = { 0, 0 };

	// HLE implementation of the channel selection effect
	//
	// Warning it must be done at the begining because it will change the
	// vertex list (it will interact with PrimitiveOverlap and accurate
	// blending)
	EmulateChannelShuffle(&rt, tex);

	// Upscaling hack to avoid various line/grid issues
	MergeSprite(tex);

	// Detect framebuffer read that will need special handling
	if ((m_context->FRAME.Block() == m_context->TEX0.TBP0) && PRIM->TME && m_sw_blending)
	{
		// This pattern is used by several games to emulate a stencil (shadow)
		// Ratchet & Clank, Jak do alpha integer multiplication (tfx) which is mostly equivalent to +1/-1
		// Tri-Ace (Star Ocean 3/RadiataStories/VP2) uses a palette to handle the +1/-1
		m_sel.ps.tex_is_fb = 1;
		m_sel.ps.interlock = 1;
	}

	if (DATE)
	{
		m_sel.ps.interlock = 1;
		m_sel.ps.date = 1 + m_context->TEST.DATM;
	}

	m_dev->BeginScene();

	// om

	EmulateZbuffer(); // will update VS depth mask

	// vs

	// FIXME D3D11 and GL support half pixel center. Code could be easier!!!
	float sx = 2.f * rtscale.x / (rtsize.x << 4);
	float sy = 2.f * rtscale.y / (rtsize.y << 4);
	float ox = float(int(m_context->XYOFFSET.OFX));
	float oy = float(int(m_context->XYOFFSET.OFY));
	float ox2 = -1.f / rtsize.x;
	float oy2 = -1.f / rtsize.y;

	//This hack subtracts around half a pixel from OFX and OFY.
	//
	//The resulting shifted output aligns better with common blending / corona / blurring effects,
	//but introduces a few bad pixels on the edges.
	if (rt && rt->LikelyOffset && m_userHacks_HPO == 1)
	{
		ox2 *= rt->OffsetHack_modx;
		oy2 *= rt->OffsetHack_mody;
	}

	m_vs_cb.vertex_scale = {sx, -sy};
	m_vs_cb.vertex_offset = { ox * sx + ox2 + 1, -(oy * sy + oy2 + 1) };

	// GS_SPRITE_CLASS are already flat (either by CPU or the GS)
	m_sel.ps.iip = (m_vt.m_primclass == GS_SPRITE_CLASS) ? 1 : PRIM->IIP;
	m_sel.vs.iip = m_sel.ps.iip;

	m_sel.ps.fba = m_context->FBA.FBA;
	m_sel.ps.dither = m_dithering > 0 && m_sel.ps.dfmt == 2 && m_env.DTHE.DTHE;

	if (m_sel.ps.dither)
	{
		m_sel.ps.dither = m_dithering;
		m_ps_cb.dither_matrix =
		{
			simd_make_float4(m_env.DIMX.DM00, m_env.DIMX.DM01, m_env.DIMX.DM02, m_env.DIMX.DM03),
			simd_make_float4(m_env.DIMX.DM10, m_env.DIMX.DM11, m_env.DIMX.DM12, m_env.DIMX.DM13),
			simd_make_float4(m_env.DIMX.DM20, m_env.DIMX.DM21, m_env.DIMX.DM22, m_env.DIMX.DM23),
			simd_make_float4(m_env.DIMX.DM30, m_env.DIMX.DM31, m_env.DIMX.DM32, m_env.DIMX.DM33),
		};
	}

	if (PRIM->FGE)
	{
		m_sel.ps.fog = 1;
		const auto& c = m_env.FOGCOL;
		m_ps_cb.fog_color = simd_make_float3(c.FCR, c.FCG, c.FCB);
	}

	EmulateAtst(1, tex);

	if (tex)
		EmulateTextureSampler(tex);
	else
		m_sel.ps.tfx = 4;

	// rs
	const GSVector4& hacked_scissor = m_channel_shuffle ? GSVector4(0, 0, 1024, 1024) : m_context->scissor.in;
	GSVector4i scissor = GSVector4i(GSVector4(rtscale).xyxy() * hacked_scissor).rintersect(GSVector4i(rtsize).zwxy());

	SetupIA(sx, sy);

	GSTextureMTL* mrt = nullptr;
	GSTextureMTL* mds = nullptr;
	if (rt)
		mrt = static_cast<GSTextureMTL*>(rt);
	if (ds)
		mds = static_cast<GSTextureMTL*>(ds);

	SendDraw(mrt, mds, tex);
}

void GSRendererMTL::SendDraw(GSTextureMTL* rt, GSTextureMTL* ds, GSTextureCache::Source* tex)
{
	GSDeviceMTL* dev = static_cast<GSDeviceMTL*>(m_dev);
	id<MTLDevice> mtldev = dev->MTLDevice();
	id<MTLCommandBuffer> cmdbuf = dev->CmdBuffer();

	id<MTLRenderCommandEncoder> enc;
	if (m_sel.ps.interlock)
	{
		// TODO: Color clear
		if (ds)
		{
			m_rdesc_interlock.colorAttachments[0].texture = nil;
			m_rdesc_interlock.depthAttachment.texture = ds->GetTexture();
			ds->ApplyDepthLoadAction(m_rdesc_interlock, MTLLoadActionLoad);
			enc = [cmdbuf renderCommandEncoderWithDescriptor:m_rdesc_interlock_depth];
		}
		else
		{
			m_rdesc_interlock.colorAttachments[0].texture = nil;
			GSVector2i size = rt->GetSize();
			GSVector2i cur_size = GSVector2i(m_dummy_tex.width, m_dummy_tex.height);
			if (cur_size.x < size.x || cur_size.y < size.y)
			{
				int newWidth = std::max(cur_size.x, size.x);
				int newHeight = std::max(cur_size.y, size.y);
				MTLTextureDescriptor* texdesc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatR8Unorm width:newWidth height:newHeight mipmapped:NO];
				texdesc.usage = MTLTextureUsageRenderTarget;
				texdesc.storageMode = MTLStorageModePrivate;
				m_dummy_tex = [mtldev newTextureWithDescriptor:texdesc];
				m_rdesc_interlock.colorAttachments[0].texture = m_dummy_tex;
			}
			if (@available(macOS 10.15, *))
			{
				m_rdesc_interlock.renderTargetWidth = size.x;
				m_rdesc_interlock.renderTargetHeight = size.y;
				enc = [cmdbuf renderCommandEncoderWithDescriptor:m_rdesc_interlock];
			}
			else
			{
				enc = [cmdbuf renderCommandEncoderWithDescriptor:m_rdesc_interlock];
				[enc setViewport:(MTLViewport){0, 0, (double)size.x, (double)size.y}];
			}

		}
	}
	else
	{
		m_rdesc.colorAttachments[0].texture = rt->GetTexture();
		rt->ApplyColorLoadAction(m_rdesc, MTLLoadActionLoad);
		if (ds)
		{
			m_rdesc.depthAttachment.texture = ds->GetTexture();
			ds->ApplyDepthLoadAction(m_rdesc, MTLLoadActionLoad);
		}
		enc = [cmdbuf renderCommandEncoderWithDescriptor:m_rdesc];
	}

	[enc setRenderPipelineState:dev->GetPipeline(m_sel.vs, m_sel.ps)];

	[enc setFragmentTexture:rt->GetTexture() atIndex:GSMTLTextureIndexRenderTarget];
	if (ds)
		[enc setFragmentTexture:ds->GetTexture() atIndex:GSMTLTextureIndexDepth];
	if (tex)
	{
		[enc setFragmentTexture:static_cast<GSTextureMTL*>(tex->m_texture)->GetTexture()
		                atIndex:GSMTLTextureIndexTex];
		[enc setFragmentTexture:static_cast<GSTextureMTL*>(tex->m_palette)->GetTexture()
		                atIndex:GSMTLTextureIndexPalette];
	}

	size_t vb_size = m_vertex.next * sizeof(*m_vertex.buff);
	size_t ib_size = m_index.tail * sizeof(*m_index.buff);
	static_assert(sizeof(GSVertex) == sizeof(GSMTLMainVertex), "These need to have the same layout");
	void* buf = dev->MainBuffer().Map(cmdbuf, vb_size + ib_size);
	memcpy(buf, m_vertex.buff, vb_size);
	memcpy((void*)(ib_size + (uptr)buf), m_index.buff, ib_size);
	auto data = dev->MainBuffer().Unmap();

	[enc setVertexBuffer:data.first offset:data.second atIndex:GSMTLIndexVertices];
	[enc drawIndexedPrimitives:m_primclass
	                indexCount:m_index.tail
	                 indexType:MTLIndexTypeUInt32
	               indexBuffer:data.first
	         indexBufferOffset:data.second + vb_size];
}

#endif
