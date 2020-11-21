/*
 *	Copyright (C) 2007-2009 Gabest
 *	http://www.gabest.org
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
#include "GSVertexTrace.h"
#include "GSUtil.h"
#include "GSState.h"

GSVector4 GSVertexTrace::s_minmax;

void GSVertexTrace::InitVectors()
{
	s_minmax = GSVector4(FLT_MAX, -FLT_MAX);
}

GSVertexTrace::GSVertexTrace(const GSState* state)
	: m_accurate_stq(false), m_state(state), m_primclass(GS_INVALID_CLASS)
{
	m_force_filter = static_cast<BiFiltering>(theApp.GetConfigI("filter"));
	memset(&m_alpha, 0, sizeof(m_alpha));

	#define InitUpdate3(P, IIP, TME, FST, COLOR) \
		m_fmm[COLOR][FST][TME][IIP][P] = &GSVertexTrace::FindMinMax<P, IIP, TME, FST, COLOR>;

	#define InitUpdate2(P, IIP, TME) \
		InitUpdate3(P, IIP, TME, 0, 0) \
		InitUpdate3(P, IIP, TME, 0, 1) \
		InitUpdate3(P, IIP, TME, 1, 0) \
		InitUpdate3(P, IIP, TME, 1, 1) \

	#define InitUpdate(P) \
		InitUpdate2(P, 0, 0) \
		InitUpdate2(P, 0, 1) \
		InitUpdate2(P, 1, 0) \
		InitUpdate2(P, 1, 1) \

	InitUpdate(GS_POINT_CLASS);
	InitUpdate(GS_LINE_CLASS);
	InitUpdate(GS_TRIANGLE_CLASS);
	InitUpdate(GS_SPRITE_CLASS);
}

void GSVertexTrace::Update(const void* vertex, const uint32* index, int v_count, int i_count, GS_PRIM_CLASS primclass)
{
	m_primclass = primclass;

	uint32 iip = m_state->PRIM->IIP;
	uint32 tme = m_state->PRIM->TME;
	uint32 fst = m_state->PRIM->FST;
	uint32 color = !(m_state->PRIM->TME && m_state->m_context->TEX0.TFX == TFX_DECAL && m_state->m_context->TEX0.TCC);

	(this->*m_fmm[color][fst][tme][iip][primclass])(vertex, index, v_count, i_count);

	// Potential float overflow detected. Better uses the slower division instead
	// Note: If Q is too big, 1/Q will end up as 0. 1e30 is a random number
	// that feel big enough.
	if (!fst && !m_accurate_stq && m_min.t.z > 1e30) {
		fprintf(stderr, "Vertex Trace: float overflow detected ! min %e max %e\n", m_min.t.z, m_max.t.z);
		m_accurate_stq = true;
	}

	m_eq.value = (m_min.c == m_max.c).mask() | ((m_min.p == m_max.p).mask() << 16) | ((m_min.t == m_max.t).mask() << 20);

	m_alpha.valid = false;

	// I'm not sure of the cost. In doubt let's do it only when depth is enabled
	if(m_state->m_context->TEST.ZTE == 1 && m_state->m_context->TEST.ZTST > ZTST_ALWAYS) {
		CorrectDepthTrace(vertex, v_count);
	}

	if(m_state->PRIM->TME)
	{
		const GIFRegTEX1& TEX1 = m_state->m_context->TEX1;

		m_filter.mmag = TEX1.IsMagLinear();
		m_filter.mmin = TEX1.IsMinLinear();

		if(TEX1.MXL == 0) // MXL == 0 => MMIN ignored, tested it on ps2
		{
			m_filter.linear = m_filter.mmag;
		}
		else
		{
			float K = (float)TEX1.K / 16;

			if(TEX1.LCM == 0 && m_state->PRIM->FST == 0) // FST == 1 => Q is not interpolated
			{
				// LOD = log2(1/|Q|) * (1 << L) + K

				GSVector4::storel(&m_lod, m_max.t.uph(m_min.t).log2(3).neg() * (float)(1 << TEX1.L) + K);

				if(m_lod.x > m_lod.y) {float tmp = m_lod.x; m_lod.x = m_lod.y; m_lod.y = tmp;}
			}
			else
			{
				m_lod.x = K;
				m_lod.y = K;
			}

			if(m_lod.y <= 0)
			{
				m_filter.linear = m_filter.mmag;
			}
			else if(m_lod.x > 0)
			{
				m_filter.linear = m_filter.mmin;
			}
			else
			{
				m_filter.linear = m_filter.mmag | m_filter.mmin;
			}
		}

		switch (m_force_filter)
		{
			case BiFiltering::Nearest:
				m_filter.opt_linear = 0;
				break;

			case BiFiltering::Forced_But_Sprite:
				// Special case to reduce the number of glitch when upscaling is enabled
				m_filter.opt_linear = (m_primclass == GS_SPRITE_CLASS) ? m_filter.linear : 1;
				break;

			case BiFiltering::Forced:
				m_filter.opt_linear = 1;
				break;

			case BiFiltering::PS2:
			default:
				m_filter.opt_linear = m_filter.linear;
				break;
		}
	}
}

template<GS_PRIM_CLASS primclass, uint32 iip, uint32 tme, uint32 fst, uint32 color>
void GSVertexTrace::FindMinMax(const void* vertex, const uint32* index, int v_count, int i_count)
{
	const GSDrawingContext* context = m_state->m_context;

	int n = 1;

	switch(primclass)
	{
	case GS_POINT_CLASS:
		n = 1;
		break;
	case GS_LINE_CLASS:
	case GS_SPRITE_CLASS:
		n = 2;
		break;
	case GS_TRIANGLE_CLASS:
		n = 3;
		break;
	}

	GSVector4 tmin = s_minmax.xxxx();
	GSVector4 tmax = s_minmax.yyyy();
	GSVector4i cmin = GSVector4i::xffffffff();
	GSVector4i cmax = GSVector4i::zero();

	#if _M_SSE >= 0x401

	GSVector4i pmin = GSVector4i::xffffffff();
	GSVector4i pmax = GSVector4i::zero();

	#else

	GSVector4 pmin = s_minmax.xxxx();
	GSVector4 pmax = s_minmax.yyyy();
	
	#endif

	const GSVertex* RESTRICT v = (GSVertex*)vertex;

	for(int i = 0; i < (v_count - 1); i += 2) // 2x loop unroll
	{
		const GSVertex& v0 = v[i + 0];
		const GSVertex& v1 = v[i + 1];

		if (color)
		{
			GSVector4i c0 = GSVector4i::load(v0.RGBAQ.u32[0]);
			GSVector4i c1 = GSVector4i::load(v1.RGBAQ.u32[0]);
			if (iip || primclass == GS_POINT_CLASS)
			{
				cmin = cmin.min_u8(c0.min_u8(c1));
				cmax = cmax.max_u8(c0.max_u8(c1));
			}
			else if (primclass == GS_SPRITE_CLASS)
			{
				// Point and sprite always have indices == vertices
				// Other classes will need to use the index table here
				cmin = cmin.min_u8(c1);
				cmax = cmax.max_u8(c1);
			}
		}

		if (tme)
		{
			if (!fst)
			{
				GSVector4 stq0 = GSVector4::cast(GSVector4i(v0.m[0]));
				GSVector4 stq1 = GSVector4::cast(GSVector4i(v1.m[0]));

				GSVector4 q;
				// Sprites always have indices == vertices, so we don't have to look at the index table here
				if (primclass == GS_SPRITE_CLASS)
					q = stq1.wwww();
				else
					q = stq0.wwww(stq1);

				GSVector4 st = stq0.xyxy(stq1) / q;

				stq0 = st.xyww(primclass == GS_SPRITE_CLASS ? stq1 : stq0);
				stq1 = st.zwww(stq1);

				tmin = tmin.min(stq0.min(stq1));
				tmax = tmax.max(stq0.max(stq1));
			}
			else
			{
				GSVector4i uv0(v0.m[1]);
				GSVector4i uv1(v1.m[1]);

				GSVector4 st0 = GSVector4(uv0.uph16()).xyxy();
				GSVector4 st1 = GSVector4(uv1.uph16()).xyxy();

				tmin = tmin.min(st0.min(st1));
				tmax = tmax.max(st0.max(st1));
			}
		}

		GSVector4i xyzf0(v0.m[1]);
		GSVector4i xyzf1(v1.m[1]);

		GSVector4i xy0 = xyzf0.upl16();
		GSVector4i z0 = xyzf0.yyyy();
		GSVector4i xy1 = xyzf1.upl16();
		GSVector4i z1 = xyzf1.yyyy();

		#if _M_SSE >= 0x401

		GSVector4i p0 = xy0.blend16<0xf0>(z0.uph32(primclass == GS_SPRITE_CLASS ? xyzf1 : xyzf0));
		GSVector4i p1 = xy1.blend16<0xf0>(z1.uph32(xyzf1));

		pmin = pmin.min_u32(p0.min_u32(p1));
		pmax = pmax.max_u32(p0.max_u32(p1));

		#else

		GSVector4 p0 = GSVector4(xy0.upl64(z0.srl32(1).upl32(primclass == GS_SPRITE_CLASS ? xyzf1.wwww() : xyzf0.wwww())));
		GSVector4 p1 = GSVector4(xy1.upl64(z1.srl32(1).upl32(xyzf1.wwww())));

		pmin = pmin.min(p0.min(p1));
		pmax = pmax.max(p0.max(p1));

		#endif
	}

	if ((v_count & 1) && (primclass != GS_SPRITE_CLASS)) // Last item from unrolled loop
	{
		const GSVertex& last = v[v_count - 1];
		GSVector4i c(last.m[0]);

		if (color)
		{
			// !iip needs indices on non-point, so do it separately
			if (iip || primclass == GS_POINT_CLASS)
			{
				cmin = cmin.min_u8(c);
				cmax = cmax.max_u8(c);
			}
		}

		if (tme)
		{
			if (!fst)
			{
				GSVector4 stq = GSVector4::cast(c);

				stq = (stq.xyww() / stq.wwww()).noopt().xyww(stq);

				tmin = tmin.min(stq);
				tmax = tmax.max(stq);
			}
			else
			{
				GSVector4i uv(last.m[1]);

				GSVector4 st = GSVector4(uv.uph16()).xyxy();

				tmin = tmin.min(st);
				tmax = tmax.max(st);
			}
		}

		GSVector4i xyzf(last.m[1]);

		GSVector4i xy = xyzf.upl16();
		GSVector4i z = xyzf.yyyy();

		#if _M_SSE >= 0x401

		GSVector4i p = xy.blend16<0xf0>(z.uph32(xyzf));

		pmin = pmin.min_u32(p);
		pmax = pmax.max_u32(p);

		#else

		GSVector4 p = GSVector4(xy.upl64(z.srl32(1).upl32(xyzf.wwww())));

		pmin = pmin.min(p);
		pmax = pmax.max(p);

		#endif
	}

	if (color && !iip && primclass != GS_POINT_CLASS && primclass != GS_SPRITE_CLASS)
	{
		int i = n - 1; // Only min/max final vertices
		for (; i < (i_count - n); i += n * 2) // 2x loop unroll
		{
			GSVector4i c0 = GSVector4i::load(v[index[i + 0]].RGBAQ.u32[0]);
			GSVector4i c1 = GSVector4i::load(v[index[i + n]].RGBAQ.u32[0]);
			cmin = cmin.min_u8(c0.min_u8(c1));
			cmax = cmax.max_u8(c0.max_u8(c1));
		}
		if (i < i_count) // Last item from unrolled loop
		{
			GSVector4i c = GSVector4i::load(v[index[i]].RGBAQ.u32[0]);
			cmin = cmin.min_u8(c);
			cmax = cmax.max_u8(c);
		}
	}

	// FIXME/WARNING. A division by 2 is done on the depth on SSE<4.1 to avoid
	// negative value. However it means that we lost the lsb bit. m_eq.z could
	// be true if depth isn't constant but close enough. It also imply that
	// pmin.z & 1 == 0 and pax.z & 1 == 0

	GSVector4 o(context->XYOFFSET);
	GSVector4 s(1.0f / 16, 1.0f / 16, 2.0f, 1.0f);

	m_min.p = (GSVector4(pmin) - o) * s;
	m_max.p = (GSVector4(pmax) - o) * s;

	#if _M_SSE >= 0x401

	// Keep full precision on depth
	m_min.p = m_min.p.insert32<0, 2>(GSVector4::load((float)(uint32)pmin.extract32<2>()));
	m_max.p = m_max.p.insert32<0, 2>(GSVector4::load((float)(uint32)pmax.extract32<2>()));

	#endif

	if(tme)
	{
		if(fst)
		{
			s = GSVector4(1.0f / 16, 1.0f).xxyy();
		}
		else
		{
			s = GSVector4(1 << context->TEX0.TW, 1 << context->TEX0.TH, 1, 1);
		}

		m_min.t = tmin * s;
		m_max.t = tmax * s;
	}
	else
	{
		m_min.t = GSVector4::zero();
		m_max.t = GSVector4::zero();
	}

	if(color)
	{
		m_min.c = cmin.u8to32();
		m_max.c = cmax.u8to32();
	}
	else
	{
		m_min.c = GSVector4i::zero();
		m_max.c = GSVector4i::zero();
	}
}

void GSVertexTrace::CorrectDepthTrace(const void* vertex, int count)
{
	if (m_eq.z == 0)
		return;

	// FindMinMax isn't accurate for the depth value. Lsb bit is always 0.
	// The code below will check that depth value is really constant
	// and will update m_min/m_max/m_eq accordingly
	//
	// Really impact Xenosaga3
	//
	// Hopefully function is barely called so AVX/SSE will be useless here


	const GSVertex* RESTRICT v = (GSVertex*)vertex;
	uint32 z = v[0].XYZ.Z;

	// ought to check only 1/2 for sprite
	if (z & 1) {
		// Check that first bit is always 1
		for (int i = 0; i < count; i++) {
			z &= v[i].XYZ.Z;
		}
	} else {
		// Check that first bit is always 0
		for (int i = 0; i < count; i++) {
			z |= v[i].XYZ.Z;
		}
	}

	if (z == v[0].XYZ.Z) {
		m_eq.z = 1;
	} else {
		m_eq.z = 0;
	}
}
