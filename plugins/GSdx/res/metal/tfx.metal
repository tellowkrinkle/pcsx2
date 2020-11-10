#include "common.h"

using namespace metal;

constant bool FST [[function_constant(GSMTLConstantIndex_FST)]];
constant bool IIP [[function_constant(GSMTLConstantIndex_IIP)]];
//constant bool VS_TME [[function_constant(GSMTLConstantIndex_VS_TME)]];
constant bool PS_INTERLOCK [[function_constant(GSMTLConstantIndex_PS_INTERLOCK)]];
constant uint PS_WMS [[function_constant(GSMTLConstantIndex_PS_WMS)]];
constant uint PS_WMT [[function_constant(GSMTLConstantIndex_PS_WMT)]];
constant uint PS_TEX_FMT [[function_constant(GSMTLConstantIndex_PS_TEX_FMT)]];
constant bool PS_AEM [[function_constant(GSMTLConstantIndex_PS_AEM)]];
constant uint PS_TFX [[function_constant(GSMTLConstantIndex_PS_TFX)]];
constant bool PS_TCC [[function_constant(GSMTLConstantIndex_PS_TCC)]];
constant uint PS_ATST [[function_constant(GSMTLConstantIndex_PS_ATST)]];
constant uint PS_AFAIL [[function_constant(GSMTLConstantIndex_PS_AFAIL)]];
constant bool PS_FOG [[function_constant(GSMTLConstantIndex_PS_FOG)]];
constant bool PS_CLR1 [[function_constant(GSMTLConstantIndex_PS_CLR1)]];
constant bool PS_FBA [[function_constant(GSMTLConstantIndex_PS_FBA)]];
constant bool PS_FBMASK [[function_constant(GSMTLConstantIndex_PS_FBMASK)]];
constant bool PS_LTF [[function_constant(GSMTLConstantIndex_PS_LTF)]];
constant uint PS_DATE [[function_constant(GSMTLConstantIndex_PS_DATE)]];
//constant bool PS_TCOFFSETHACK [[function_constant(GSMTLConstantIndex_PS_TCOFFSETHACK)]];
//constant bool PS_POINT_SAMPLER [[function_constant(GSMTLConstantIndex_PS_POINT_SAMPLER)]];
constant bool PS_SHUFFLE [[function_constant(GSMTLConstantIndex_PS_SHUFFLE)]];
constant bool PS_READ_BA [[function_constant(GSMTLConstantIndex_PS_READ_BA)]];
constant bool PS_WRITE_RG [[function_constant(GSMTLConstantIndex_PS_WRITE_RG)]];
constant uint PS_DFMT [[function_constant(GSMTLConstantIndex_PS_DFMT)]];
constant uint PS_DEPTH_FMT [[function_constant(GSMTLConstantIndex_PS_DEPTH_FMT)]];
//constant bool PS_PAL_FMT [[function_constant(GSMTLConstantIndex_PS_PAL_FMT)]];
constant uint PS_CHANNEL_FETCH [[function_constant(GSMTLConstantIndex_PS_CHANNEL_FETCH)]];
//constant bool PS_TALES_OF_ABYSS_HLE [[function_constant(GSMTLConstantIndex_PS_TALES_OF_ABYSS_HLE)]];
//constant bool PS_URBAN_CHAOS_HLE [[function_constant(GSMTLConstantIndex_PS_URBAN_CHAOS_HLE)]];
constant bool PS_INVALID_TEX0 = false;
//constant bool PS_INVALID_TEX0 [[function_constant(GSMTLConstantIndex_PS_INVALID_TEX0)]];
//constant bool PS_SCALE_FACTOR [[function_constant(GSMTLConstantIndex_PS_SCALE_FACTOR)]];
constant bool PS_COLCLIP [[function_constant(GSMTLConstantIndex_PS_COLCLIP)]];
constant uint PS_BLEND_A [[function_constant(GSMTLConstantIndex_PS_BLEND_A)]];
constant uint PS_BLEND_B [[function_constant(GSMTLConstantIndex_PS_BLEND_B)]];
constant uint PS_BLEND_C [[function_constant(GSMTLConstantIndex_PS_BLEND_C)]];
constant uint PS_BLEND_D [[function_constant(GSMTLConstantIndex_PS_BLEND_D)]];
constant uint PS_DITHER [[function_constant(GSMTLConstantIndex_PS_DITHER)]];
constant bool PS_ZCLAMP [[function_constant(GSMTLConstantIndex_PS_ZCLAMP)]];

constant bool SW_BLEND = (PS_BLEND_A || PS_BLEND_B || PS_BLEND_D);
constant bool NOT_IIP = !IIP;
constant uint PS_PAL_FMT = PS_TEX_FMT >> 2;
constant uint PS_AEM_FMT = PS_TEX_FMT & 3;
// TODO: Use
constant bool PS_AUTOMATIC_LOD = true;
constant bool PS_MANUAL_LOD = false;

enum FMT
{
	FMT_32 = 0,
	FMT_24 = 1,
	FMT_16 = 2,
};

constant float exp_min32 = 0x1p-32;

struct GSMTLMainVSOut
{
	float4 p [[position]];
	float4 t;
	float4 ti;
	float4 c [[function_constant(IIP)]];
	uchar4 fc [[flat, function_constant(NOT_IIP)]];
};

struct GSMTLMainPSOut
{
	float4 c0 [[color(0), index(0)]];
	float4 c1 [[color(0), index(1)]];
	float depth [[depth(any), function_constant(PS_ZCLAMP)]];
};

// MARK: - Vertex Shader

void texture_coord(thread GSMTLMainVSOut& out, constant GSMTLMainVertex& v, constant GSMTLMainVSUniform& cb)
{
	float2 uv = float2(v.uv) - cb.texture_offset.xy;
	float2 st = v.st - cb.texture_offset.xy;

	// Float coordinate
	out.t.xy = st;
	out.t.w = v.q;

	// Integer coordinate => normalized
	out.ti.xy = uv * cb.texture_scale;

	if (FST)
	{
		// Integer coordinate => integral
		out.ti.zw = uv;
	}
	else
	{
		// Some games uses float coordinate for post-processing effect
		out.ti.zw = st / cb.texture_scale;
	}
}

vertex GSMTLMainVSOut vs_main(
	uint vid [[vertex_id]],
	constant GSMTLMainVertex* vertices [[buffer(GSMTLIndexVertices)]],
	constant GSMTLMainVSUniform& cb [[buffer(GSMTLIndexUniforms)]])
{
	constant GSMTLMainVertex& v = vertices[vid];

	// Clamp to max depth, gs doesn't wrap
	uint z = min(v.z, cb.max_depth);

	// pos -= 0.05 (1/320 pixel) helps avoiding rounding problems (integral part of pos is usually 5 digits, 0.05 is about as low as we can go)
	// example: ceil(afterseveralvertextransformations(y = 133)) => 134 => line 133 stays empty
	// input granularity is 1/16 pixel, anything smaller than that won't step drawing up/left by one pixel
	// example: 133.0625 (133 + 1/16) should start from line 134, ceil(133.0625 - 0.05) still above 133
	GSMTLMainVSOut out;
	out.p.xy = float2(v.p) - float2(0.05, 0.05);
	out.p.xy = out.p.xy * cb.vertex_scale - cb.vertex_offset;
	out.p.w = 1;
	out.p.z = float(z) * exp_min32;

	texture_coord(out, v, cb);

	if (IIP)
		out.c = float4(v.c) / 255.f;
	else
		out.fc = v.c;

	out.t.z = float(v.f) / 255.f; // pack for with texture

	return out;
}

// MARK: - Fragment Shader

constexpr sampler palette_sampler(filter::nearest, address::clamp_to_edge);

struct PSAll {
	thread const GSMTLMainVSOut& data;
	constant GSMTLMainPSUniform& cb;
	sampler texture_sampler;
};

struct PSMain
{
	thread const texture2d<float>& tex;
	thread const texture2d<float>& palette;
	thread const sampler& s;
	constant GSMTLMainPSUniform& cb;
	thread const GSMTLMainVSOut& in;
	uchar4 current_color;
	uint mask;
	bool discard_depth = false;

	[[gnu::always_inline]]
	PSMain(
		thread const texture2d<float>& tex,
		thread const texture2d<float>& palette,
		thread const sampler& s,
		constant GSMTLMainPSUniform& cb,
		thread const GSMTLMainVSOut& in)
		: tex(tex), palette(palette), s(s), cb(cb), in(in)
	{
		if (PS_FBMASK)
			mask = cb.fb_mask;
		else
			mask = 0;
	}

	[[gnu::always_inline]]
	float4 clamp_wrap_uv(float4 uv)
	{
		float4 uv_out = uv;
		float4 tex_size = PS_INVALID_TEX0 ? cb.wh.zwzw : cb.wh.xyxy;

		if (PS_WMS == PS_WMT)
		{
			switch (PS_WMS)
			{
				case 2:
					uv_out = clamp(uv, cb.min_max.xyxy, cb.min_max.zwzw);
					break;
				case 3:
					if (!FST)
					{
						// wrap negative uv coords to avoid an off by one error that shifted
						// textures. Fixes Xenosaga's hair issue.
						uv = fract(uv);
					}
					uv_out = float4((uint4(uv * tex_size) & cb.msk_fix.xyxy) | cb.msk_fix.zwzw) / tex_size;
					break;
			}
		}
		else
		{
			switch (PS_WMS)
			{
				case 2:
					uv_out.xz = clamp(uv.xz, cb.min_max.xx, cb.min_max.zz);
					break;
				case 3:
					if (!FST)
						uv.xz = fract(uv.xz);
					uv_out.xz = float2((uint2(uv.xz * tex_size.xx) & cb.msk_fix.xx) | cb.msk_fix.zz) / tex_size.xx;
					break;
			}
			switch (PS_WMT)
			{
				case 2:
					uv_out.yw = clamp(uv.yw, cb.min_max.yy, cb.min_max.ww);
					break;
				case 3:
					if (!FST)
						uv.yw = fract(uv.yw);
					uv_out.yw = float2((uint2(uv.yw * tex_size.yy) & cb.msk_fix.yy) | cb.msk_fix.ww) / tex_size.yy;
					break;
			}
		}

		return uv_out;
	}

	[[gnu::always_inline]]
	float4 sample_c(float2 uv)
	{
		// TODO: TEX_IS_FB

		if (PS_AUTOMATIC_LOD)
		{
			return tex.sample(s, uv);
		}
		else if (PS_MANUAL_LOD)
		{
			float K = cb.min_max.x;
			float L = cb.min_max.y;
			float bias = cb.min_max.z;
			float max_lod = cb.min_max.w;

			float gs_lod = K - log2(abs(in.t.w)) * L;
			float lod = min(gs_lod, max_lod) - bias;

			return tex.sample(s, uv, level(lod));
		}
		else
		{
			return tex.sample(s, uv, level(0)); // No lod
		}
	}

	[[gnu::always_inline]]
	float4 sample_p(float idx)
	{
		return palette.sample(palette_sampler, float2(idx, 0));
	}

	float4x4 sample_4c(float4 uv)
	{
		return {
			sample_c(uv.xy),
			sample_c(uv.zy),
			sample_c(uv.xw),
			sample_c(uv.zw),
		};
	}

	float4 sample_4_index(float4 uv)
	{
		float4 c = {
			sample_c(uv.xy).a,
			sample_c(uv.zy).a,
			sample_c(uv.xw).a,
			sample_c(uv.zw).a,
		};

		uint4 i = uint4(c * 255.99f);

		switch (PS_PAL_FMT)
		{
			case 1: // 4HL
				return float4(i & 0xF) / 255.f;
			case 2: // 4HH
				return float4(i >> 4) / 255.f;
			default:
				return c;
		}
	}

	float4x4 sample_4p(float4 u)
	{
		return {
			sample_p(u.x),
			sample_p(u.y),
			sample_p(u.z),
			sample_p(u.w),
		};
	}

	[[gnu::always_inline]]
	uchar4 sample_color(float2 st)
	{
		float4 t;
		float4x4 c;
		float2 dd;
		float4 uv;
		if (PS_LTF)
		{
			uv = st.xyxy + cb.half_texel;
			dd = fract(uv.xy * cb.wh.zw);
			if (!FST)
			{
				// Background in Shin Megami Tensei Lucifers
				// I suspect that uv isn't a standard number, so fract is outside of the [0;1] range
				// Note: it is free on GPU but let's do it only for float coordinate
				dd = saturate(dd);
			}
		}
		else
		{
			uv = st.xyxy;
		}

		uv = clamp_wrap_uv(uv);

		if (PS_PAL_FMT != 0)
			c = sample_4p(sample_4_index(uv));
		else
			c = sample_4c(uv);

		for (int i = 0; i < 4; i++)
		{
			float tax = (PS_AEM || any(bool3(c[i].rgb))) ? cb.ta.x : 0;
			if (PS_AEM_FMT == FMT_24)
				c[i].a = tax;
			else if (PS_AEM_FMT == FMT_16)
				c[i].a = c[i].a >= 0.5 ? cb.ta.y : tax;
		}

		if (PS_LTF)
			t = mix(mix(c[0], c[1], dd.x), mix(c[2], c[3], dd.x), dd.y);
		else
			t = c[0];

		return uchar4(t * 255.f + 0.05f);
	}

	[[gnu::always_inline]]
	uchar4 tfx(uchar4 T, uchar4 C)
	{
		ushort4 FxT = ushort4(C) * ushort4(T) >> 7;
		ushort4 C_out;
		switch (PS_TFX)
		{
			case 0:
				C_out = FxT;
			case 1:
				C_out = ushort4(T);
			case 2:
				C_out = ushort4(FxT.rgb, T.a) + C.a;
			case 3:
				C_out = ushort4(FxT.rgb + C.a, T.a);
			default:
				C_out = ushort4(C);
		}

		if (!PS_TCC)
			C_out.a = C.a;

		if (PS_TFX == 0 || PS_TFX == 2 || PS_TFX == 3)
		{
			// Clamp only when it is useful
			C_out = min(C_out, 255);
		}

		return uchar4(C_out);
	}

	[[gnu::always_inline]]
	void fog(thread uchar4& C, uchar f)
	{
		if (PS_FOG)
			C.rgb = mulhi(C.rgb, f) + mulhi(uchar3(cb.fog_color), 0xFF - f);
	}

	[[gnu::always_inline]]
	uchar4 sample_depth(float2 st_int)
	{
		
	}

	[[gnu::always_inline]]
	uchar4 color()
	{
		float2 st, st_int;

		if (!FST && PS_INVALID_TEX0)
		{
			st = (in.t.xy * cb.wh.xy) / (in.t.w * cb.wh.zw);
			// no st_int yet
		}
		else if (!FST)
		{
			st = in.t.xy / in.t.w;
			st_int = in.ti.zw / in.t.w;
		}
		else
		{
			// Note xy are normalized coordinate
			st = in.t.xy;
			st_int = in.ti.zw;
		}

		uchar4 T, C;

		if (PS_DEPTH_FMT)
		{
			T = sample_depth(st_int);
		}
		else
		{
			T = sample_color(st);
		}

		if (IIP)
		{
			C = tfx(T, uchar4(in.c * 255.f + 0.5f));
		}
		else
		{
			C = tfx(T, in.fc);
		}

		fog(C, uchar(in.t.z * 255.f));

		return C;
	}

	[[gnu::always_inline]]
	uchar4 run()
	{
		// Z test done in hardware

		// Destination Alpha Test
		switch (PS_DATE)
		{
			case 1: // DATE 1, DATM 0, pixels with bit 7 of A set to 0 pass
				if (current_color.a & (1<<7))
					discard_fragment();
				break;
			case 2: // DATE 1, DATM 1, pixels with bit 7 of A set to 1 pass
				if (!(current_color.a & (1<<7)))
					discard_fragment();
				break;
			default: // DATE 0, all pixels pass
				break;
		}

		uchar4 reading = color();

		// Alpha Test
		bool ate_passed;
		switch (PS_ATST)
		{
			case 1: // Less
				ate_passed = reading.a < cb.aref;
				break;
			case 2: // Greater
				ate_passed = reading.a > cb.aref;
				break;
			case 3: // Equal
				ate_passed = reading.a == cb.aref;
				break;
			case 4: // Not Equal
				ate_passed = reading.a != cb.aref;
				break;
			default: // Always
				ate_passed = true;
				break;
		}
		if (!ate_passed)
		{
			switch (PS_AFAIL)
			{
				case 1: // FB_ONLY
					discard_depth = true;
					break;
				case 2: // ZB_ONLY
					mask = 0xFFFFFFFF;
					break;
				case 3: // RGB_ONLY
					mask |= 0xFF000000;
					break;
				default: // KEEP
					discard_fragment();
					break;
			}
		}

		return reading;
	}

	[[gnu::always_inline]]
	uchar4 blend(uchar4 px)
	{

	}
};

// MARK: Fetch a Single Channel
/*
float4 ps_color(PSAll in)
{
	float2 st;
	float2 st_int;
	if (!FST && PS_INVALID_TEX0)
	{
		// Re-normalize coordinate from invalid GS to corrected texture size
		st = (in.data.t.xy * in.cb.wh.xy) / (in.data.t.w * in.cb.wh.zw);
		// no st_int yet
	}
	else if (!FST)
	{
		st = in.data.t.xy / in.data.t.w;
		st_int = in.data.ti.zw / in.data.t.w;
	}
	else
	{
		st = in.data.ti.xy;
		st_int = in.data.ti.zw;
	}

	float4 T;
	switch (PS_CHANNEL_FETCH)
	{
//		case 1: T =
	}
}

fragment GSMTLMainPSOut ps_main(
	GSMTLMainVSOut data [[stage_in]],
	constant GSMTLMainFSUniform& cb [[buffer(GSMTLIndexUniforms)]],
	sampler texture_sampler [[sampler(0)]])
{

}
*/


[[early_fragment_tests]]
fragment void ps_main_interlock(
	GSMTLMainVSOut data [[stage_in]],
	constant GSMTLMainPSUniform& cb [[buffer(GSMTLIndexUniforms)]],
	sampler s [[sampler(0)]],
	texture2d<float> tex [[texture(GSMTLTextureIndexTex)]],
	texture2d<float> palette [[texture(GSMTLTextureIndexPalette)]],
	texture2d<uint, access::read_write> rt [[texture(GSMTLTextureIndexRenderTarget)]],
	texture2d<float> ds [[texture(GSMTLTextureIndexDepth)]])
{
	PSMain main(tex, palette, s, cb, data);


}
