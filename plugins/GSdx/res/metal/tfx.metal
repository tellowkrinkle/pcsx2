#include "common.h"

using namespace metal;

constant bool FST [[function_constant(GSMTLConstantIndex_FST)]];
constant bool IIP [[function_constant(GSMTLConstantIndex_IIP)]];
//constant bool VS_TME [[function_constant(GSMTLConstantIndex_VS_TME)]];
constant uint PS_WMS [[function_constant(GSMTLConstantIndex_PS_WMS)]];
constant uint PS_WMT [[function_constant(GSMTLConstantIndex_PS_WMT)]];
constant uint PS_FMT [[function_constant(GSMTLConstantIndex_PS_FMT)]];
constant bool PS_AEM [[function_constant(GSMTLConstantIndex_PS_AEM)]];
constant bool PS_TFX [[function_constant(GSMTLConstantIndex_PS_TFX)]];
constant bool PS_TCC [[function_constant(GSMTLConstantIndex_PS_TCC)]];
constant bool PS_ATST [[function_constant(GSMTLConstantIndex_PS_ATST)]];
constant bool PS_FOG [[function_constant(GSMTLConstantIndex_PS_FOG)]];
constant bool PS_CLR1 [[function_constant(GSMTLConstantIndex_PS_CLR1)]];
constant bool PS_FBA [[function_constant(GSMTLConstantIndex_PS_FBA)]];
constant bool PS_FBMASK [[function_constant(GSMTLConstantIndex_PS_FBMASK)]];
constant bool PS_LTF [[function_constant(GSMTLConstantIndex_PS_LTF)]];
constant bool PS_TCOFFSETHACK [[function_constant(GSMTLConstantIndex_PS_TCOFFSETHACK)]];
constant bool PS_POINT_SAMPLER [[function_constant(GSMTLConstantIndex_PS_POINT_SAMPLER)]];
constant bool PS_SHUFFLE [[function_constant(GSMTLConstantIndex_PS_SHUFFLE)]];
constant bool PS_READ_BA [[function_constant(GSMTLConstantIndex_PS_READ_BA)]];
constant bool PS_DFMT [[function_constant(GSMTLConstantIndex_PS_DFMT)]];
constant bool PS_DEPTH_FMT [[function_constant(GSMTLConstantIndex_PS_DEPTH_FMT)]];
constant bool PS_PAL_FMT [[function_constant(GSMTLConstantIndex_PS_PAL_FMT)]];
constant uint PS_CHANNEL_FETCH [[function_constant(GSMTLConstantIndex_PS_CHANNEL_FETCH)]];
constant bool PS_TALES_OF_ABYSS_HLE [[function_constant(GSMTLConstantIndex_PS_TALES_OF_ABYSS_HLE)]];
constant bool PS_URBAN_CHAOS_HLE [[function_constant(GSMTLConstantIndex_PS_URBAN_CHAOS_HLE)]];
constant bool PS_INVALID_TEX0 [[function_constant(GSMTLConstantIndex_PS_INVALID_TEX0)]];
constant bool PS_SCALE_FACTOR [[function_constant(GSMTLConstantIndex_PS_SCALE_FACTOR)]];
constant bool PS_HDR [[function_constant(GSMTLConstantIndex_PS_HDR)]];
constant bool PS_COLCLIP [[function_constant(GSMTLConstantIndex_PS_COLCLIP)]];
constant bool PS_BLEND_A [[function_constant(GSMTLConstantIndex_PS_BLEND_A)]];
constant bool PS_BLEND_B [[function_constant(GSMTLConstantIndex_PS_BLEND_B)]];
constant bool PS_BLEND_C [[function_constant(GSMTLConstantIndex_PS_BLEND_C)]];
constant bool PS_BLEND_D [[function_constant(GSMTLConstantIndex_PS_BLEND_D)]];
constant bool PS_DITHER [[function_constant(GSMTLConstantIndex_PS_DITHER)]];
constant bool PS_ZCLAMP [[function_constant(GSMTLConstantIndex_PS_ZCLAMP)]];

constant bool NOT_IIP = !IIP;

constant float exp_min32 = 0x1p-32;

struct GSMTLMainVSOut
{
	float4 p [[position]];
	float4 t;
	float4 ti;
	float4 c [[function_constant(IIP)]];
	float4 fc [[flat, function_constant(NOT_IIP)]];
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
		out.fc = float4(v.c) / 255.f;

	out.t.z = float(v.f) / 255.f; // pack for with texture

	return out;
}

// MARK: - Fragment Shader

constexpr sampler palette_sampler(filter::nearest, address::clamp_to_edge);

struct PSAll {
	thread const GSMTLMainVSOut& data;
	constant GSMTLMainFSUniform& cb;
	sampler texture_sampler;
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
