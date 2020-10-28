#pragma once

#include <simd/simd.h>

enum GSMetalInputIndex
{
	GSMTLIndexVertices = 0,
	GSMTLIndexUniforms = 1,
};

enum GSMTLConstantIndex
{
	GSMTLConstantIndex_FST = 1,
	GSMTLConstantIndex_IIP,
	//GSMTLConstantIndex_VS_TME,
	GSMTLConstantIndex_PS_WMS,
	GSMTLConstantIndex_PS_WMT,
	GSMTLConstantIndex_PS_FMT,
	GSMTLConstantIndex_PS_AEM,
	GSMTLConstantIndex_PS_TFX,
	GSMTLConstantIndex_PS_TCC,
	GSMTLConstantIndex_PS_ATST,
	GSMTLConstantIndex_PS_FOG,
	GSMTLConstantIndex_PS_CLR1,
	GSMTLConstantIndex_PS_FBA,
	GSMTLConstantIndex_PS_FBMASK,
	GSMTLConstantIndex_PS_LTF,
	GSMTLConstantIndex_PS_TCOFFSETHACK,
	GSMTLConstantIndex_PS_POINT_SAMPLER,
	GSMTLConstantIndex_PS_SHUFFLE,
	GSMTLConstantIndex_PS_READ_BA,
	GSMTLConstantIndex_PS_DFMT,
	GSMTLConstantIndex_PS_DEPTH_FMT,
	GSMTLConstantIndex_PS_PAL_FMT,
	GSMTLConstantIndex_PS_CHANNEL_FETCH,
	GSMTLConstantIndex_PS_TALES_OF_ABYSS_HLE,
	GSMTLConstantIndex_PS_URBAN_CHAOS_HLE,
	GSMTLConstantIndex_PS_INVALID_TEX0,
	GSMTLConstantIndex_PS_SCALE_FACTOR,
	GSMTLConstantIndex_PS_HDR,
	GSMTLConstantIndex_PS_COLCLIP,
	GSMTLConstantIndex_PS_BLEND_A,
	GSMTLConstantIndex_PS_BLEND_B,
	GSMTLConstantIndex_PS_BLEND_C,
	GSMTLConstantIndex_PS_BLEND_D,
	GSMTLConstantIndex_PS_DITHER,
	GSMTLConstantIndex_PS_ZCLAMP,
};

struct ConvertShaderVertex
{
	vector_float2 position;
	vector_float2 texcoord0;
	vector_uchar4 color;
};

struct ConvertFragShaderUniform
{
	vector_int4 scaling_factor;
	vector_int4 channel_shuffle;
	int emoda;
	int emodc;
};

struct InterlaceFragShaderUniform
{
	vector_float2 ZrH;
	float hH;
};

struct GSMTLMainVertex
{
	vector_float2 st;
	vector_uchar4 c;
	float q;
	vector_ushort2 p;
	uint z;
	vector_ushort2 uv;
	unsigned char f;
};

struct GSMTLMainVSUniform
{
	vector_float2 vertex_scale;
	vector_float2 vertex_offset;
	vector_float2 texture_offset;
	vector_float2 texture_scale;
	uint max_depth;
};

struct GSMTLMainFSUniform
{
	vector_float4 half_texel;
	vector_float4 wh;
	vector_float4 min_max;
	vector_float4 min_f;
	vector_float4 ta;
	vector_uint4 msk_fix;
	vector_int4 channel_shuffle;
	vector_uint4 fb_mask;
	vector_float4 tc_offset_hack;
	vector_float3 fog_color;
	float aref;
	float af;
	float max_depth;
	matrix_float4x4 dither_matrix;
};
