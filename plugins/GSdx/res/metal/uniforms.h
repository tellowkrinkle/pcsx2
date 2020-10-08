#pragma once

#include <simd/simd.h>

enum GSMetalInputIndex
{
	GSMTLIndexVertices = 0,
	GSMTLIndexUniforms = 1,
};

struct ConvertShaderVertex
{
	vector_float2 position;
	vector_float2 texcoord0;
	vector_uchar4 color;
};

struct InterlaceFragShaderUniform
{
	vector_float2 ZrH;
	float hH;
};
