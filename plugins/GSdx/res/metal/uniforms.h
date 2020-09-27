#pragma once

#include <simd/simd.h>

enum MetalInputIndex
{
	MetalInputIndexVertices = 0,
	MetalInputIndexVertexUniforms = 1,
	MetalInputIndexFragUniforms = 2,
};

struct ConvertShaderVertex
{
	vector_float2 position;
	vector_float2 texcoord0;
	vector_float4 color;
};

struct InterlaceFragShaderUniform
{
	vector_float2 ZrH;
	float hH;
};