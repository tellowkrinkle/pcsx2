#pragma once

#include <metal_stdlib>
#include <simd/simd.h>
#include "uniforms.h"

struct ConvertShaderData
{
	float4 p [[position]];
	float2 t;
	float4 c;
};

#define CONVERT_FRAG_IN_NO_SAMPLER \
	ConvertShaderData data [[stage_in]], \
	texture2d<float> texture [[texture(0)]]

#define CONVERT_FRAG_IN CONVERT_FRAG_IN_NO_SAMPLER, sampler s [[sampler(0)]]
