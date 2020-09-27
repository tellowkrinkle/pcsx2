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