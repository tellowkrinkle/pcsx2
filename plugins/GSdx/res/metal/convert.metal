#include "common.h"

vertex ConvertShaderData vs_convert(
	uint vid [[vertex_id]],
	constant ConvertShaderVertex* vertices [[buffer(GSMTLIndexVertices)]])
{
	ConvertShaderData out;
	out.p = float4(vertices[vid].position, 0.5f, 1.f);
	out.t = vertices[vid].texcoord0;
	out.c = float4(vertices[vid].color) / 255.f;
	return out;
}
