#include "common.h"

vertex ConvertShaderData vs_convert(
	uint vid [[vertex_id]],
	constant ConvertShaderVertex* vertices [[buffer(MetalInputIndexVertices)]])
{
	ConvertShaderData out;
	out.p = float4(vertices[vid].position, 0.5f, 1.f);
	out.t = vertices[vid].texcoord0;
	out.c = vertices[vid].color;
	return out;
}
