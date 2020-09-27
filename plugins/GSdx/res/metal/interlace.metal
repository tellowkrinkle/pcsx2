#include "common.h"

using namespace metal;

constexpr sampler s(coord::normalized, address::repeat, filter::linear);

fragment float4 ps_interlace0(
	ConvertShaderData data [[stage_in]],
	constant InterlaceFragShaderUniform& uniform [[buffer(MetalInputIndexFragUniforms)]],
	texture2d<float> texture [[texture(0)]])
{
	if (fract(data.t.y * uniform.hH) - 0.5f < 0.f)
		discard_fragment();
	return texture.sample(s, data.t);
}

fragment float4 ps_interlace1(
	ConvertShaderData data [[stage_in]],
	constant InterlaceFragShaderUniform& uniform [[buffer(MetalInputIndexFragUniforms)]],
	texture2d<float> texture [[texture(0)]])
{
	if (0.5f - fract(data.t.y * uniform.hH) < 0.f)
		discard_fragment();
	return texture.sample(s, data.t);
}

fragment float4 ps_interlace2(
	ConvertShaderData data [[stage_in]],
	constant InterlaceFragShaderUniform& uniform [[buffer(MetalInputIndexFragUniforms)]],
	texture2d<float> texture [[texture(0)]])
{
	float4 c0 = texture.sample(s, data.t - uniform.ZrH);
	float4 c1 = texture.sample(s, data.t);
	float4 c2 = texture.sample(s, data.t + uniform.ZrH);
	return (c0 + c1 * 2.f + c2) / 4.f;
}

fragment float4 ps_interlace3(
	ConvertShaderData data [[stage_in]],
	texture2d<float> texture [[texture(0)]])
{
	return texture.sample(s, data.t);
}
