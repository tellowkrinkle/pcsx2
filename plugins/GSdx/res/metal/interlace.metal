#include "common.h"

using namespace metal;

fragment float4 ps_interlace0(CONVERT_FRAG_IN,
	constant InterlaceFragShaderUniform& uniform [[buffer(GSMTLIndexUniforms)]])
{
	if (fract(data.t.y * uniform.hH) - 0.5f < 0.f)
		discard_fragment();
	return texture.sample(s, data.t);
}

fragment float4 ps_interlace1(CONVERT_FRAG_IN,
	constant InterlaceFragShaderUniform& uniform [[buffer(GSMTLIndexUniforms)]])
{
	if (0.5f - fract(data.t.y * uniform.hH) < 0.f)
		discard_fragment();
	return texture.sample(s, data.t);
}

fragment float4 ps_interlace2(CONVERT_FRAG_IN,
	constant InterlaceFragShaderUniform& uniform [[buffer(GSMTLIndexUniforms)]])
{
	float4 c0 = texture.sample(s, data.t - uniform.ZrH);
	float4 c1 = texture.sample(s, data.t);
	float4 c2 = texture.sample(s, data.t + uniform.ZrH);
	return (c0 + c1 * 2.f + c2) / 4.f;
}

fragment float4 ps_interlace3(CONVERT_FRAG_IN)
{
	return texture.sample(s, data.t);
}
