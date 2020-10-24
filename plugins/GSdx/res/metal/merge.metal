#include "common.h"

using namespace metal;

fragment float4 ps_merge0(CONVERT_FRAG_IN)
{
	float4 c = texture.sample(s, data.t);
	c.a *= 2.f;
	return c;
}

fragment float4 ps_merge1(CONVERT_FRAG_IN,
	constant vector_float4& BGColor [[buffer(GSMTLIndexUniforms)]])
{
	float4 c = texture.sample(s, data.t);
	c.a = BGColor.a;
	return c;
}
