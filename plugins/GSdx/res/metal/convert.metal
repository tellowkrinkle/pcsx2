#include "common.h"

using namespace metal;

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

float4 ps_crt(float4 color, int i)
{
	constexpr float4 mask[4] =
	{
		float4(1, 0, 0, 0),
		float4(0, 1, 0, 0),
		float4(0, 0, 1, 0),
		float4(1, 1, 1, 0)
	};

	return color * saturate(mask[i] + 0.5f);
}

float4 ps_scanlines(float4 color, int i)
{
	constexpr float4 mask[2] =
	{
		float4(1, 1, 1, 0),
		float4(0, 0, 0, 0)
	};

	return color * saturate(mask[i] + 0.5f);
}

fragment float4 ps_copy(CONVERT_FRAG_IN)
{
	return texture.sample(s, data.t);
}

fragment ushort ps_convert_rgba8_16bits(CONVERT_FRAG_IN)
{
	float4 c = texture.sample(s, data.t);
	c.a *= 256.f / 127.f; // hm, 0.5 won't give us 1.0 if we just multiply with 2

	uint4 i = uint4(c * float4(0x001f, 0x03e0, 0x7c00, 0x8000));
	return (i.x & 0x001f) | (i.y & 0x03e0) | (i.z & 0x7c00) | (i.w & 0x8000);
}

fragment float4 ps_datm1(CONVERT_FRAG_IN)
{
	if (texture.sample(s, data.t).a < (127.5f / 255.f))
		discard_fragment();
	return float4(0);
}

fragment float4 ps_datm0(CONVERT_FRAG_IN)
{
	if (texture.sample(s, data.t).a > (127.5f / 255.f))
		discard_fragment();
	return float4(0);
}

fragment float4 ps_mod256(CONVERT_FRAG_IN)
{
	float4 c = round(texture.sample(s, data.t) * 255.f);
	return (c - 256.f * floor(c / 256.f)) / 255.f;
}

fragment float4 ps_filter_scanlines(CONVERT_FRAG_IN)
{
	return ps_scanlines(texture.sample(s, data.t), uint(data.p.y) % 2);
}

fragment float4 ps_filter_diagonal(CONVERT_FRAG_IN)
{
	uint4 p = uint4(data.p);
	return ps_crt(texture.sample(s, data.t), (p.x + (p.y % 3)) % 3);
}

fragment float4 ps_filter_transparency(CONVERT_FRAG_IN)
{
	float4 c = texture.sample(s, data.t);
	c.a = dot(c.rgb, float3(0.299f, 0.587f, 0.114f));
	return c;
}

fragment float4 ps_filter_triangular(CONVERT_FRAG_IN)
{
	uint4 p = uint4(data.p);
	uint val = ((p.x + ((p.y >> 1) & 1) * 3) >> 1) % 3;
	return ps_crt(texture.sample(s, data.t), val);
}

fragment float4 ps_filter_complex(CONVERT_FRAG_IN)
{
	float2 texdim = float2(texture.get_width(), texture.get_height());

	if (dfdy(data.t.y) * texdim.y > 0.5)
	{
		return texture.sample(s, data.t);
	}
	else
	{
		float factor = (0.9f - 0.4f * cos(2.f * M_PI_F * data.t.y * texdim.y));
		float ycoord = (floor(data.t.y * texdim.y) + 0.5f) / texdim.y;
		return factor * texture.sample(s, float2(data.t.x, ycoord));
	}
}

fragment uint ps_convert_float32_32bits(CONVERT_FRAG_IN)
{
	return uint(0x1p32 * texture.sample(s, data.t).r);
}

fragment float4 ps_convert_float32_rgba8(CONVERT_FRAG_IN)
{
	constexpr float4 bitSh = float4(0x1p24, 0x1p16, 0x1p8, 0x1p0);
	constexpr float4 bitMsk = float4(0, 0x1p-8, 0x1p-8, 0x1p-8);

	float4 res = fract(float4(texture.sample(s, data.t).r) * bitSh);
	return (res - res.xxyz * bitMsk) * (256.f/255.f);
}

fragment float4 ps_convert_float16_rgb5a1(CONVERT_FRAG_IN)
{
	constexpr float4 bitSh = float4(0x1p32, 0x1p27, 0x1p22, 0x1p17);
	constexpr uint4 bitMsk = uint4(0x1F, 0x1F, 0x1F, 0x1);

	uint4 res = uint4(float4(texture.sample(s, data.t).r) * bitSh) & bitMsk;
	return float4(res) / float4(32, 32, 32, 1);
}

fragment float ps_convert_rgba8_float32(CONVERT_FRAG_IN)
{
	constexpr float4 bitSh = float4(0xFFp-32, 0xFFp-24, 0xFFp-16, 0xFFp-8);

	return dot(texture.sample(s, data.t), bitSh);
}

fragment float ps_convert_rgba8_float24(CONVERT_FRAG_IN)
{
	// Same as above but without the alpha channel (24 bits Z)
	constexpr float3 bitSh = float3(0xFFp-32, 0xFFp-24, 0xFFp-16);
	return dot(texture.sample(s, data.t).rgb, bitSh);
}

fragment float ps_convert_rgba8_float16(CONVERT_FRAG_IN)
{
	// Same as above but without the A/B channels (16 bits Z)
	constexpr float2 bitSh = float2(0xFFp-32, 0xFFp-24);
	return dot(texture.sample(s, data.t).rg, bitSh);
}

fragment float ps_convert_rgb5a1_float16(CONVERT_FRAG_IN)
{
	constexpr float4 bitSh = float4(0x1p-32, 0x1p-27, 0x1p-22, 0x1p-17);
	// Trunc color to drop useless lsb
	float4 color = trunc(texture.sample(s, data.t) * (float4(255) / float4(8, 8, 8, 128)));
	return dot(color, bitSh);
}

fragment float4 ps_convert_rgba_8i(CONVERT_FRAG_IN,
	constant ConvertFragShaderUniform& uniform [[buffer(GSMTLIndexUniforms)]])
{
	// Convert a RGBA texture into a 8 bits packed texture
	// Input column: 8x2 RGBA pixels
	// 0: 8 RGBA
	// 1: 8 RGBA
	// Output column: 16x4 Index pixels
	// 0: 8 R | 8 B
	// 1: 8 R | 8 B
	// 2: 8 G | 8 A
	// 3: 8 G | 8 A
	float c;

	uint2 sel = uint2(data.p.xy) % uint2(16, 16);
	uint2 tb  = (uint2(data.p.xy) & ~uint2(15, 3)) >> 1;

	uint ty  = tb.y | (uint(data.p.y) & 1);
	uint txN = tb.x | (uint(data.p.x) & 7);
	uint txH = tb.x | ((uint(data.p.x) + 4) & 7);

	txN *= uniform.scaling_factor.x;
	txH *= uniform.scaling_factor.x;
	ty  *= uniform.scaling_factor.y;

	// TODO investigate texture gather
	float4 cN = texture.read(uint2(txN, ty));
	float4 cH = texture.read(uint2(txH, ty));

	if ((sel.y & 4) == 0)
	{
		// Column 0 and 2
		if ((sel.y & 2) == 0)
		{
			if ((sel.x & 8) == 0)
				c = cN.r;
			else
				c = cN.b;
		}
		else
		{
			if ((sel.x & 8) == 0)
				c = cH.g;
			else
				c = cH.a;
		}
	}
	else
	{
		// Column 1 and 3
		if ((sel.y & 2) == 0)
		{
			if ((sel.x & 8) == 0)
				c = cH.r;
			else
				c = cH.b;
		}
		else
		{
			if ((sel.x & 8) == 0)
				c = cN.g;
			else
				c = cN.a;
		}
	}
	return float4(c);
}

fragment float4 ps_yuv(CONVERT_FRAG_IN,
	constant ConvertFragShaderUniform& uniform [[buffer(GSMTLIndexUniforms)]])
{
	float4 i = texture.sample(s, data.t);
	float4 o;

	// Value from GS manual
	const float3x3 rgb2yuv =
	{
		{0.587, -0.311, -0.419},
		{0.114,  0.500, -0.081},
		{0.299, -0.169,  0.500}
	};

	float3 yuv = rgb2yuv * i.gbr;

	float Y  = 0xDB / 255.f * yuv.x + 0x10 / 255.f;
	float Cr = 0xE0 / 255.f * yuv.y + 0x80 / 255.f;
	float Cb = 0xE0 / 255.f * yuv.z + 0x80 / 255.f;

	switch (uniform.emoda)
	{
		case 0: o.a = i.a; break;
		case 1: o.a = Y;   break;
		case 2: o.a = Y/2; break;
		case 3: o.a = 0;   break;
	}

	switch (uniform.emodc)
	{
		case 0: o.rgb = i.rgb;             break;
		case 1: o.rgb = float3(Y);         break;
		case 2: o.rgb = float3(Y, Cb, Cr); break;
		case 3: o.rgb = float3(i.a);       break;
	}

	return o;
}

fragment float4 ps_osd(CONVERT_FRAG_IN)
{
	return data.c * float4(1, 1, 1, texture.sample(s, data.t).r);
}
