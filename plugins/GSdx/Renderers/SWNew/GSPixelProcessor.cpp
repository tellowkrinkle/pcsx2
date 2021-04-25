#include "stdafx.h"
#include "GSPixelProcessor.h"

#include "../SW/GSScanlineEnvironment.h"

#if _M_SSE >= 0x501
using Vector = GSVector8i;
using HalfVector = GSVector4i;
using QuarterVector = uint64;
#else
using Vector = GSVector4i;
using HalfVector = uint64;
using QuarterVector = uint32;
#endif

__forceinline static void loadTex(const void* texcache, const HalfVector& offs, Vector& rb, Vector& ga)
{
#if _M_SSE >= 0x501
	Vector tmp = GSVector8i::cast(offs).u16to32c().gather32_32(texcache);
#else
	const uint16* offptr = reinterpret_cast<const uint16*>(offs);
	const uint32* tc = static_cast<const uint32*>(texcache);
	Vector tmp = GSVector4i(tc[offptr[0]], tc[offptr[1]], tc[offptr[2]], tc[offptr[3]]);
#endif
	rb = tmp & Vector::x00ff();
	ga = tmp.srl16(8);
}

__forceinline static Vector loadTexBlend(const QuarterVector& v)
{
#if _M_SSE >= 0x501
	Vector tmp = GSVector8i::cast(GSVector4i::loadl(&v)).u8to32c();
	return tmp | tmp << 16;
#else
	Vector tmp = GSVector4i::load(v);
	return tmp.upl8(tmp).upl8();
#endif
}

__forceinline static void loadTex2(const void* texcache, const HalfVector* offs, const Vector& f, Vector& rb, Vector& ga)
{
	Vector rb0, rb1, ga0, ga1;
	loadTex(texcache, offs[0], rb0, ga0);
	loadTex(texcache, offs[1], rb1, ga1);
	rb = rb0.lerp16_4(rb1, f);
	ga = ga0.lerp16_4(ga1, f);
}

__forceinline static void loadTex4(const void* texcache, const HalfVector* offs, const Vector& f0, const Vector& f1, Vector& rb, Vector& ga)
{
	Vector rb0, rb1, ga0, ga1;
	loadTex2(texcache, offs + 0, f0, rb0, ga0);
	loadTex2(texcache, offs + 2, f0, rb1, ga1);
	rb = rb0.lerp16_4(rb1, f1);
	ga = ga0.lerp16_4(ga1, f1);
}

__forceinline static Vector splatB2D(const QuarterVector& v)
{
#if _M_SSE >= 0x501
	constexpr static GSVector8i splatVector = GSVector8i(
		0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
		4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7);
	return GSVector8i::broadcast64(&v).shuffle8(splatVector);
#else
	constexpr static GSVector4i splatVector = GSVector4i(
		0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3);
	return GSVector4i::load(v).shuffle8(splatVector);
#endif
}

__forceinline static Vector loadF(const QuarterVector& v)
{
#if _M_SSE >= 0x501
	constexpr static GSVector8i splatVector = GSVector8i(
		0, 0x80, 0, 0x80, 1, 0x80, 1, 0x80, 2, 0x80, 2, 0x80, 3, 0x80, 3, 0x80,
		4, 0x80, 4, 0x80, 5, 0x80, 5, 0x80, 6, 0x80, 6, 0x80, 7, 0x80, 7, 0x80);
	return GSVector8i::broadcast64(&v).shuffle8(splatVector);
#else
	constexpr static GSVector4i splatVector = GSVector4i(
		0, 0x80, 0, 0x80, 1, 0x80, 1, 0x80, 2, 0x80, 2, 0x80, 3, 0x80, 3, 0x80);
	return GSVector4i::load(v).shuffle8(splatVector);
#endif
}

__forceinline static Vector loadDthe(uint32 dthe)
{
	constexpr static GSVector4i splatVector = GSVector4i(
		0, 0x80, 0, 0x80, 1, 0x80, 1, 0x80, 2, 0x80, 2, 0x80, 3, 0x80, 3, 0x80);
#if _M_SSE >= 0x501
	return GSVector8i::broadcast32(&dthe).shuffle8(GSVector8i::broadcast128(splatVector));
#else
	return GSVector4i::load(dthe).shuffle8(splatVector);
#endif
}

__forceinline static Vector loadMask(const QuarterVector& v)
{
#if _M_SSE >= 0x501
	return GSVector8i::cast(GSVector4i::loadl(&v)).i8to32c();
#else
	return splatB2D(v);
#endif
}

__forceinline static Vector loadCov(const QuarterVector& v)
{
#if _M_SSE >= 0x501
	return GSVector8i::cast(GSVector4i::loadl(&v)).u8to32c() << 16;
#else
	GSVector4i tmp = GSVector4i::load(v).upl8();
	return tmp.upl16(tmp);
#endif
}

__forceinline static Vector loadu16to32(void* src)
{
#if _M_SSE >= 0x501
	return GSVector8i::cast(GSVector4i::load<true>(src)).u16to32c();
#else
	GSVector4i tmp = GSVector4i::loadl(&src);
	return tmp.upl16();
#endif
}

__forceinline static void storeu32to16(void* dst, const Vector& v)
{
#if _M_SSE >= 0x501
	GSVector4i::store<true>(dst, v.ps32().acbd().extract<0>());
#else
	GSVector4i::storel(dst, v.ps32());
#endif
}

/// Load and expand 16-bit pixel
__forceinline static Vector loadRGB5A1(void* src)
{
#if _M_SSE >= 0x501
	GSVector8i v = GSVector8i::cast(GSVector4i::load<true>(src)).i16to32c();
#else
	GSVector4i v = GSVector4i::loadl(src);
	v = v.upl16(v);
#endif
	Vector rmask(0x000000f8);
	Vector gmask(0x0000f800);
	Vector bmask(0x00f80000);
	Vector amask(0x80000000);
	return ((v << 3) & rmask) | ((v << 6) & gmask) | ((v << 9) & bmask) | (v & amask);
}

template <bool HasZ, bool HasF, bool HasCov, int TexSamples>
__forceinline void GSPixelProcessor::DrawPixelsBase(uint64 selkey, int count, void* mainptr)
{
	GSScanlineSelector sel;
	sel.key = selkey;

	using Data = MainData<HasZ, HasF, HasCov, TexSamples>;

	Data* data = static_cast<Data*>(mainptr);

	for (int i = 0; i < count; i++, data++)
	{
		Vector z, zd;
		Vector fb;
		Vector rgbad;
		Vector mask = loadMask(data->mask);

		void* fbptr = &fbcache[data->fboff];
		void* zptr = &zcache[data->zoff];

		if constexpr (HasZ)
		{
			z = data->z;

			switch (sel.zpsm)
			{
				case 0:
					zd = Vector::load<true>(zptr);
					break;
				case 1:
					zd = Vector::load<true>(zptr);
					break;
				case 2:
					zd = loadu16to32(zptr);
					break;
			}

			if (sel.ztst == ZTST_GEQUAL || sel.ztst == ZTST_GREATER)
			{
				// Depth test
				Vector zstmp = z;
				Vector zdtmp = zd;

				if (sel.zpsm == 0)
				{
					// Makes signed compare act like unsigned compare
					zdtmp ^= Vector::x80000000();
					zstmp ^= Vector::x80000000();
				}
				else if (sel.zpsm == 1)
				{
					zdtmp &= Vector::x00ffffff();
				}

				switch (sel.ztst)
				{
					case ZTST_GEQUAL:
						mask = mask.andnot(zstmp < zdtmp);
						break;
					case ZTST_GREATER:
						mask &= zstmp > zdtmp;
						break;
				}

				if (mask.allfalse())
					continue;
			}
		}

		switch (sel.fpsm)
		{
			case 0:
			case 1:
				fb = Vector::load<true>(fbptr);
				break;
			case 2:
				fb = loadRGB5A1(fbptr);
				break;
		}

		if (sel.rfb)
		{
			rgbad = fb;
			if (sel.fpsm == 1)
				rgbad = rgbad.blend8(Vector::x80000000(), Vector::xff000000());
		}

		if (sel.date)
		{
			Vector bit = rgbad.sra32(31);
			if (sel.datm)
				mask &= bit;
			else
				mask = mask.andnot(bit);

			if (mask.allfalse())
				continue;
		}

		Vector rbf = data->rgba & Vector::x00ff();
		Vector gaf = data->rgba.srl16(8);
		Vector rb, ga;

		if constexpr (TexSamples == 8)
		{
			Vector rb0, ga0;
			Vector uf = loadTexBlend(data->tblends[0]);
			Vector vf = loadTexBlend(data->tblends[1]);
			loadTex4(texcache, data->toffs + 0, uf, vf, rb, ga);
			loadTex4(texcache, data->toffs + 4, uf, vf, rb0, ga0);
			Vector lodf = loadTexBlend(data->tblends[2]);
			rb = rb.lerp16_4(rb0, lodf);
			ga = ga.lerp16_4(ga0, lodf);
		}
		else if constexpr (TexSamples == 4)
		{
			Vector uf = loadTexBlend(data->tblends[0]);
			Vector vf = loadTexBlend(data->tblends[1]);
			loadTex4(texcache, data->toffs, uf, vf, rb, ga);
		}
		else if constexpr (TexSamples == 2)
		{
			Vector lodf = loadTexBlend(data->tblends[0]);
			loadTex2(texcache, data->toffs, lodf, rb, ga);
		}
		else if constexpr (TexSamples == 1)
		{
			loadTex(texcache, data->toffs[0], rb, ga);
		}

		if (TexSamples)
		{
			Vector af;
			switch (sel.tfx)
			{
				case TFX_MODULATE:
					rb = rb.modulate16<1>(rbf.sll16(7)).min_u16(Vector::x00ff());
					ga = ga.modulate16<1>(gaf.sll16(7)).min_u16(Vector::x00ff());
					if (!sel.tcc)
						ga = ga.mix16(gaf);
					break;
				case TFX_DECAL:
					if (!sel.tcc)
						ga = ga.mix16(gaf);
					break;
				case TFX_HIGHLIGHT:
					af = (gaf >> 16).mix16(gaf);
					if (!sel.tcc)
						ga &= Vector::x0000ffff();
					rb = rb.modulate16<1>(rbf.sll16(7)).add16(af).min_u16(Vector::x00ff());
					ga = ga.modulate16<1>(gaf.sll16(7)).add16(af).min_u16(Vector::x00ff());
					break;
				case TFX_HIGHLIGHT2:
					af = (gaf >> 16).mix16(gaf);
					rb = rb.modulate16<1>(rbf.sll16(7)).add16(af).min_u16(Vector::x00ff());
					ga = ga.modulate16<1>(gaf.sll16(7)).add16(af).min_u16(Vector::x00ff()).mix16(sel.tcc ? ga : af);
					break;
			}
		}
		else
		{
			rb = rbf;
			ga = gaf;
		}

		if (sel.aa1)
		{
			Vector mask(0x00800080);

			Vector a = mask;
			if constexpr (HasCov)
				a = loadCov(data->cov);

			if (!sel.abe)
			{
				ga = ga.mix16(a);
			}
			else
			{
				ga = ga.blend8(a, ga.eq16(mask) & Vector::xffff0000());
			}
		}

		// Alpha Test
		Vector atst;
		switch (sel.atst)
		{
			case ATST_NEVER:
				atst = Vector::zero();
				break;
			case ATST_ALWAYS:
				atst = Vector::xffffffff();
				break;
			case ATST_LESS:
			case ATST_LEQUAL:
				atst = (ga >> 16) < Vector(aref);
				break;
			case ATST_EQUAL:
				atst = (ga >> 16) == Vector(aref);
				break;
			case ATST_GEQUAL:
			case ATST_GREATER:
				atst = (ga >> 16) > Vector(aref);
				break;
			case ATST_NOTEQUAL:
				atst = (ga >> 16) != Vector(aref);
				break;
		}

		Vector zmask;
		switch (sel.afail)
		{
			case AFAIL_KEEP:
				mask &= atst;
				zmask = mask;
				break;
			case AFAIL_FB_ONLY:
				zmask &= atst;
				break;
			case AFAIL_ZB_ONLY:
				mask &= atst;
				break;
			case AFAIL_RGB_ONLY:
				zmask &= atst;
				mask &= (atst & Vector::x00ffffff());
				break;
		}

		// Z writeback
		if (HasZ && sel.zwrite)
		{
			switch (sel.zpsm)
			{
				case 0:
					Vector::store<true>(zptr, zd.blend8(z, zmask));
					break;
				case 1:
					Vector::store<true>(zptr, zd.blend8(z, zmask & Vector::x00ffffff()));
					break;
				case 2:
					storeu32to16(zptr, zd.blend8(z, zmask));
					break;
			}
		}

		if ((sel.afail == AFAIL_KEEP || sel.afail == AFAIL_ZB_ONLY) && mask.allfalse())
			continue;

		// Fog
		if constexpr (HasF)
		{
			Vector fog = loadF(data->f).sll16(7);

			Vector frb(this->frb);
			Vector fga(this->fga);

			rb = frb.lerp16<0>(rb, fog);
			ga = fga.lerp16<0>(ga, fog);
		}

		if (sel.fwrite)
		{
			// Alpha Blend
			if (sel.abe || sel.aa1)
			{
				Vector rbd, gad; // Destination colors
				Vector rbs = rb, gas = ga; // Source colors
				if (sel.aba & 1 || sel.abb & 1 || sel.abc & 1 || sel.abd & 1)
				{
					rbd = rgbad & Vector::x00ff();
					gad = rgbad.srl16(8);
				}

				if (sel.aba != sel.abb)
				{
					switch (sel.aba)
					{
						case 0:
							break;
						case 1:
							rb = rbd;
							ga = gad;
							break;
						case 2:
							rb = Vector::zero();
							ga = Vector::zero();
							break;
					}

					switch (sel.abb)
					{
						case 0:
							rb = rb.sub16(rbs);
							ga = ga.sub16(gas);
							break;
						case 1:
							rb = rb.sub16(rbd);
							ga = ga.sub16(gad);
							break;
						case 2:
							break;
					}

					Vector a;
					switch (sel.abc)
					{
						case 0: a = (gas >> 16).mix16(gas).sll16(7); break;
						case 1: a = (gad >> 16).mix16(gad).sll16(7); break;
						case 2: a = Vector(afix); break;
					}
					rb = rb.modulate16<1>(a);
					ga = ga.modulate16<1>(a);

					switch (sel.abd) {
						case 0:
							rb = rb.add16(rbs);
							ga = ga.add16(gas);
							break;
						case 1:
							rb = rb.add16(rbd);
							ga = ga.add16(gas);
							break;
						case 2:
							break;
					}

					if (sel.pabe)
					{
						Vector amask = (gas << 8).sra32(31);
						rb = rbs.blend8(rb, amask);
						ga = gas.blend8(ga, amask >> 16);
					}
					else if (sel.fpsm != 1)
					{
						ga = ga.mix16(gas);
					}
				}
			}

			if (sel.fpsm == 2 && sel.dthe)
			{
				int y = (data->fboff >> 5) & 3;
				Vector dthe = loadDthe(dither.u32[y]);

				rb = rb.add16(dthe);
				ga = ga.add16(dthe >> 16);
			}

			if (!sel.colclamp)
			{
				rb &= Vector::x00ff();
				ga &= Vector::x00ff();
			}

			Vector fs = rb.upl16(ga).pu16(rb.uph16(ga));

			fs = fb.blend(fs, mask & Vector(fbmask));

			if (sel.fpsm == 2)
			{
				Vector rb = fs & Vector(0x00f800f8);
				Vector ga = fs & Vector(0x8000f800);
				fs = (ga >> 16) | (rb >> 9) | (ga >> 6) | (rb >> 3);
				fs &= Vector::x0000ffff();
			}

			switch (sel.fpsm)
			{
				case 0:
				case 1:
					Vector::store<true>(fbptr, fs);
					break;
				case 2:
					storeu32to16(fbptr, fs);
			}
		}
	}
}

template <bool HasZ, bool HasF, bool HasCov, int TexSamples>
void GSPixelProcessor::DrawPixelsUnspecialized(GSPixelProcessor* self, uint64 selkey, int count, void* mainptr)
{
	self->DrawPixelsBase<HasZ, HasF, HasCov, TexSamples>(selkey, count, mainptr);
}

constexpr GSPixelProcessor::UnspecializedDrawPixelsList GSPixelProcessor::UnspecializedDrawPixelsList::create()
{
	GSPixelProcessor::UnspecializedDrawPixelsList out;

#define MAKE_LIST(HasZ, HasF, HasCov, TexSamplesIdx, TexSamples) \
	out._list[HasZ][HasF][HasCov][TexSamplesIdx] = DrawPixelsUnspecialized<HasZ, HasF, HasCov, TexSamples>;

#define MAKE_LIST2(HasZ, HasF, HasCov) \
	MAKE_LIST(HasZ, HasF, HasCov, 0, 0) \
	MAKE_LIST(HasZ, HasF, HasCov, 1, 1) \
	MAKE_LIST(HasZ, HasF, HasCov, 2, 2) \
	MAKE_LIST(HasZ, HasF, HasCov, 3, 4) \
	MAKE_LIST(HasZ, HasF, HasCov, 4, 8)

#define MAKE_LIST3(HasZ, HasF) \
	MAKE_LIST2(HasZ, HasF, false) \
	MAKE_LIST2(HasZ, HasF, true)

	MAKE_LIST3(false, false)
	MAKE_LIST3(false, true)
	MAKE_LIST3(true, false)
	MAKE_LIST3(true, true)

#undef MAKE_LIST3
#undef MAKE_LIST2
#undef MAKE_LIST

	return out;
}

const GSPixelProcessor::UnspecializedDrawPixelsList GSPixelProcessor::s_unspecializedDrawPixels = UnspecializedDrawPixelsList::create();
