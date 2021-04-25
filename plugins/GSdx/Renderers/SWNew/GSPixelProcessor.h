#pragma once

#include "GSAlignedClass.h"
#include "GSVector.h"

namespace GSPixelProcessorDetail
{
#if _M_SSE >= 0x501
	using Vector = GSVector8i;
	using HalfVector = GSVector4i;
	using QuarterVector = uint64;
#else
	using Vector = GSVector4i;
	using HalfVector = uint64;
	using QuarterVector = uint32;
#endif

	struct MainData0
	{
		Vector rgba;
	};

	template <bool HasZ> struct MainData1;

	template <>
	struct MainData1<true> : public MainData0
	{
		Vector z;
	};

	template <>
	struct MainData1<false> : public MainData0
	{
	};

	template <bool HasZ, int TexSamples> struct MainData2;

	template <bool HasZ>
	struct MainData2<HasZ, 0> : public MainData1<HasZ>
	{
	};

	template <bool HasZ>
	struct MainData2<HasZ, 1> : public MainData1<HasZ>
	{
		HalfVector toffs[1];
	};

	template <bool HasZ>
	struct MainData2<HasZ, 2> : public MainData1<HasZ>
	{
		HalfVector toffs[2];
		QuarterVector tblends[1];
	};

	template <bool HasZ>
	struct MainData2<HasZ, 4> : public MainData1<HasZ>
	{
		HalfVector toffs[4];
		QuarterVector tblends[2];
	};

	template <bool HasZ>
	struct MainData2<HasZ, 8> : public MainData1<HasZ>
	{
		HalfVector toffs[8];
		QuarterVector tblends[3];
	};

	template <bool HasZ, bool HasF, int TexSamples> struct MainData3;

	template <bool HasZ, int TexSamples>
	struct MainData3<HasZ, true, TexSamples> : public MainData2<HasZ, TexSamples>
	{
		QuarterVector f;
	};

	template <bool HasZ, int TexSamples>
	struct MainData3<HasZ, false, TexSamples> : public MainData2<HasZ, TexSamples>
	{
	};

	template <bool HasZ, bool HasF, bool HasCov, int TexSamples> struct MainData4;

	template <bool HasZ, bool HasF, int TexSamples>
	struct MainData4<HasZ, HasF, true, TexSamples> : public MainData3<HasZ, HasF, TexSamples>
	{
		QuarterVector cov;
	};

	template <bool HasZ, bool HasF, int TexSamples>
	struct MainData4<HasZ, HasF, false, TexSamples> : public MainData3<HasZ, HasF, TexSamples>
	{
	};
}

class GSPixelProcessor : public GSAlignedClass<64>
{
public:
	template <bool HasZ, bool HasF, bool HasCov, int TexSamples>
	struct MainData : public GSPixelProcessorDetail::MainData4<HasZ, HasF, HasCov, TexSamples>
	{
		GSPixelProcessorDetail::QuarterVector mask;
		uint16 fboff;
		uint16 zoff;
	};

private:
	static constexpr int BLOCK_BYTES = 256;
	static constexpr int PAGE_BYTES = BLOCK_BYTES * 32;

	typedef void (*UnspecializedDrawPixelsPtr)(GSPixelProcessor* self, uint64 selkey, int count, void* mainptr);
	struct UnspecializedDrawPixelsList
	{
		typedef UnspecializedDrawPixelsPtr(List)[2][2][2][5];

	private:
		List _list = {};

	public:
		constexpr static UnspecializedDrawPixelsList create();
		operator const List&() const { return _list; }
	};

	static const UnspecializedDrawPixelsList s_unspecializedDrawPixels;

	uint8* mem_ptr;
	uint32 fbmask;
	uint32 aref;
	uint32 frb;
	uint32 fga;
	uint32 afix;
	GSVector4i dither;

	alignas(64) uint8 fbcache[PAGE_BYTES];
	alignas(64) uint8 zcache[PAGE_BYTES];
	alignas(64) uint8 texcache[PAGE_BYTES * 8];

	template <bool HasZ, bool HasF, bool HasCov, int TexSamples>
	__forceinline void DrawPixelsBase(uint64 selkey, int count, void* mainptr);

	template <bool HasZ, bool HasF, bool HasCov, int TexSamples>
	static void DrawPixelsUnspecialized(GSPixelProcessor* self, uint64 selkey, int count, void* mainptr);
};
