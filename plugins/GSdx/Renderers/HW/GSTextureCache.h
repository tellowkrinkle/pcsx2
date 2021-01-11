/*
 *	Copyright (C) 2007-2009 Gabest
 *	http://www.gabest.org
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#pragma once

#include "Renderers/Common/GSRenderer.h"
#include "Renderers/Common/GSFastList.h"
#include "Renderers/Common/GSDirtyRect.h"
#include <memory>

class GSTextureCacheNew
{
public:
	class Surface
	{
	public:
		enum class Type : uint8_t { Source, Target };
	private:
		uint8_t m_tw;   ///< Texture width (GS pages)
		uint8_t m_th;   ///< Texture height (GS pages)
		uint16_t m_tbp; ///< Texture base pointer
		Type m_type;
		GSTexture* m_texture; ///< (Owned) texture pointer
		GSRenderer* m_renderer; ///< (Unowned) renderer pointer, used for recycling texture
	public:
		Surface(GSTexture* texture, Type type, uint8_t tw, uint8_t th, uint16_t tbp)
			: m_tw(tw), m_th(th), m_tbp(tbp), m_type(type), m_texture(texture) {}
		~Surface();

		uint8_t tw() const { return m_tw; }
		uint8_t th() const { return m_th; }
		uint16_t tbp() const { return m_tbp; }
		Type type() const { return m_type; }
		GSTexture* texture() const { return m_texture; }
	};

	/// Represents the backing of a GS page
	class Entry
	{
		uint8_t m_hoff; ///< Horizontal offset in texture (GS pages)
		uint8_t m_voff; ///< Vertical offset in texture (GS pages)
		uint8_t m_psm;  ///< Type
		std::shared_ptr<Surface> m_surface;
	public:
		Entry(std::shared_ptr<Surface> surface, uint8_t hoff, uint8_t voff, GS_PSM psm)
			: m_hoff(hoff), m_voff(voff), m_psm(psm), m_surface(std::move(surface))
		{
		}

		uint8_t hoff() const { return m_hoff; }
		uint8_t voff() const { return m_voff; }
		GS_PSM psm() const { return static_cast<GS_PSM>(m_psm); }
		const std::shared_ptr<Surface>& surface() const { return m_surface; }
	};

	struct EntryList
	{
		bool is_lm_valid;     ///< Is the data in (CPU) local memory up to date?
		bool is_cleared;      ///< Was the most recent write to this memory a clear (write of the same value to all pixels)?
		uint8_t clear_psm;    ///< If `is_cleared`, the format the clear was done in
		uint32_t clear_color; ///< If `is_cleared`, the color of the clear
		SmallVector<Entry, 1> entries; ///< List of textures that match the current memory value (Can be multiple if memory was read in multiple formats)
	};

	struct TextureWithOffset
	{
		GSTexture& texture;
		uint8_t horizontal_page_offset;
		uint8_t vertical_page_offset;
	};

private: // MARK: GSTextureCacheNew ivars
	GSRenderer* m_renderer;
	GSDevice* m_device;
	/// Mapping of GS local memory page offsets to a list of valid backings
	std::array<EntryList, MAX_PAGES> m_lm;
public: // MARK: GSTextureCacheNew methods
	explicit GSTextureCacheNew(GSRenderer* r);
	~GSTextureCacheNew();

	GSTexture& GetSource(int tw, int th, int tbp0, GS_PSM psm);
	GSTexture& GetTarget(int tw, int th, int tbp0, GS_PSM psm);
	TextureWithOffset GetSourceWithOffset(int tw, int th, int tbp0, GS_PSM psm);
	TextureWithOffset GetTargetWithOffset(int tw, int th, int tbp0, GS_PSM psm);
private:
	/// Clear the entry list at the given GS memory offset
	/// (Returns the value for easy chaining into an emplace_back)
	EntryList& ClearEntryAt(uint16_t offset);
	/// Clear the given entry list
	void ClearEntryList(EntryList& list);
	/// Register a surface as a possible source for the given location, adding it to m_lm at all offsets it covers
	void RegisterSurface(std::shared_ptr<Surface> surface, int tw, int th, int tbp0, GS_PSM psm);
	/// Force the texture with offset to start at (0, 0), copying it to a new texture if necessary
	GSTexture& ForceOffsetTo00(TextureWithOffset tex, int tw, int th, int tbp0);
	/// Helper to search textures
	/// @param fn bool(EntryList&) function which will be called on each page of memory covered by the texture
	/// @returns true if all invocations of `fn` return true, otherwise short circuits and returns false
	template <typename Fn>
	bool Scan(int tw, int th, int tbp0, Fn fn);
	/// Searches for a surface that covers the entire texture, doing its best to match the given format and shape
	Entry* SearchSurface(int tw, int th, int tbp0, GS_PSM psm);
};

class GSTextureCache
{
public:
	enum {RenderTarget, DepthStencil};

	class Surface : public GSAlignedClass<32>
	{
	protected:
		GSRenderer* m_renderer;

	public:
		GSTexture* m_texture;
		GIFRegTEX0 m_TEX0;
		GIFRegTEXA m_TEXA;
		int m_age;
		uint8* m_temp;
		bool m_32_bits_fmt; // Allow to detect the casting of 32 bits as 16 bits texture
		bool m_shared_texture;
		uint32 m_end_block;  // Hint of the surface area.

	public:
		Surface(GSRenderer* r, uint8* temp);
		virtual ~Surface();

		void UpdateAge();
		bool Inside(uint32 bp, uint32 bw, uint32 psm, const GSVector4i& rect);
		bool Overlaps(uint32 bp, uint32 bw, uint32 psm, const GSVector4i& rect);
	};

	struct PaletteKey {
		const uint32* clut;
		uint16 pal;
	};

	class Palette
	{
	private:
		uint32* m_clut;
		uint16 m_pal;
		GSTexture* m_tex_palette;
		const GSRenderer* m_renderer;

	public:
		Palette(const GSRenderer* renderer, uint16 pal, bool need_gs_texture);
		~Palette();

		// Disable copy constructor and copy operator
		Palette(const Palette&) = delete;
		Palette& operator=(const Palette&) = delete;

		// Disable move constructor and move operator
		Palette(const Palette&&) = delete;
		Palette& operator=(const Palette&&) = delete;

		GSTexture* GetPaletteGSTexture();

		PaletteKey GetPaletteKey();

		void InitializeTexture();
	};

	struct PaletteKeyHash {
		// Calculate hash
		std::size_t operator()(const PaletteKey &key) const;
	};

	struct PaletteKeyEqual {
		// Compare pal value and clut contents
		bool operator()(const PaletteKey &lhs, const PaletteKey &rhs) const;
	};

	class Source : public Surface
	{
		struct {GSVector4i* rect; uint32 count;} m_write;

		void Write(const GSVector4i& r, int layer);
		void Flush(uint32 count, int layer);

	public:
		std::shared_ptr<Palette> m_palette_obj;
		GSTexture* m_palette;
		uint32 m_valid[MAX_PAGES]; // each uint32 bits map to the 32 blocks of that page
		GSVector4i m_valid_rect;
		bool m_target;
		bool m_complete;
		bool m_repeating;
		std::vector<GSVector2i>* m_p2t;
		// Keep a trace of the target origin. There is no guarantee that pointer will
		// still be valid on future. However it ought to be good when the source is created
		// so it can be used to access un-converted data for the current draw call.
		GSTexture* m_from_target;
		GIFRegTEX0 m_from_target_TEX0;  // TEX0 of the target texture, if any, else equal to texture TEX0
		GIFRegTEX0 m_layer_TEX0[7]; // Detect already loaded value
		// Keep a GSTextureCache::SourceMap::m_map iterator to allow fast erase
		std::array<uint16, MAX_PAGES> m_erase_it;
		uint32* m_pages_as_bit;

	public:
		Source(GSRenderer* r, const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, uint8* temp, bool dummy_container = false);
		virtual ~Source();

		void Update(const GSVector4i& rect, int layer = 0);
		void UpdateLayer(const GIFRegTEX0& TEX0, const GSVector4i& rect, int layer = 0);

		bool ClutMatch(PaletteKey palette_key);
	};

	class Target : public Surface
	{
	public:
		int m_type;
		bool m_used;
		GSDirtyRectList m_dirty;
		GSVector4i m_valid;
		bool m_depth_supported;
		bool m_dirty_alpha;

	public:
		Target(GSRenderer* r, const GIFRegTEX0& TEX0, uint8* temp, bool depth_supported);

		void UpdateValidity(const GSVector4i& rect);

		void Update();
	};

	class PaletteMap
	{
	private:
		static const uint16 MAX_SIZE = 65535; // Max size of each map.
		const GSRenderer* m_renderer;
		
		// Array of 2 maps, the first for 64B palettes and the second for 1024B palettes.
		// Each map stores the key PaletteKey (clut copy, pal value) pointing to the relevant shared pointer to Palette object.
		// There is one PaletteKey per Palette, and the hashing and comparison of PaletteKey is done with custom operators PaletteKeyHash and PaletteKeyEqual.
		std::array<std::unordered_map<PaletteKey, std::shared_ptr<Palette>, PaletteKeyHash, PaletteKeyEqual>, 2> m_maps;

	public:
		PaletteMap(const GSRenderer* renderer);

		// Retrieves a shared pointer to a valid Palette from m_maps or creates a new one adding it to the data structure
		std::shared_ptr<Palette> LookupPalette(uint16 pal, bool need_gs_texture);

		void Clear(); // Clears m_maps, thus deletes Palette objects
	};

	class SourceMap
	{
	public:
		std::unordered_set<Source*> m_surfaces;
		std::array<FastList<Source*>, MAX_PAGES> m_map;
		uint32 m_pages[16]; // bitmap of all pages
		bool m_used;

		SourceMap() : m_used(false) {memset(m_pages, 0, sizeof(m_pages));}

		void Add(Source* s, const GIFRegTEX0& TEX0, GSOffset* off);
		void RemoveAll();
		void RemovePartial();
		void RemoveAt(Source* s);
	};

	struct TexInsideRtCacheEntry
	{
		uint32 psm;
		uint32 bp;
		uint32 bp_end;
		uint32 bw;
		uint32 t_tex0_tbp0;
		uint32 m_end_block;
		bool has_valid_offset;
		int x_offset;
		int y_offset;
	};

protected:
	GSRenderer* m_renderer;
	PaletteMap m_palette_map;
	SourceMap m_src;
	FastList<Target*> m_dst[2];
	bool m_paltex;
	bool m_preload_frame;
	uint8* m_temp;
	bool m_can_convert_depth;
	bool m_cpu_fb_conversion;
	CRCHackLevel m_crc_hack_level;
	static bool m_disable_partial_invalidation;
	bool m_texture_inside_rt;
	static bool m_wrap_gs_mem;
	uint8 m_texture_inside_rt_cache_size = 255;
	std::vector<TexInsideRtCacheEntry> m_texture_inside_rt_cache;

	virtual Source* CreateSource(const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, Target* t = NULL, bool half_right = false, int x_offset = 0, int y_offset = 0);
	virtual Target* CreateTarget(const GIFRegTEX0& TEX0, int w, int h, int type);

	virtual int Get8bitFormat() = 0;

	// TODO: virtual void Write(Source* s, const GSVector4i& r) = 0;
	// TODO: virtual void Write(Target* t, const GSVector4i& r) = 0;

public:
	GSTextureCache(GSRenderer* r);
	virtual ~GSTextureCache();
	virtual void Read(Target* t, const GSVector4i& r) = 0;
	virtual void Read(Source* t, const GSVector4i& r) = 0;
	void RemoveAll();
	void RemovePartial();

	Source* LookupSource(const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, const GSVector4i& r);
	Source* LookupDepthSource(const GIFRegTEX0& TEX0, const GIFRegTEXA& TEXA, const GSVector4i& r, bool palette = false);

	Target* LookupTarget(const GIFRegTEX0& TEX0, int w, int h, int type, bool used, uint32 fbmask = 0);
	Target* LookupTarget(const GIFRegTEX0& TEX0, int w, int h, int real_h);

	void InvalidateVideoMemType(int type, uint32 bp);
	void InvalidateVideoMemSubTarget(GSTextureCache::Target* rt);
	void InvalidateVideoMem(GSOffset* off, const GSVector4i& r, bool target = true);
	void InvalidateLocalMem(GSOffset* off, const GSVector4i& r);

	void IncAge();
	bool UserHacks_HalfPixelOffset;
	void ScaleTexture(GSTexture* texture);

	bool ShallSearchTextureInsideRt();

	const char* to_string(int type) {
		return (type == DepthStencil) ? "Depth" : "Color";
	}

	void PrintMemoryUsage();

	void AttachPaletteToSource(Source* s, uint16 pal, bool need_gs_texture);
};
