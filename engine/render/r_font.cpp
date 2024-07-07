// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Render Font
//

#include "r_local.h"

#include <fmt/format.h>
#include <array>
#include <deque>
#include <iostream>
#include <fstream>
#include <map>
#include <mutex>
#include <nlohmann/json.hpp>
#include <stack>
#include <string>
#include <thread>
#include <unordered_map>
#include <variant>
#include <ztd/text.hpp>

#include "stb_image_write.h"

#define STB_RECT_PACK_IMPLEMENTATION 1
#include <stb_rect_pack.h>

#include <ft2build.h>
#include FT_ADVANCES_H
#include FT_FREETYPE_H
#include FT_MODULE_H
#include FT_GLYPH_H

FT_Long FT_CEIL(FT_Long val) { return ((val + 63) & -64) / 64; }

#include <fribidi.h>
#include <fribidi-bidi-types.h>

#include <hb.h>
#include <hb-ft.h>

// =======
// Classes
// =======

struct f_fontData_s {
	std::vector<uint8_t> ttData;
};

struct f_metrics_s {
	float scale;
	int ascent, descent, lineGap;
	float baseline;
	int oversampleH, oversampleV;
};

class f_rectPackState_c {
public:
	explicit f_rectPackState_c(int textureWidth = 1024, int textureHeight = 1024);
	~f_rectPackState_c() = default;
	f_rectPackState_c(const f_rectPackState_c&) = delete;
	f_rectPackState_c& operator = (const f_rectPackState_c&) = delete;

	void InitTarget();

	int texWidth, texHeight;
	stbrp_context ctx;
	std::vector<stbrp_node> nodes;
	std::vector<stbrp_rect> rects;
};

struct f_richTextString_c {
	f_richTextString_c(std::string_view str);
	f_richTextString_c(std::u32string_view str);

	size_t SegmentCount() const;
	std::u32string_view Segment(size_t idx) const;

	using Color = std::array<float, 3>;
	std::map<size_t, Color> colorChanges;
	struct SegmentExtent {
		size_t strippedStartIdx, count;
		size_t fullStartIdx;
	};
	std::vector<SegmentExtent> segments;
	std::u32string fullText;
	std::u32string onlyText;

private:
	void AddSegment(size_t startIdx, size_t endIdx, std::optional<Color> color);
};

struct f_glyphLayout_s {
	f_glyphLayout_s() = default;
	f_glyphLayout_s(uint32_t fontId, hb_glyph_info_t gi, hb_glyph_position_t gp);

	uint32_t fontId{};
	uint32_t glyphId{};
	float advX{}, advY{};
	float offX{}, offY{};
	uint32_t cluster{};
};

struct f_textLayout_s {
	using Chunk = std::vector<f_glyphLayout_s>;

	static f_textLayout_s LayoutRichTextLine(const f_fontStack_c* stack, const f_richTextString_c& richStr);
	static f_textLayout_s LayoutRichTextLineCached(const f_fontStack_c* stack, const f_richTextString_c& str);
	double TotalAdvanceX() const;

	std::deque<Chunk> chunks;
	double totalAdvanceX{};

	struct CacheKey {
		const f_fontStack_c* stack;
		std::u32string plainText;

		bool operator == (const CacheKey& rhs) const {
			return stack == rhs.stack && plainText == rhs.plainText;
		}
	};

	struct CacheValue;

private:
	void AddChunk(Chunk&& chunk);

	static std::unordered_map<CacheKey, CacheValue> cache;
};

struct f_textLayout_s::CacheValue
{
	f_textLayout_s layout;
};

namespace std {
	template <> struct hash<f_textLayout_s::CacheKey> {
		size_t operator()(const f_textLayout_s::CacheKey& x) const {
			return hash<const f_fontStack_c*>()(x.stack) ^ hash<u32string>()(x.plainText);
		}
	};
}

std::unordered_map<f_textLayout_s::CacheKey, f_textLayout_s::CacheValue> f_textLayout_s::cache;

f_rectPackState_c::f_rectPackState_c(int textureWidth, int textureHeight)
	: texWidth(textureWidth), texHeight(textureHeight), nodes(textureWidth) {
	InitTarget();
}

void f_rectPackState_c::InitTarget() {
	stbrp_init_target(&ctx, texWidth, texHeight, nodes.data(), (int)nodes.size());
	stbrp_setup_allow_out_of_mem(&ctx, 1);
}

r_fontAtlas_c::r_fontAtlas_c(class r_renderer_c* renderer)
	: renderer(renderer)
{
	packState = std::make_shared<f_rectPackState_c>();
	cpuTex.resize(packState->texWidth * packState->texHeight * 4);
	PushBlankTexture();
}

r_fontAtlas_c::~r_fontAtlas_c() {}

r_fontAtlas_c::RectHandle r_fontAtlas_c::AllocateGlyphRect(const uint8_t* sourceBitmapData, int bitmapWidth, int bitmapHeight) {
	RectHandle ret = allocatedRects.size();

	Request req{};
	req.width = bitmapWidth;
	req.height = bitmapHeight;
	req.stageOffset = stagedBitmapStorage.size();
	requestedRects.push_back(req);
	size_t bitmapSize = bitmapWidth * bitmapHeight * 4;
	stagedBitmapStorage.resize(stagedBitmapStorage.size() + bitmapSize);
	memcpy(stagedBitmapStorage.data() + req.stageOffset, sourceBitmapData, bitmapSize);

	stbrp_rect r{};
	r.id = -(int)ret - 1;
	// +2 is to accommodate padding
	r.w = bitmapWidth + 2;
	r.h = bitmapHeight + 2;
	r.x = 0;
	r.y = 0;
	r.was_packed = 0;
	packState->rects.push_back(r);

	f_glyphAllocation_s alloc{};
	alloc.sheetIndex = -1;
	allocatedRects.push_back(alloc);
	return ret;
}

void r_fontAtlas_c::PackNewRects() {
	if (requestedRects.empty()) {
		return;
	}
	while (true) {
		int allPacked = stbrp_pack_rects(&packState->ctx, packState->rects.data(), (int)packState->rects.size());
		for (int rectIdx = 0; rectIdx < packState->rects.size(); ++rectIdx) {
			auto& rect = packState->rects[rectIdx];
			if (rect.id < 0 && rect.was_packed) {
				size_t allocIdx = (size_t)-(rect.id + 1);
				rect.id = (int)textures.size() - 1;
				// Stage updates to texture
				auto& req = requestedRects[allocIdx - requestBaseIndex];
				uintptr_t storageOffset = req.stageOffset;
				size_t comp = 4;
				size_t srcStride = req.width * 4;
				const uint8_t* srcData = stagedBitmapStorage.data() + storageOffset;
				size_t dstStride = packState->texWidth * comp;
				uint8_t* dstData = cpuTex.data() + (rect.y + 1) * dstStride + (rect.x + 1) * comp;
				for (int row = 0; row < req.height; ++row) {
					memcpy(dstData, srcData, srcStride);
					srcData += srcStride;
					dstData += dstStride;
				}

				// Initialize allocated rect
				f_glyphAllocation_s& alloc = allocatedRects[allocIdx];
				alloc.sheetIndex = (int)textures.size() - 1;
				// +1 and -1 are to compensate for padding requested when packing
				alloc.tcX0 = (rect.x + 1.0f) / packState->texWidth;
				alloc.tcX1 = (rect.x + rect.w - 1.0f) / packState->texWidth;
				alloc.tcY0 = (rect.y + 1.0f) / packState->texHeight;
				alloc.tcY1 = (rect.y + rect.h - 1.0f) / packState->texHeight;
			}
		}
		// Update texture
		UpdateTexture();

		// Bail if done
		if (allPacked) {
			break;
		}
		// Move on to next texture as the current one is considered full enough to not fit everything.
		// We lose a bit at the bottom for thin rects but that's fine.
		packState->InitTarget();
		{
			auto& rects = packState->rects;
			auto I = std::remove_if(rects.begin(), rects.end(), [&](auto& r) -> bool {
				return r.id >= 0;
				});
			rects.erase(I, rects.end());
		}
		PushBlankTexture();
	}
	requestedRects.clear();
	requestBaseIndex = allocatedRects.size();
	stagedBitmapStorage.clear();
}

const f_glyphAllocation_s* r_fontAtlas_c::LookupRect(r_fontAtlas_c::RectHandle rh) {
	if (rh < 0 || rh >= allocatedRects.size() || allocatedRects[rh].sheetIndex < 0) {
		return nullptr;
	}
	return &allocatedRects[rh];
}

r_tex_c* r_fontAtlas_c::LookupSheet(r_fontAtlas_c::SheetHandle sh) {
	if (sh < 0 || sh >= textures.size()) {
		return nullptr;
	}
	return textures[sh].get();
}

void r_fontAtlas_c::PushBlankTexture() {
	std::fill(cpuTex.begin(), cpuTex.end(), 0u);
	textures.push_back({});
	UpdateTexture();
}

bool r_fontAtlas_c::NeedPacking() const {
	return allocatedRects.size() < packState->rects.size();
}

void r_fontAtlas_c::UpdateTexture() {
	image_c img{};
	img.dat = cpuTex.data();
	img.width = packState->texWidth;
	img.height = packState->texHeight;
	img.comp = 4;
	img.type = IMGTYPE_RGBA;
	std::shared_ptr<r_tex_c> tex(new r_tex_c(renderer->texMan, &img, TF_NEAREST | TF_NOMIPMAP));
	textures.back() = tex;
	tex->status = r_tex_c::DONE;
	img.dat = nullptr;
}

using f_glyphId_t = size_t;

struct f_dynamicGlyphExtent_s {
	int texX, texY;
	int width, height;
	float offsetX, offsetY;
	float advanceX;
};

struct f_glyphExtents_s {
	int bitmap_left, bitmap_top, bitmap_width, bitmap_height;
};

class f_dynamicFontHeight_c {
public:
	f_dynamicFontHeight_c(r_renderer_c* renderer, f_dynamicFont_c* parent, FT_Library ftLib, const uint8_t* ttfDataPtr, size_t ttfDataSize, int height);
	~f_dynamicFontHeight_c();
	f_dynamicFontHeight_c(const f_dynamicFontHeight_c&) = delete;
	f_dynamicFontHeight_c& operator = (const f_dynamicFontHeight_c&) = delete;

	void ScheduleGlyphLoad(uint32_t glyphIdx);

	class r_renderer_c* renderer{};
	class f_dynamicFont_c* parent{};
	int height{};
	FT_Face ftFace{};
	FT_Size_Metrics ftMetrics{};

	/* We would like to have the HarfBuzz face shared among sizes but as setting
	the height of a FreeType face changes the face it is impossible. */
	std::shared_ptr<hb_font_t> hbFont;

	std::shared_ptr<r_tex_c> dummyTex;
	struct SlotData {
		r_fontAtlas_c::RectHandle rect;
		f_glyphExtents_s extents;
	};
	std::unordered_map<uint32_t, SlotData> glyphSlots;
};

struct f_glyphMapping_s {
	size_t font;
	uint32_t glyphIdx;
};

class f_dynamicFont_c {
public:
	f_dynamicFont_c(r_renderer_c* renderer, const uint8_t* ttfDataPtr, size_t ttfDataSize, double additionalScale = 1.0);
	~f_dynamicFont_c();
	f_dynamicFont_c(const f_dynamicFont_c&) = delete;
	f_dynamicFont_c& operator = (const f_dynamicFont_c&) = delete;

	f_dynamicFontHeight_c* GetHeightInstance(int height);
	uint32_t GlyphFromChar(char32_t ch);

	class r_renderer_c* renderer{};
	std::vector<uint8_t> ttfData;
	FT_MemoryRec_ ftMem{};
	FT_Library ftLib{};
	std::unordered_map<int, std::shared_ptr<f_dynamicFontHeight_c>> heights;
	std::unordered_map<char32_t, uint32_t> glyphFromChar;
	double additionalScale = 1.0;
};

f_glyphMapping_s f_fontStack_c::GlyphMappingFromChar(char32_t ch) const {
	for (size_t fontIdx = 0; fontIdx < FontCount(); ++fontIdx) {
		auto dfh = heights[fontIdx];
		if (auto glyphIdx = dfh->parent->GlyphFromChar(ch)) {
			return { fontIdx, glyphIdx };
		}
	}
	return { 0, 0 };
}

std::vector<f_dynamicFontHeight_c*> r_font_c::FetchFontHeights(int height) {
	std::vector<f_dynamicFontHeight_c*> ret;
	for (auto& dynFont : dynFonts) {
		ret.push_back(dynFont->GetHeightInstance(height));
	}
	return ret;
}

// ===========
// Font Loader
// ===========

void f_dynamicFontHeight_c::ScheduleGlyphLoad(uint32_t glyphIdx) {
	if (glyphSlots.count(glyphIdx)) {
		return;
	}
	std::vector<uint8_t> bitmapData;
	auto glyph = ftFace->glyph;
	FT_Error ftErr{};
	auto loadFlags = hb_ft_font_get_load_flags(hbFont.get());
	ftErr = FT_Load_Glyph(ftFace, glyphIdx, loadFlags);
	assert(!ftErr);
	ftErr = FT_Render_Glyph(glyph, FT_RENDER_MODE_LIGHT);
	assert(!ftErr);

	auto& bitmap = glyph->bitmap;
	switch (bitmap.pixel_mode) {
	case FT_PIXEL_MODE_GRAY: {
		bitmapData.resize(bitmap.width * bitmap.rows * 4);
		uint8_t* src = bitmap.buffer;
		uint8_t* dst = bitmapData.data();
		size_t n = (size_t)bitmap.width * (size_t)bitmap.rows;
		while (n--) {
			memset(dst, 0xFF, 3);
			dst[3] = *src;
			src += 1;
			dst += 4;
		}
	} break;
	case FT_PIXEL_MODE_BGRA: {
		bitmapData.resize(bitmap.width * bitmap.rows * 4);
		uint8_t* src = bitmap.buffer;
		uint8_t* dst = bitmapData.data();
		size_t n = bitmap.width * bitmap.rows;
		while (n--) {
			dst[0] = src[2];
			dst[1] = src[1];
			dst[2] = src[0];
			dst[3] = src[3];
			src += 4;
			dst += 4;
		}
	} break;
	}
	auto& gs = glyphSlots[glyphIdx];
	gs.rect = renderer->fontAtlas->AllocateGlyphRect(bitmapData.data(), bitmap.width, bitmap.rows);
	f_glyphExtents_s extents{};
	extents.bitmap_left = glyph->bitmap_left;
	extents.bitmap_top = glyph->bitmap_top;
	extents.bitmap_width = glyph->bitmap.width;
	extents.bitmap_height = glyph->bitmap.rows;
	gs.extents = extents;
}

f_dynamicFontHeight_c::f_dynamicFontHeight_c(r_renderer_c* renderer, f_dynamicFont_c* parent, FT_Library ftLib,
	const uint8_t* ttfDataPtr, size_t ttfDataSize, int height)
	: renderer(renderer), parent(parent), height(height)
{
	FT_Error ftErr{};
	ftErr = FT_New_Memory_Face(ftLib, ttfDataPtr, (FT_Long)ttfDataSize, 0, &ftFace);
	assert(ftErr == 0);
	ftErr = FT_Select_Charmap(ftFace, FT_ENCODING_UNICODE);
	assert(ftErr == 0);

	FT_Size_RequestRec req;
	req.type = FT_SIZE_REQUEST_TYPE_REAL_DIM;
	req.width = 0;
	req.height = (uint32_t)height * 64;
	req.horiResolution = 0;
	req.vertResolution = 0;
	FT_Request_Size(ftFace, &req);

	hbFont.reset(hb_ft_font_create_referenced(ftFace), hb_font_destroy);
	hb_ft_font_set_funcs(hbFont.get());
	hb_ft_font_set_load_flags(hbFont.get(), FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT);

	ftMetrics = ftFace->size->metrics;

	image_c img;
	uint8_t texData[8 * 8 * 4]{};
	memset(texData, 0xFF, 8 * 8 * 4);
	img.dat = (byte*)texData;
	img.width = 8;
	img.height = 8;
	img.comp = 4;
	img.type = IMGTYPE_RGBA;
	dummyTex.reset(new r_tex_c(renderer->texMan, &img, TF_NOMIPMAP));
	dummyTex->status = r_tex_c::DONE;
	img.dat = nullptr;

	for (char32_t ch = 0; ch < 128; ++ch) {
		uint32_t glyphIdx = parent->GlyphFromChar(ch);
		ScheduleGlyphLoad(glyphIdx);
	}
}

f_dynamicFontHeight_c::~f_dynamicFontHeight_c() {
	FT_Done_Face(ftFace);
}

f_dynamicFont_c::f_dynamicFont_c(r_renderer_c* renderer, const uint8_t* ttfDataPtr, size_t ttfDataSize, double additionalScale)
	: renderer(renderer), ttfData(ttfDataPtr, ttfDataPtr + ttfDataSize), additionalScale(additionalScale)
{
	FT_Error ftErr{};
	ftMem.user = nullptr;
	ftMem.alloc = [](FT_Memory mem, long req) -> void* { return malloc(req); };
	ftMem.free = [](FT_Memory mem, void* p) { free(p); };
	ftMem.realloc = [](FT_Memory mem, long oldSize, long newSize, void* p) -> void* {
		return realloc(p, newSize);
		};

	ftErr = FT_New_Library(&ftMem, &ftLib);
	assert(ftErr == 0);

	FT_Add_Default_Modules(ftLib);

	// Init font only to gather supported characters and glyphs.
	FT_Face face{};
	ftErr = FT_New_Memory_Face(ftLib, ttfDataPtr, (FT_Long)ttfDataSize, 0, &face);
	assert(ftErr == 0);
	ftErr = FT_Select_Charmap(face, FT_ENCODING_UNICODE);
	assert(ftErr == 0);

	FT_ULong ch;
	FT_UInt glyphIdx{};
	ch = FT_Get_First_Char(face, &glyphIdx);
	while (glyphIdx != 0) {
		glyphFromChar.insert({ ch, glyphIdx });
		ch = FT_Get_Next_Char(face, ch, &glyphIdx);
	}

	FT_Done_Face(face);
}

f_dynamicFont_c::~f_dynamicFont_c() {
	heights.clear(); // Resources inside rely on ftLib, must be destroyed before the following library.
	FT_Done_Library(ftLib);
}

f_dynamicFontHeight_c* f_dynamicFont_c::GetHeightInstance(int height) {
	int adjHeight = (int)(height * additionalScale + 0.5);
	auto I = heights.find(adjHeight);
	if (I == heights.end()) {
		auto dfh = std::make_shared<f_dynamicFontHeight_c>(renderer, this, ftLib, ttfData.data(), ttfData.size(), adjHeight);
		I = heights.insert_or_assign(adjHeight, dfh).first;
	}
	return I->second.get();
}

uint32_t f_dynamicFont_c::GlyphFromChar(char32_t ch) {
	if (auto I = glyphFromChar.find(ch); I != glyphFromChar.end()) {
		return I->second;
	}
	return 0;
}

r_font_c::r_font_c(r_renderer_c* renderer, const char* fontName)
	: renderer(renderer)
{
	std::string fileNameBase = fmt::format(CFG_DATAPATH "Fonts/{}", fontName);

	// Open info file
	std::string tgfName = fileNameBase + ".tgf";
	std::ifstream tgf(tgfName);
	if (!tgf) {
		renderer->sys->con->Warning("font \"%s\" not found", fontName);
		return;
	}

	struct TgfFontDesc {
		std::filesystem::path filename;
		double scale = 1.0;

		bool valid() const { return !filename.empty(); }
	};

	std::vector<TgfFontDesc> fontDescs;

	// Parse info file
	if (tgf.peek() == '{') {
		// Parse as new-style JSON
		nlohmann::json doc = nlohmann::json::parse(tgf);

		auto docFonts = doc["fonts"];
		if (docFonts.is_array()) {
			for (auto docFont : docFonts) {
				if (docFont.is_object()) {
					TgfFontDesc desc{};
					if (auto scale = docFont["scale"]; scale.is_number()) {
						desc.scale = (double)scale;
					}
					if (auto file = docFont["file"]; file.is_string()) {
						auto filename = (std::string)file;
						desc.filename = std::filesystem::u8path(filename);
					}
					fontDescs.push_back(desc);
				}
			}
		}
	}
	else {
		std::string sub;
		while (std::getline(tgf, sub)) {
			std::string_view subv = sub;
			if (subv.substr(0, 5) == "TTF \"" && subv.substr(subv.size() - 2) == "\";") {
				std::string_view ttfName = std::string_view(sub).substr(5);
				ttfName = ttfName.substr(0, ttfName.size() - 2);
				TgfFontDesc desc{};
				desc.filename = ttfName;
				fontDescs.push_back(desc);
			}
		}
	}

	for (auto& desc : fontDescs) {
		if (!desc.valid()) {
			throw std::runtime_error("Missing font file");
		}

		std::filesystem::path ttfPath = CFG_DATAPATH "Fonts" / desc.filename;
		std::ifstream is(ttfPath, std::ios::binary);
		auto ttfDataSize = file_size(ttfPath);

		auto fontData = std::make_shared<f_fontData_s>();
		fontData->ttData.resize(ttfDataSize);
		is.read((char*)fontData->ttData.data(), ttfDataSize);
		perFontData.push_back(fontData);
		auto dynFont = std::make_shared<f_dynamicFont_c>(renderer, fontData->ttData.data(), fontData->ttData.size(), desc.scale);
		dynFonts.push_back(dynFont);
	}
}

r_font_c::~r_font_c()
{
	perFontData.clear();
}

// =============
// Font Renderer
// =============

static std::string StripColorEscapes(const char* str) {
	std::string ret;
	for (const char* p = str; !*p;) {
		if (int escapeLen = IsColorEscape(p)) {
			p += escapeLen;
		}
		else {
			ret += *p++;
		}
	}
	return ret;
}

int r_font_c::StringWidthInternal(f_fontStack_c* stack, std::u32string_view str)
{
	col3_t col{};
	f_richTextString_c richStr(str);
	auto layout = f_textLayout_s::LayoutRichTextLineCached(stack, richStr);
	return (int)std::ceil(layout.TotalAdvanceX());
}

int r_font_c::StringWidth(int height, std::u32string_view str)
{
	auto stack = FetchFontStack(height);
	std::u32string_view tail = str;
	int max = 0;
	while (!tail.empty()) {
		if (tail[0] != U'\n') {
			int lw = (int)(StringWidthInternal(stack, tail));
			if (lw > max) max = lw;
		}
		size_t np = tail.find(L'\n');
		if (np != tail.npos) {
			tail = tail.substr(np + 1);
		}
		else {
			break;
		}
	}
	return max;
}

std::u32string_view r_font_c::StringCursorInternal(f_fontStack_c* stack, std::u32string_view str, int curX)
{
	// TODO(LV): Implement this via f_textLayout_s queries for the horizontal section corresponding to the cluster. Also probably group characters as graphemes too.
	col3_t col{};
	f_richTextString_c richStr(str);
	auto layout = f_textLayout_s::LayoutRichTextLineCached(stack, richStr);

	float x{};
	for (auto& chunk : layout.chunks) {
		for (auto& glyph : chunk) {
			if (curX >= x) {
				x += glyph.advX;
				if (curX <= x) {
					for (auto& seg : richStr.segments) {
						if (auto rel = (int32_t)glyph.cluster - seg.strippedStartIdx; rel >= 0 && rel < seg.count) {
							auto ret = str.substr(seg.fullStartIdx + rel);
							// For tabs, select one or the other side depending where the cursor is.
							if (ret[0] == U'\t') {
								float fraction = (x - curX) / glyph.advX;
								if (glyph.advX < 0.0f) {
									fraction = 1.0f - fraction;
								}
								if (fraction <= 0.5f) {
									ret = ret.substr(1);
								}
							}
							return ret;
						}
					}
				}
			}
		}
	}
	return str.substr(str.size());
}

int	r_font_c::StringCursorIndex(int height, std::u32string_view str, int curX, int curY)
{
	auto stack = FetchFontStack(height);
	int lastIndex = 0;
	int lineY = height;
	auto codepoints = str;
	std::u32string_view tail(codepoints);
	while (1) {
		auto line = tail;
		size_t np = tail.find(L'\n');
		if (np != tail.npos) {
			line = tail.substr(0, np);
			tail = tail.substr(np + 1);
		}
		else {
			line = tail;
			tail = {};
		}
		lastIndex = (int)(StringCursorInternal(stack, line, curX).data() - codepoints.data());
		if (curY <= lineY) {
			break;
		}
		else {
			if (tail.empty()) {
				break;
			}
		}
		lineY += height;
	}
	return lastIndex;
}

f_fontStack_c::f_fontStack_c(std::vector<f_dynamicFontHeight_c*> heights)
	: heights(heights) {}

f_fontStack_c* r_font_c::FetchFontStack(int height) {
	auto I = fontStackForHeight.find(height);
	if (I == fontStackForHeight.end()) {
		auto heights = FetchFontHeights(height);
		auto fs = std::make_shared<f_fontStack_c>(heights);
		I = fontStackForHeight.insert({ height, fs }).first;
	}
	return I->second.get();
}

double f_fontStack_c::Baseline() const {
	return FT_CEIL(heights.front()->ftMetrics.ascender);
}

f_dynamicFontHeight_c* f_fontStack_c::Font(size_t fontIdx) const {
	return heights[fontIdx];
}

size_t f_fontStack_c::FontCount() const { return heights.size(); }

struct f_glyphSprite_s {
	r_tex_c* tex;
	float tcLeft, tcRight, tcTop, tcBottom;
	f_glyphExtents_s extents;
};

f_glyphSprite_s f_fontStack_c::SpriteForGlyphMapping(f_glyphMapping_s gm) const {
	f_glyphSprite_s ret{};
	auto [fontIdx, glyphIdx] = gm;
	auto dfh = Font(fontIdx);
	if (auto I = dfh->glyphSlots.find(glyphIdx); I != dfh->glyphSlots.end()) {
		auto [rect, extents] = I->second;
		auto renderer = dfh->renderer;
		if (auto* alloc = renderer->fontAtlas->LookupRect(rect)) {
			ret.tex = renderer->fontAtlas->LookupSheet(alloc->sheetIndex);
			ret.tcLeft = alloc->tcX0;
			ret.tcRight = alloc->tcX1;
			ret.tcTop = alloc->tcY0;
			ret.tcBottom = alloc->tcY1;
			ret.extents = extents;
		}
	}
	else {
		dfh->ScheduleGlyphLoad(glyphIdx);
	}
	return ret;
}

f_glyphSprite_s f_fontStack_c::SpriteForChar(char32_t ch) const {
	return SpriteForGlyphMapping(GlyphMappingFromChar(ch));
};

f_richTextString_c::f_richTextString_c(std::string_view str)
	: f_richTextString_c(ztd::text::transcode(str, ztd::text::utf8, ztd::text::utf32))
{}

f_richTextString_c::f_richTextString_c(std::u32string_view str)
	: fullText(str)
{
	std::u32string_view view(fullText);
	size_t segmentStartIdx = 0;
	for (size_t charIdx = 0; charIdx < fullText.size();) {
		auto tail = view.substr(charIdx);
		if (int escLen = IsColorEscape(tail)) {
			Color color{};
			ReadColorEscape(tail, color.data());
			AddSegment(segmentStartIdx, charIdx, color);
			charIdx += escLen;
			segmentStartIdx = charIdx;
		}
		else {
			++charIdx;
		}
	}

	if (segmentStartIdx != fullText.size()) {
		AddSegment(segmentStartIdx, fullText.size(), std::nullopt);
	}
}

void f_richTextString_c::AddSegment(size_t startIdx, size_t endIdx, std::optional<Color> color)
{
	size_t outStartIdx = onlyText.size();
	size_t outEndIdx = outStartIdx;
	if (startIdx != endIdx) {
		onlyText.append(fullText.begin() + startIdx, fullText.begin() + endIdx);
		outEndIdx = onlyText.size();
		SegmentExtent ext{};
		ext.strippedStartIdx = outStartIdx;
		ext.count = outEndIdx - outStartIdx;
		ext.fullStartIdx = startIdx;
		segments.push_back(ext);
	}
	if (color && (colorChanges.empty() || colorChanges.rbegin()->second != color)) {
		colorChanges[outEndIdx] = color.value();
	}
}

size_t f_richTextString_c::SegmentCount() const
{
	return segments.size();
}

std::u32string_view f_richTextString_c::Segment(size_t idx) const
{
	auto segment = segments[idx];
	return std::u32string_view(onlyText).substr(segment.strippedStartIdx, segment.count);
}

void r_font_c::DrawTextLine(scp_t pos, int align, int height, col4_t col, std::u32string_view codepoints)
{
	// This helper is useful as `col` is a sneaky out parameter.
	auto LatchCurrentColor = [renderer = this->renderer, &col](f_richTextString_c::Color c) {
		col[0] = c[0];
		col[1] = c[1];
		col[2] = c[2];
		col[3] = 1.0f;
		renderer->curLayer->Color(col);
		};

	std::u32string_view tail(codepoints);
	f_richTextString_c richStr(tail);
	// Check if the line is visible
	if (pos[Y] >= renderer->sys->video->vid.size[1] || pos[Y] <= -height) {
		// Just apply the final colour code
		if (!richStr.colorChanges.empty()) {
			auto c = richStr.colorChanges.rbegin()->second;
			LatchCurrentColor(c);
		}
		return;
	}

	auto stack = FetchFontStack(height);
	auto layout = f_textLayout_s::LayoutRichTextLineCached(stack, richStr);

	// Calculate the string position
	float x = pos[X];
	float y = pos[Y];
	if (align != F_LEFT) {
		// Calculate the real width of the string
		float width = (float)layout.TotalAdvanceX();
		switch (align) {
		case F_CENTRE:
			x = floor((renderer->VirtualScreenWidth() - width) / 2.0f + pos[X]);
			break;
		case F_RIGHT:
			x = floor(renderer->VirtualScreenWidth() - width - pos[X]);
			break;
		case F_CENTRE_X:
			x = floor(pos[X] - width / 2.0f);
			break;
		case F_RIGHT_X:
			x = floor(pos[X] - width);
			break;
		}
	}

	std::u32string_view startText = tail;
	float startX = x;
	float startY = y;

	// Render with HarfBuzz-assisted FreeType
	if (1) {
		x = startX;
		y = startY;

		float baseline = (float)stack->Baseline();
		y += baseline;

		auto colorI = richStr.colorChanges.begin();
		for (auto& chunk : layout.chunks) {
			for (auto& info : chunk) {
				// Apply an upcoming colour change if it's due.
				if (colorI != richStr.colorChanges.end() && info.cluster >= colorI->first) {
					auto change = colorI->second;
					LatchCurrentColor(change);
					++colorI;
				}
				
				bool const isTab = chunk.size() == 1 && richStr.onlyText[chunk[0].cluster] == U'\t';
				if (!isTab) {
					f_glyphMapping_s gm{ info.fontId, info.glyphId };

					auto sprite = stack->SpriteForGlyphMapping(gm);
					if (sprite.tex) {
						auto& extents = sprite.extents;
						auto vp = renderer->curViewport;
						float dstX0 = x + info.offX + extents.bitmap_left;
						float dstX1 = dstX0 + extents.bitmap_width;
						float dstY0 = y - info.offY - extents.bitmap_top;
						float dstY1 = dstY0 + extents.bitmap_height;
						renderer->curLayer->Bind(sprite.tex);
						renderer->curLayer->Quad(
							sprite.tcLeft, sprite.tcTop, dstX0, dstY0,
							sprite.tcRight, sprite.tcTop, dstX1, dstY0,
							sprite.tcRight, sprite.tcBottom, dstX1, dstY1,
							sprite.tcLeft, sprite.tcBottom, dstX0, dstY1
						);
					}
				}

				x += info.advX;
				y += info.advY;
			}
		}
		if (!richStr.colorChanges.empty()) {
			auto change = richStr.colorChanges.rbegin()->second;
			LatchCurrentColor(change);
		}
	}
}

void r_font_c::Draw(scp_t pos, int align, int height, col4_t col, std::u32string_view str)
{
	if (str.empty()) {
		pos[Y] += height;
		return;
	}

	// Prepare for rendering
	renderer->curLayer->Color(col);

	// Separate into lines and render them
	auto codepoints = str;
	std::u32string_view tail(codepoints);
	while (!tail.empty()) {
		std::u32string_view head;
		auto np = tail.find(U'\n');
		if (np != tail.npos) {
			head = tail.substr(0, np);
			tail = tail.substr(np + 1);
		}
		else {
			head = tail;
			tail = tail.substr(0, 0);
		}
		if (!head.empty()) {
			DrawTextLine(pos, align, height, col, head);
		}
		pos[Y] += height;
	}
}

void r_font_c::FDraw(scp_t pos, int align, int height, col4_t col, const char* fmt, ...)
{
	va_list va;
	va_start(va, fmt);
	VDraw(pos, align, height, col, fmt, va);
	va_end(va);
}

void r_font_c::VDraw(scp_t pos, int align, int height, col4_t col, const char* fmt, va_list va)
{
	char str[65536];
	vsnprintf(str, 65535, fmt, va);
	str[65535] = 0;
	auto idxStr = IndexUTF8ToUTF32(str);
	Draw(pos, align, height, col, idxStr.text);
}

inline f_glyphLayout_s::f_glyphLayout_s(uint32_t fontId, hb_glyph_info_t gi, hb_glyph_position_t gp)
	: fontId(fontId), glyphId(gi.codepoint), advX(gp.x_advance / 64.0f), advY(gp.y_advance / 64.0f),
	offX(gp.x_offset / 64.0f), offY(gp.y_offset / 64.0f), cluster(gi.cluster) {}

f_textLayout_s f_textLayout_s::LayoutRichTextLine(const f_fontStack_c* stack, const f_richTextString_c& richStr)
{
	/* TODO(LV) : For HarfBuzz shaped text, it makes more sense to layout once and interpret those measurements both
	* for the string width needed for alignment and as the actual layout for drawing.
	*
	* - split input text into segments based on directionality
	* - for each segment: keep a queue of work items that either are spans that need to be (re)shaped or successfully
		shaped runs of glyphs
	* - consume that queue one item at a time;
	*   - if it's a good run write it out to the layout
	*   - if not try to shape it with the next font in line and split up into new work items in reverse order,
	*     pushing them onto the queue
	*
	* Remember to consult colour segmentation with cluster ID when iterating for drawing.
	*
	* Glyph runs are either monotonically increasing in cluster IDs or monotonically decreasing for LTR and RTL order
	* respectively. We want cluster level 1 - HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS, allowing us to colour marks
	  differently for example (https://harfbuzz.github.io/working-with-harfbuzz-clusters.html)
	*/

	f_textLayout_s layout{};
	// Segment and shape text
	if (!richStr.onlyText.empty()) {
		struct Segment {
			uint32_t clusterBase{}, clusterCount{};
			bool isRtl{};
		};

		struct PendingSpan {
			uint32_t nextFontId{};
			uint32_t clusterBase{}, clusterEnd{};
			uint32_t ClusterCount() const { return clusterEnd - clusterBase; }
		};

		using WorkSpan = std::variant<f_textLayout_s::Chunk, PendingSpan>;

		std::map<uint32_t, Segment> initialSegments;
		std::map<uint32_t, bool> isRtl;
		auto textPtr = (const FriBidiChar*)richStr.onlyText.c_str();
		auto textLen = (int)richStr.onlyText.size();
		std::vector<FriBidiCharType> bTypes(textLen);
		fribidi_get_bidi_types(textPtr, textLen, bTypes.data());

		std::vector<FriBidiCharType> bracketTypes(textLen);
		fribidi_get_bracket_types(textPtr, textLen, bTypes.data(), bracketTypes.data());

		FriBidiParType pbaseDir = FRIBIDI_PAR_ON;
		std::vector<FriBidiLevel> embeddingLevels(textLen);
		fribidi_get_par_embedding_levels_ex(bTypes.data(), bracketTypes.data(), textLen, &pbaseDir, embeddingLevels.data());

		// Build directionality lookup table
		{
			bool lastRtl = FRIBIDI_LEVEL_IS_RTL(embeddingLevels[0]);
			isRtl[0] = lastRtl;
			for (size_t idx = 1; idx < textLen; ++idx) {
				bool thisRtl = FRIBIDI_LEVEL_IS_RTL(embeddingLevels[idx]);
				if (lastRtl != thisRtl) {
					isRtl[(uint32_t)idx] = thisRtl;
					lastRtl = thisRtl;
				}
			}
		}

		// Segment input based on directionality
		for (auto I = isRtl.begin(); I != isRtl.end(); ++I) {
			auto J = std::next(I);
			size_t segEnd = J != isRtl.end() ? J->first : textLen;
			Segment seg{};
			seg.isRtl = I->second;
			seg.clusterBase = I->first;
			seg.clusterCount = (uint32_t)(segEnd - seg.clusterBase);
			initialSegments[seg.clusterBase] = seg;
		}

		std::shared_ptr<hb_buffer_t> buf(hb_buffer_create(), &hb_buffer_destroy);
		for (auto& [_, seg] : initialSegments) {
			std::deque<WorkSpan> spanProcessQueue;
			{
				PendingSpan span{};
				span.nextFontId = 0;
				span.clusterBase = seg.clusterBase;
				span.clusterEnd = seg.clusterBase + seg.clusterCount;
				spanProcessQueue.push_front(span);
			}

			while (!spanProcessQueue.empty()) {
				auto span = std::move(spanProcessQueue.front());
				spanProcessQueue.pop_front();
				if (auto* deferredChunk = std::get_if<f_textLayout_s::Chunk>(&span)) {
					layout.AddChunk(std::move(*deferredChunk));
				}
				else if (auto* pSpan = std::get_if<PendingSpan>(&span)) {
					std::stack<WorkSpan> newWork;

					// If a tab is present, split the span and process only the head. More work is emitted after processing.
					auto spanView = std::u32string_view((char32_t const*)textPtr + pSpan->clusterBase, pSpan->ClusterCount());
					std::optional<PendingSpan> tailSpan;
					if (auto tabOff = spanView.find(U'\t'); tabOff != spanView.npos) {
						tailSpan = *pSpan;
						tailSpan->clusterBase += (uint32_t)tabOff + 1;
						pSpan->clusterEnd = pSpan->clusterBase + (uint32_t)tabOff;
					}

					hb_buffer_add_utf32(buf.get(), (uint32_t const*)textPtr, textLen, pSpan->clusterBase,
						pSpan->ClusterCount());
					hb_buffer_set_cluster_level(buf.get(), HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS);
					hb_buffer_set_direction(buf.get(), seg.isRtl ? HB_DIRECTION_RTL : HB_DIRECTION_LTR);
					hb_buffer_guess_segment_properties(buf.get());

					auto dfh = stack->Font(pSpan->nextFontId);
					hb_shape(dfh->hbFont.get(), buf.get(), nullptr, 0);

					uint32_t glyphCount{};
					auto* glyphInfos = hb_buffer_get_glyph_infos(buf.get(), &glyphCount);
					auto* glyphPositions = hb_buffer_get_glyph_positions(buf.get(), &glyphCount);
					auto glyphInfosEnd = glyphInfos + glyphCount;
					auto glyphPositionsEnd = glyphPositions + glyphCount;

					bool seekingGood = true;
					for (size_t glyphIdx = 0; glyphIdx < glyphCount;) {
						auto giI = glyphInfos + glyphIdx;
						auto gpI = glyphPositions + glyphIdx;
						auto& gi = *giI;
						if (gi.codepoint > 0) {
							auto giJ = std::find_if(giI + 1, glyphInfosEnd, [](auto& gi) { return gi.codepoint == 0; });
							auto n = giJ - giI;
							f_textLayout_s::Chunk goodChunk;
							goodChunk.reserve(n);
							for (auto glyphOff = 0; glyphOff < n; ++glyphOff) {
								f_glyphLayout_s glyph(pSpan->nextFontId, *(giI + glyphOff), *(gpI + glyphOff));
								goodChunk.push_back(glyph);
							}
							if (newWork.empty()) {
								layout.AddChunk(std::move(goodChunk));
							}
							else {
								newWork.push(std::move(goodChunk));
							}
							glyphIdx += n;
						}
						else {
							auto giJ = std::find_if(giI + 1, glyphInfosEnd, [](auto& gi) { return gi.codepoint > 0; });
							auto n = giJ - giI;
							auto upcomingFontId = pSpan->nextFontId + 1;
							if (upcomingFontId == stack->FontCount()) {
								// Out of fonts, emit a sequence of notdef glyphs
								// TODO(LV): keep a shaped notdef from the primary font to use here for visual consistency when adding more fallback fonts
								f_textLayout_s::Chunk tofuChunk{};
								for (auto glyphOff = 0; glyphOff < n; ++glyphOff) {
									f_glyphLayout_s glyph(pSpan->nextFontId, *(giI + glyphOff), *(gpI + glyphOff));
									tofuChunk.push_back(glyph);
								}
								if (newWork.empty()) {
									layout.AddChunk(std::move(tofuChunk));
								}
								else {
									newWork.push(std::move(tofuChunk));
								}
							}
							else {
								PendingSpan pendingSpan{};
								pendingSpan.nextFontId = upcomingFontId;
								if (seg.isRtl) {
									pendingSpan.clusterBase = (uint32_t)(giI->cluster + 1 - n);
									pendingSpan.clusterEnd = (uint32_t)(giI->cluster + 1);
								}
								else {
									pendingSpan.clusterBase = giI->cluster;
									pendingSpan.clusterEnd = (uint32_t)(giI->cluster + n);
								}
								newWork.push(pendingSpan);
							}
							glyphIdx += n;
						}
					}
					hb_buffer_reset(buf.get());

					// If a tab was encountered, push work for the tab chunk and the tail.
					if (tailSpan) {
						{
							float tabWidth = stack->Font(0)->height * 1.0f; // arbitrary tab width guess
							f_glyphLayout_s g{};
							g.advX = tabWidth;
							g.cluster = pSpan->clusterEnd;
							f_textLayout_s::Chunk tabChunk(1, g);
							newWork.push(std::move(tabChunk));
						}
						newWork.push(*tailSpan);
					}

					while (!newWork.empty()) {
						spanProcessQueue.push_front(newWork.top());
						newWork.pop();
					}
				}
			}
		}
	}
	return layout;
}

f_textLayout_s f_textLayout_s::LayoutRichTextLineCached(const f_fontStack_c* stack, const f_richTextString_c& str)
{
	CacheKey key{ stack, str.fullText };
	auto I = cache.find(key);
	if (I == cache.end()) {
		CacheValue value{ LayoutRichTextLine(stack, str) };
		I = cache.insert({ key, value }).first;
	}
	return I->second.layout;
}

double f_textLayout_s::TotalAdvanceX() const
{
	return totalAdvanceX;
}

void f_textLayout_s::AddChunk(Chunk&& chunk)
{
	for (auto& glyph : chunk) {
		totalAdvanceX += glyph.advX;
	}
	chunks.push_back(std::move(chunk));
}
