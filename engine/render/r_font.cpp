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

struct f_dynMetrics_s {
	float advanceX;
};

struct f_dynKerning_s {
	float x;
};

struct f_glyphLayout_s {
	f_glyphLayout_s() = default;
	f_glyphLayout_s(uint32_t fontId, hb_glyph_info_t gi, hb_glyph_position_t gp)
		: fontId(fontId), glyphId(gi.codepoint), advX(gp.x_advance / 64.0f), advY(gp.y_advance / 64.0f),
		offX(gp.x_offset / 64.0f), offY(gp.y_offset / 64.0f), cluster(gi.cluster) {}

	uint32_t fontId{};
	uint32_t glyphId{};
	float advX{}, advY{};
	float offX{}, offY{};
	uint32_t cluster{};
};

struct f_textLayout_s {
	using Chunk = std::vector<f_glyphLayout_s>;

	std::deque<Chunk> chunks;
};

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

	f_dynMetrics_s* GlyphMetrics(uint32_t glyph);
	f_dynKerning_s* KerningPair(uint32_t glyph0, uint32_t glyph1);

	using KerningPairKey = uint64_t;

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
	std::unordered_map<uint32_t, f_dynMetrics_s> glyphMetrics;
	std::unordered_map<KerningPairKey, f_dynKerning_s> kerningPairs;
};

struct f_glyphMapping_s {
	size_t font;
	uint32_t glyphIdx;
};

class f_dynamicFont_c {
public:
	f_dynamicFont_c(r_renderer_c* renderer, const uint8_t* ttfDataPtr, size_t ttfDataSize);
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

f_dynMetrics_s* f_dynamicFontHeight_c::GlyphMetrics(uint32_t glyph)
{
	if (auto I = glyphMetrics.find(glyph); I != glyphMetrics.end()) {
		return &I->second;
	}
	return nullptr;
}

f_dynKerning_s* f_dynamicFontHeight_c::KerningPair(uint32_t glyph0, uint32_t glyph1)
{
	KerningPairKey key = (uint64_t)glyph0 | ((uint64_t)glyph1 << 32);
	if (auto I = kerningPairs.find(key); I != kerningPairs.end()) {
		return &I->second;
	}
	return nullptr;
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

	FT_Int32 loadFlags = hb_ft_font_get_load_flags(hbFont.get()) | FT_LOAD_ADVANCE_ONLY;
	std::vector<FT_Fixed> advances(ftFace->num_glyphs);
	FT_Get_Advances(ftFace, 0, ftFace->num_glyphs, loadFlags, advances.data());
	for (FT_Long glyphIdx = 0; glyphIdx < ftFace->num_glyphs; ++glyphIdx) {
		glyphMetrics[glyphIdx].advanceX = advances[glyphIdx] / 65536.0f;
		if (FT_HAS_KERNING(ftFace)) {
			for (FT_Long glyphIdx2 = 0; glyphIdx2 < ftFace->num_glyphs; ++glyphIdx2) {
				FT_Vector kerning{};
				ftErr = FT_Get_Kerning(ftFace, glyphIdx, glyphIdx2, FT_KERNING_DEFAULT, &kerning);
				if (!ftErr && kerning.x != 0) {
					KerningPairKey key = (uint64_t)glyphIdx | ((uint64_t)glyphIdx2 << 32);
					kerningPairs[key].x = kerning.x / 64.0f;
				}
			}
		}
	}

	for (char32_t ch = 0; ch < 128; ++ch) {
		uint32_t glyphIdx = parent->GlyphFromChar(ch);
		ScheduleGlyphLoad(glyphIdx);
	}
}

f_dynamicFontHeight_c::~f_dynamicFontHeight_c() {
	FT_Done_Face(ftFace);
}

f_dynamicFont_c::f_dynamicFont_c(r_renderer_c* renderer, const uint8_t* ttfDataPtr, size_t ttfDataSize)
	: renderer(renderer), ttfData(ttfDataPtr, ttfDataPtr + ttfDataSize)
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
	auto I = heights.find(height);
	if (I == heights.end()) {
		auto dfh = std::make_shared<f_dynamicFontHeight_c>(renderer, this, ftLib, ttfData.data(), ttfData.size(), height);
		I = heights.insert_or_assign(height, dfh).first;
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

	// Parse info file
	std::string sub;
	while (std::getline(tgf, sub)) {
		std::string_view subv = sub;
		int h, x, y, w, sl, sr;
		if (subv.substr(0, 5) == "TTF \"" && subv.substr(subv.size() - 2) == "\";") {
			std::string_view ttfName = std::string_view(sub).substr(5);
			ttfName = ttfName.substr(0, ttfName.size() - 2);
			OutputDebugStringA(fmt::format("{}\n", ttfName).c_str());
			std::filesystem::path ttfPath = fmt::format(CFG_DATAPATH "Fonts/{}", ttfName);
			std::ifstream is(ttfPath, std::ios::binary);
			auto ttfDataSize = file_size(ttfPath);

			auto fontData = std::make_shared<f_fontData_s>();
			fontData->ttData.resize(ttfDataSize);
			is.read((char*)fontData->ttData.data(), ttfDataSize);
			perFontData.push_back(fontData);
		}
		else if (sscanf(sub.c_str(), "HEIGHT %u;", &h) == 1) {
		}
		else if (sscanf(sub.c_str(), "GLYPH %u %u %u %d %d;", &x, &y, &w, &sl, &sr) == 5) {
		}
	}

	for (auto fontData : perFontData) {
		auto dynFont = std::make_shared<f_dynamicFont_c>(renderer, fontData->ttData.data(), fontData->ttData.size());
		dynFonts.push_back(dynFont);
	}
}

r_font_c::~r_font_c()
{
	perFontData.clear();
}

f_dynMetrics_s f_fontStack_c::MetricsForChar(char32_t ch) const {
	auto gm = GlyphMappingFromChar(ch);
	return MetricsForGlyphMapping(gm);
}

f_dynMetrics_s f_fontStack_c::MetricsForGlyphMapping(const f_glyphMapping_s& gm) const {
	auto dfh = heights[gm.font];
	if (auto mp = dfh->GlyphMetrics(gm.glyphIdx)) {
		return *mp;
	}
	return {};
	//FT_Int32 loadFlags = FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT | FT_LOAD_ADVANCE_ONLY;
	//FT_Load_Glyph(dfh->ftFace, gm.glyphIdx, loadFlags);
	//f_dynMetrics_s ret{};
	//ret.advanceX = dfh->ftFace->glyph->advance.x / 64.0f;
	//return ret;
}

f_dynKerning_s f_fontStack_c::KerningForChars(char32_t ch0, char32_t ch1) const {
	auto gm0 = GlyphMappingFromChar(ch0);
	auto gm1 = GlyphMappingFromChar(ch1);
	return KerningForGlyphMapping(gm0, gm1);
}

f_dynKerning_s f_fontStack_c::KerningForGlyphMapping(const f_glyphMapping_s& gm0, const f_glyphMapping_s& gm1) const {
	if (gm0.font == gm1.font && gm1.glyphIdx) {
		auto dfh = heights[gm0.font];
		if (auto kpp = dfh->KerningPair(gm0.glyphIdx, gm1.glyphIdx)) {
			return *kpp;
		}
		//FT_Vector kerning{};
		//FT_Get_Kerning(dfh->ftFace, gm0.glyphIdx, gm1.glyphIdx, FT_KERNING_DEFAULT, &kerning);
		//f_dynKerning_s ret{};
		//ret.x = kerning.x / 64.0f;
		//return ret;
	}
	return {};
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
	double width = 0;
	while (!str.empty() && str[0] != U'\n') {
		char32_t ch = str[0];
		int escLen = IsColorEscape(str);
		if (escLen) {
			str = str.substr(escLen);
		}
		else if (ch == U'\t') {
			auto metrics = stack->MetricsForChar(U' ');
			auto spWidth = metrics.advanceX;
			width += spWidth * 4;
			str = str.substr(1);
		}
		else {
			auto metrics = stack->MetricsForChar(ch);
			width += metrics.advanceX;
			str = str.substr(1);

			// Kern to next glyph if any
			if (!str.empty()) {
				auto kerning = stack->KerningForChars(ch, str[0]);
				width += kerning.x;
			}
		}
	}
	return (int)std::ceil(width);
}

int r_font_c::StringWidth(int height, const char* str)
{
	auto codepoints = ztd::text::transcode(std::string_view(str), ztd::text::utf8, ztd::text::utf32);
	auto stack = FetchFontStack(height);
	std::u32string_view tail = codepoints;
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
	int x = 0;
	while (!str.empty() && str[0] != U'\n') {
		int escLen = IsColorEscape(str);
		if (escLen) {
			str = str.substr(escLen);
		}
		else if (str[0] == U'\t') {
			int spWidth = (int)stack->MetricsForChar(U' ').advanceX;
			x += spWidth * 2;
			if (curX <= x) {
				break;
			}
			x += spWidth * 2;
			str = str.substr(1);
		}
		else {
			x += (int)stack->MetricsForChar(str[0]).advanceX;
			if (curX <= x) {
				break;
			}
			str = str.substr(1);
		}
	}
	return str;
}

int	r_font_c::StringCursorIndex(int height, const char* str, int curX, int curY)
{
	auto stack = FetchFontStack(height);
	int lastIndex = 0;
	int lineY = height;
	const char* lptr = str;
	auto codepoints = ztd::text::transcode(std::string_view(str), ztd::text::utf8, ztd::text::utf32);
	std::u32string_view tail(codepoints);
	while (1) {
		lastIndex = (int)(StringCursorInternal(stack, tail, curX).data() - codepoints.data());
		if (curY <= lineY) {
			break;
		}
		size_t np = tail.find(L'\n');
		if (np != tail.npos) {
			tail = tail.substr(np + 1);
		}
		else {
			break;
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
		auto fs = std::make_shared<f_fontStack_c>(FetchFontHeights(height));
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
	double tcLeft, tcRight, tcTop, tcBottom;
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

struct f_richTextString_c {
	f_richTextString_c(std::string_view str, col4_t col);
	f_richTextString_c(std::u32string_view str, col4_t col);

	size_t SegmentCount() const;
	std::u32string_view Segment(size_t idx) const;

	using Color = std::array<float, 3>;
	std::map<size_t, Color> colorChanges;
	using SegmentExtent = std::pair<size_t, size_t>;
	std::vector<SegmentExtent> segments;
	std::u32string fullText;
	std::u32string onlyText;

private:
	void AddSegment(size_t startIdx, size_t endIdx, Color color);
};

f_richTextString_c::f_richTextString_c(std::string_view str, col3_t col)
	: f_richTextString_c(ztd::text::transcode(str, ztd::text::utf8, ztd::text::utf32), col)
{}

f_richTextString_c::f_richTextString_c(std::u32string_view str, col3_t col)
	: fullText(str)
{
	std::u32string_view view(fullText);
	size_t segmentStartIdx = 0;
	Color color{ col[0], col[1], col[2] };
	for (size_t charIdx = 0; charIdx < fullText.size();) {
		auto tail = view.substr(charIdx);
		if (int escLen = IsColorEscape(tail)) {
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
		AddSegment(segmentStartIdx, fullText.size(), color);
	}
}

void f_richTextString_c::AddSegment(size_t startIdx, size_t endIdx, Color color)
{
	size_t outStartIdx = onlyText.size();
	if (colorChanges.empty() || colorChanges.rbegin()->second != color) {
		colorChanges[outStartIdx] = color;
	}
	if (startIdx != endIdx) {
		onlyText.append(fullText.begin() + startIdx, fullText.begin() + endIdx);
		size_t outEndIdx = onlyText.size();
		segments.push_back({ outStartIdx, outEndIdx });
	}
}

size_t f_richTextString_c::SegmentCount() const
{
	return segments.size();
}

std::u32string_view f_richTextString_c::Segment(size_t idx) const
{
	auto segment = segments[idx];
	return std::u32string_view(fullText).substr(segment.first, segment.second - segment.first);
}

void r_font_c::DrawTextLine(scp_t pos, int align, int height, col4_t col, std::u32string_view codepoints)
{
	std::u32string_view tail(codepoints);
	f_richTextString_c richStr(tail, col);
	// Check if the line is visible
	if (pos[Y] >= renderer->sys->video->vid.size[1] || pos[Y] <= -height) {
		if (!richStr.colorChanges.empty()) {
			auto c = richStr.colorChanges.rbegin()->second;
			col4_t col{ c[0], c[1], c[2], 1.0f };
			renderer->curLayer->Color(col);
		}
		return;
		// Just process the colour codes
		while (!tail.empty() && tail[0] != U'\n') {
			// Check for escape character
			int escLen = IsColorEscape(tail);
			if (escLen) {
				ReadColorEscape(tail, col);
				col[3] = 1.0f;
				renderer->curLayer->Color(col);
				tail = tail.substr(escLen);
				continue;
			}
			tail = tail.substr(1);
		}
		return;
	}

	auto stack = FetchFontStack(height);

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

	// Segment and shape text
	f_textLayout_s layout;
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
					layout.chunks.push_back(std::move(*deferredChunk));
				}
				else if (auto* pSpan = std::get_if<PendingSpan>(&span)) {
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

					std::stack<WorkSpan> newWork;
					bool seekingGood = true;
					for (size_t glyphIdx = 0; glyphIdx < glyphCount;) {
						auto giI = glyphInfos + glyphIdx;
						auto gpI = glyphPositions + glyphIdx;
						auto& gi = *giI;
						if (gi.codepoint > 0) {
							auto giJ = std::find_if(giI + 1, glyphInfosEnd, [](auto& gi) { return gi.codepoint == 0; });
							auto n = giJ - giI;
							f_textLayout_s::Chunk goodChunk;
							for (auto glyphOff = 0; glyphOff < n; ++glyphOff) {
								f_glyphLayout_s glyph(pSpan->nextFontId, *(giI + glyphOff), *(gpI + glyphOff));
								goodChunk.push_back(glyph);
							}
							if (newWork.empty()) {
								layout.chunks.push_back(std::move(goodChunk));
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
									layout.chunks.push_back(std::move(tofuChunk));
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
					while (!newWork.empty()) {
						spanProcessQueue.push_front(newWork.top());
						newWork.pop();
					}
				}
			}
		}
	}

	// Calculate the string position
	double x = pos[X];
	double y = pos[Y];
	if (align != F_LEFT) {
		// Calculate the real width of the string
		double width = StringWidthInternal(stack, tail);
		switch (align) {
		case F_CENTRE:
			x = floor((renderer->sys->video->vid.size[0] - width) / 2.0f + pos[X]);
			break;
		case F_RIGHT:
			x = floor(renderer->sys->video->vid.size[0] - width - pos[X]);
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
	double startX = x;
	double startY = y;

	// Render with HarfBuzz-assisted FreeType
	if (1) {
		std::shared_ptr<hb_buffer_t> buf(hb_buffer_create(), &hb_buffer_destroy);
		hb_buffer_add_utf32(buf.get(), (const uint32_t*)tail.data(), (int)tail.size(), 0, -1);
		hb_buffer_guess_segment_properties(buf.get());
		hb_shape(stack->Font(0)->hbFont.get(), buf.get(), nullptr, 0);
		unsigned int glyphCount;
		hb_glyph_info_t* glyphInfo = hb_buffer_get_glyph_infos(buf.get(), &glyphCount);
		hb_glyph_position_t* glyphPos = hb_buffer_get_glyph_positions(buf.get(), &glyphCount);

		x = startX;
		y = startY;

		double baseline = stack->Baseline();
		y += baseline;

		for (auto& chunk : layout.chunks) {
			for (auto& info : chunk) {
				auto colorI = richStr.colorChanges.lower_bound(info.cluster);
				if (colorI != richStr.colorChanges.end()) {
					auto change = colorI->second;
					col4_t color{ change[0], change[1], change[2], 1.0f };
					renderer->curLayer->Color(color);
				}
				f_glyphMapping_s gm{ info.fontId, info.glyphId };

				auto sprite = stack->SpriteForGlyphMapping(gm);
				if (sprite.tex) {
					auto& extents = sprite.extents;
					auto vp = renderer->curViewport;
					double dstX0 = x + info.offX + extents.bitmap_left;
					double dstX1 = dstX0 + extents.bitmap_width;
					double dstY0 = y - info.offY - extents.bitmap_top;
					double dstY1 = dstY0 + extents.bitmap_height;
					renderer->curLayer->Bind(sprite.tex);
					renderer->curLayer->Quad(
						sprite.tcLeft, sprite.tcTop, dstX0, dstY0,
						sprite.tcRight, sprite.tcTop, dstX1, dstY0,
						sprite.tcRight, sprite.tcBottom, dstX1, dstY1,
						sprite.tcLeft, sprite.tcBottom, dstX0, dstY1
					);
				}

				x += info.advX;
				y += info.advY;
			}
		}
		if (!richStr.colorChanges.empty()) {
			auto change = richStr.colorChanges.rbegin()->second;
			col4_t color{ change[0], change[1], change[2], 1.0f };
			renderer->curLayer->Color(color);
		}
	}
}

void r_font_c::Draw(scp_t pos, int align, int height, col4_t col, const char* str)
{
	if (*str == 0) {
		pos[Y] += height;
		return;
	}

	// Prepare for rendering
	renderer->curLayer->Color(col);

	// Separate into lines and render them
	auto codepoints = ztd::text::transcode(std::string_view(str), ztd::text::utf8, ztd::text::utf32);
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
	Draw(pos, align, height, col, str);
}
