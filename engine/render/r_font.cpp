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
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <variant>
#include <ztd/text.hpp>

#include "stb_image_write.h"

#define STB_RECT_PACK_IMPLEMENTATION 1
#include <stb_rect_pack.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_MODULE_H
#include FT_GLYPH_H

FT_Long FT_CEIL(FT_Long val) { return ((val + 63) & -64) / 64; }

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
	std::shared_ptr<r_tex_c> dummyTex;
	std::unordered_map<uint32_t, r_fontAtlas_c::RectHandle> glyphSlots;
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

f_glyphMapping_s r_font_c::GlyphMappingFromChar(char32_t ch) {
	for (size_t fontIdx = 0; fontIdx < dynFonts.size(); ++fontIdx) {
		if (auto glyphIdx = dynFonts[fontIdx]->GlyphFromChar(ch)) {
			return { fontIdx, glyphIdx };
		}
	}
	return { 0, 0 };
}

f_glyphMapping_s f_fontStack_c::GlyphMappingFromChar(char32_t ch) const {
	for (size_t fontIdx = 0; fontIdx < heights.size(); ++fontIdx) {
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
	FT_Error ftErr{};
	ftErr = FT_Load_Glyph(ftFace, glyphIdx, FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT);
	assert(!ftErr);
	ftErr = FT_Render_Glyph(ftFace->glyph, FT_RENDER_MODE_LIGHT);
	assert(!ftErr);

	auto& bitmap = ftFace->glyph->bitmap;
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
	glyphSlots[glyphIdx] = renderer->fontAtlas->AllocateGlyphRect(bitmapData.data(), bitmap.width, bitmap.rows);
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

	ftMetrics = ftFace->size->metrics;
	// TODO(LV): store these somewhere
	//fh->metrics.ascent = (float)FT_CEIL(metrics.ascender);
	//fh->metrics.descent = (float)FT_CEIL(metrics.descender);

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

	std::vector<uint8_t> bitmapData;
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

	// TODO(LV): Initialize atlas with codepoint ranges, record information and metrics in stacked fonts somewhere.
}

r_font_c::~r_font_c()
{
	perFontData.clear();
}

f_dynMetrics_s r_font_c::MetricsForChar(const std::vector<f_dynamicFontHeight_c*> &dynHeights, char32_t ch) {
	FT_Int32 loadFlags = FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT | FT_LOAD_ADVANCE_ONLY;
	auto [fontIdx, glyphIdx] = GlyphMappingFromChar(ch);
	auto dfh = dynHeights[fontIdx];
	FT_Load_Glyph(dfh->ftFace, glyphIdx, loadFlags);
	f_dynMetrics_s ret{};
	ret.advanceX = dfh->ftFace->glyph->advance.x / 64.0f;
	return ret;
}

f_dynMetrics_s f_fontStack_c::MetricsForChar(char32_t ch) const {
	auto gm = GlyphMappingFromChar(ch);
	return MetricsForGlyphMapping(gm);
}

f_dynMetrics_s f_fontStack_c::MetricsForGlyphMapping(const f_glyphMapping_s& gm) const {
	FT_Int32 loadFlags = FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT | FT_LOAD_ADVANCE_ONLY;
	auto dfh = heights[gm.font];
	FT_Load_Glyph(dfh->ftFace, gm.glyphIdx, loadFlags);
	f_dynMetrics_s ret{};
	ret.advanceX = dfh->ftFace->glyph->advance.x / 64.0f;
	return ret;
}

f_dynKerning_s r_font_c::KerningForChars(const std::vector<f_dynamicFontHeight_c*>& dynHeights, char32_t ch0, char32_t ch1) {
	auto [fontIdx, glyphIdx] = GlyphMappingFromChar(ch0);
	auto [nextFontIdx, nextGlyphIdx] = GlyphMappingFromChar(ch1);
	if (fontIdx == nextFontIdx && nextGlyphIdx) {
		auto* dfh = dynHeights[fontIdx];
		FT_Vector kerning{};
		FT_Get_Kerning(dfh->ftFace, glyphIdx, nextGlyphIdx, FT_KERNING_DEFAULT, &kerning);
		f_dynKerning_s ret{};
		ret.x = kerning.x / 64.0f;
		return ret;
	}
	return {};
}

f_dynKerning_s f_fontStack_c::KerningForChars(char32_t ch0, char32_t ch1) const {
	auto gm0 = GlyphMappingFromChar(ch0);
	auto gm1 = GlyphMappingFromChar(ch1);
	return KerningForGlyphMapping(gm0, gm1);
}

f_dynKerning_s f_fontStack_c::KerningForGlyphMapping(const f_glyphMapping_s& gm0, const f_glyphMapping_s& gm1) const {
	if (gm0.font == gm1.font && gm1.glyphIdx) {
		auto dfh = heights[gm0.font];
		FT_Vector kerning{};
		FT_Get_Kerning(dfh->ftFace, gm0.glyphIdx, gm1.glyphIdx, FT_KERNING_DEFAULT, &kerning);
		f_dynKerning_s ret{};
		ret.x = kerning.x / 64.0f;
		return ret;
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

struct f_glyphSprite_s {
	r_tex_c* tex;
	double tcLeft, tcRight, tcTop, tcBottom;
};

f_glyphSprite_s f_fontStack_c::SpriteForChar(char32_t ch) const {
	// TODO(LV): Populate font atlas, both initially at font loading and here on demand.
	f_glyphSprite_s ret{};
	auto [fontIdx, glyphIdx] = GlyphMappingFromChar(ch);
	auto dfh = Font(fontIdx);
	if (auto I = dfh->glyphSlots.find(glyphIdx); I != dfh->glyphSlots.end()) {
		auto rectHandle = I->second;
		auto renderer = dfh->renderer;
		if (auto* alloc = renderer->fontAtlas->LookupRect(rectHandle)) {
			ret.tex = renderer->fontAtlas->LookupSheet(alloc->sheetIndex);
			ret.tcLeft = alloc->tcX0;
			ret.tcRight = alloc->tcX1;
			ret.tcTop = alloc->tcY0;
			ret.tcBottom = alloc->tcY1;
		}
	}
	else {
		dfh->ScheduleGlyphLoad(glyphIdx);
	}
	return ret;
};

void r_font_c::DrawTextLine(scp_t pos, int align, int height, col4_t col, std::u32string_view codepoints)
{
	std::u32string_view tail(codepoints);
	// Check if the line is visible
	if (pos[Y] >= renderer->sys->video->vid.size[1] || pos[Y] <= -height) {
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

	// Render with FreeType.
	if (1) {
		FT_Error err{};
		FT_Int32 loadFlags = FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT;
		FT_Render_Mode renderMode = FT_RENDER_MODE_LIGHT;
		auto dynHeights = FetchFontHeights(height);

		double baseline = stack->Baseline();
		y += baseline;

		while (!tail.empty() && tail[0] != U'\n') {
			char32_t ch = tail[0];

			if (int escLen = IsColorEscape(tail)) {
				ReadColorEscape(tail, col);
				col[3] = 1.0f;
				renderer->curLayer->Color(col);
				tail = tail.substr(escLen);
				continue;
			}

			if (ch == U'\t') {
				// TODO(LV): Handle tabs
				auto metrics = stack->MetricsForChar(U' ');
				x += metrics.advanceX * 4;
				tail = tail.substr(1);
				continue;
			}
			
			auto gm = stack->GlyphMappingFromChar(ch);
			auto& [fontIdx, glyphIdx] = gm;
			auto* dfh = stack->Font(fontIdx);

			// Skip characters without glyphs - maybe filter out control characters and draw tofu instead?
			//if (glyphIdx == 0) {
			//	tail = tail.substr(1);
			//	continue;
			//}

			auto metrics = stack->MetricsForChar(ch);
			err = FT_Load_Glyph(dfh->ftFace, glyphIdx, loadFlags);

			struct f_glyphExtents_s {
				int bitmap_left, bitmap_top, bitmap_width, bitmap_height;
			};

			auto glyph = dfh->ftFace->glyph;
			auto ExtentsForChar = [&](char32_t ch) {
				f_glyphExtents_s ret{};
				ret.bitmap_left = glyph->bitmap_left;
				ret.bitmap_top = glyph->bitmap_top;
				ret.bitmap_width = glyph->bitmap.width;
				ret.bitmap_height = glyph->bitmap.rows;
				return ret;
			};

			auto extents = ExtentsForChar(ch);

			auto sprite = stack->SpriteForChar(ch);
			if (sprite.tex) {
				auto vp = renderer->curViewport;
				double dstX0 = x + extents.bitmap_left;
				double dstX1 = dstX0 + extents.bitmap_width;
				double dstY0 = y - extents.bitmap_top;
				double dstY1 = dstY0 + extents.bitmap_height;
				renderer->curLayer->Bind(sprite.tex);
				renderer->curLayer->Quad(
					sprite.tcLeft, sprite.tcTop, dstX0, dstY0,
					sprite.tcRight, sprite.tcTop, dstX1, dstY0,
					sprite.tcRight, sprite.tcBottom, dstX1, dstY1,
					sprite.tcLeft, sprite.tcBottom, dstX0, dstY1
				);
			}

			x += metrics.advanceX;
			tail = tail.substr(1);

			if (!tail.empty()) {
				auto gm1 = stack->GlyphMappingFromChar(tail[0]);
				auto kerning = stack->KerningForGlyphMapping(gm, gm1);
				x += kerning.x;
			}
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
		if (tail[0] != U'\n') {
			DrawTextLine(pos, align, height, col, tail);
		}
		pos[Y] += height;
		size_t np = tail.find(U'\n');
		if (np != tail.npos) {
			tail = tail.substr(np + 1);
		}
		else {
			break;
		}
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
