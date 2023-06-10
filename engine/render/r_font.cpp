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

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_MODULE_H
#include FT_GLYPH_H

FT_Long FT_CEIL(FT_Long val) { return ((val + 63) & -64) / 64; }

// =======
// Classes
// =======

// Glyph parameters
struct f_glyph_s {
	double	tcLeft = 0.0;
	double	tcRight = 0.0;
	double	tcTop = 0.0;
	double	tcBottom = 0.0;
	int		width = 0;
	int		spLeft = 0;
	int		spRight = 0;
};

struct f_fontData_s {
	std::unique_ptr<unsigned char[]> ttData;
};

struct f_subpixelGlyph_s {
	int x0, y0, x1, y1;
	int dx, dy;
};

struct f_metrics_s {
	float scale;
	int ascent, descent, lineGap;
	float baseline;
	int oversampleH, oversampleV;
};

struct f_stackedGlyph_s {
	f_subpixelGlyph_s sub;
	std::array<int, 2> sheetPos;
	int advance, lsb;
};

struct f_rangeSheet_s {
	struct glyphInfo {
		int width, height;
		float offsetX, offsetY;
		float advanceX;
	};
	r_tex_c* tex{};
	std::unordered_map<size_t, glyphInfo> glyphs;
};

struct f_sheet_s {
	f_sheet_s() : width(1024), height(64), backingData(width* height), skyline(width) {}

	void Grow(int addedHeight) {
		int toAdd = (addedHeight + 63) & -64;
		height += toAdd;
		backingData.resize(width * height);
	}

	std::pair<int, int> AllocateRect(int w, int h) {
		// Sweep line by line to find a row where there's enough of a horizontal gap to
		// fit the desired rectangle.
		// If not enough vertical space exists, grow the storage by the required amount.
		int skylineBase = *std::min_element(skyline.begin(), skyline.end()); // naive but hey
		int retX = -1, retY = -1;
		bool found = false;
		for (int row = skylineBase; row < height && !found; ++row) {
			for (int col = 0; col < width; ++col) {
				int needed = w;
				for (int scan = col; scan < width && needed; ++scan) {
					if (skyline[scan] <= row) {
						--needed;
					}
					else {
						break;
					}
				}
				if (!needed) {
					retX = col;
					retY = row;
					found = true;
					break;
				}
			}
		}

		if (!found) {
			retX = 0;
			retY = height;
		}

		int availY = height - retY;
		if (availY < h) {
			Grow(h - availY);
		}

		std::fill_n(skyline.begin() + retX, w, retY + h);

		return { retX, retY };
	}

	int width, height;
	std::vector<uint32_t> backingData;
	std::vector<int> skyline;
};

// Font height info
struct f_fontHeight_s {
	r_tex_c* tex{};
	r_tex_c* genTex{};
	f_sheet_s sheet;
	int		height{};
	int		numGlyph{};
	f_glyph_s glyphs[128];
	f_metrics_s metrics;
	std::unordered_map<char32_t, size_t> glyphFromCodepoint;
	std::unordered_map<int, f_stackedGlyph_s> spGlyphs;
};

using f_glyphId_t = size_t;

struct f_dynamicGlyphExtent_s {
	int texX, texY;
	int width, height;
	float offsetX, offsetY;
	float advanceX;
};

class f_dynamicFontSheet_c {
public:
	std::shared_ptr<r_tex_c> tex;
	std::unordered_map<f_glyphId_t, f_dynamicGlyphExtent_s> glyphExtents;
};

class f_dynamicFontHeight_c {
public:
	f_dynamicFontHeight_c(r_renderer_c* renderer, FT_Library ftLib, const uint8_t* ttfDataPtr, size_t ttfDataSize, int height);
	~f_dynamicFontHeight_c();
	f_dynamicFontHeight_c(const f_dynamicFontHeight_c&) = delete;
	f_dynamicFontHeight_c& operator = (const f_dynamicFontHeight_c&) = delete;

	class r_renderer_c* renderer{};
	int height{};
	FT_Face ftFace{};
	FT_Size_Metrics ftMetrics{};
	std::deque<std::shared_ptr<f_dynamicFontSheet_c>> sheets;
	std::shared_ptr<r_tex_c> dummyTex;
	std::unordered_map<uint32_t, std::shared_ptr<r_tex_c>> glyphTextures;
};


class f_dynamicFont_c {
public:
	f_dynamicFont_c(r_renderer_c* renderer, const uint8_t* ttfDataPtr, size_t ttfDataSize);
	~f_dynamicFont_c();
	f_dynamicFont_c(const f_dynamicFont_c&) = delete;
	f_dynamicFont_c& operator = (const f_dynamicFont_c&) = delete;

	f_dynamicFontHeight_c* GetHeightInstance(int height);

	class r_renderer_c* renderer{};
	std::vector<uint8_t> ttfData;
	FT_MemoryRec_ ftMem{};
	FT_Library ftLib{};
	std::unordered_map<int, std::shared_ptr<f_dynamicFontHeight_c>> heights;
};

// ===========
// Font Loader
// ===========

f_dynamicFontHeight_c::f_dynamicFontHeight_c(r_renderer_c* renderer, FT_Library ftLib,
	const uint8_t* ttfDataPtr, size_t ttfDataSize, int height)
	: renderer(renderer), height(height)
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
	for (FT_Long glyphIdx = 0; glyphIdx < ftFace->num_glyphs; ++glyphIdx) {
		ftErr = FT_Load_Glyph(ftFace, glyphIdx, FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT);
		assert(!ftErr);
		ftErr = FT_Render_Glyph(ftFace->glyph, FT_RENDER_MODE_LIGHT);
		assert(!ftErr);

		auto& bitmap = ftFace->glyph->bitmap;
		image_c img;
		img.width = bitmap.width;
		img.height = bitmap.rows;
		switch (bitmap.pixel_mode) {
		case FT_PIXEL_MODE_GRAY: {
			img.comp = 4;
			img.type = IMGTYPE_RGBA;
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
			img.comp = 4;
			img.type = IMGTYPE_RGBA;
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
		img.dat = bitmapData.data();
		auto tex = std::make_shared<r_tex_c>(renderer->texMan, &img, TF_NOMIPMAP);
		tex->status = r_tex_c::DONE;
		img.dat = nullptr;
		glyphTextures[glyphIdx] = tex;
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
}

f_dynamicFont_c::~f_dynamicFont_c() {
	heights.clear();
	FT_Done_Library(ftLib);
}

f_dynamicFontHeight_c* f_dynamicFont_c::GetHeightInstance(int height) {
	auto I = heights.find(height);
	if (I == heights.end()) {
		auto dfh = std::make_shared<f_dynamicFontHeight_c>(renderer, ftLib, ttfData.data(), ttfData.size(), height);
		I = heights.insert_or_assign(height, dfh).first;
	}
	return I->second.get();
}

r_font_c::r_font_c(r_renderer_c* renderer, const char* fontName)
	: renderer(renderer)
{
	numFontHeight = 0;
	fontHeightMap = NULL;

	std::string fileNameBase = fmt::format(CFG_DATAPATH "Fonts/{}", fontName);

	// Open info file
	std::string tgfName = fileNameBase + ".tgf";
	std::ifstream tgf(tgfName);
	if (!tgf) {
		renderer->sys->con->Warning("font \"%s\" not found", fontName);
		return;
	}

	maxHeight = 0;
	f_fontHeight_s* fh = NULL;

	// Parse info file
	std::string sub;
	while (std::getline(tgf, sub)) {
		std::string_view subv = sub;
		int h, x, y, w, sl, sr;
		if (subv.substr(0, 5) == "TTF \"" && subv.substr(subv.size() - 2) == "\";") {
			if (fontData) {
				continue;
			}

			std::string_view ttfName = std::string_view(sub).substr(5);
			ttfName = ttfName.substr(0, ttfName.size() - 2);
			OutputDebugStringA(fmt::format("{}\n", ttfName).c_str());
			std::filesystem::path ttfPath = fmt::format(CFG_DATAPATH "Fonts/{}", ttfName);
			std::ifstream is(ttfPath, std::ios::binary);
			auto ttfDataSize = file_size(ttfPath);

			fontData = new f_fontData_s();
			fontData->ttData = std::make_unique<unsigned char[]>(ttfDataSize);
			is.read((char*)fontData->ttData.get(), ttfDataSize);
			dynFont = std::make_shared<f_dynamicFont_c>(renderer, fontData->ttData.get(), ttfDataSize);
		}
		else if (sscanf(sub.c_str(), "HEIGHT %u;", &h) == 1) {
			// New height
			fh = new f_fontHeight_s;
			fontHeights[numFontHeight++] = fh;
			fh->height = h;
			if (h > maxHeight) {
				maxHeight = h;
			}
			fh->numGlyph = 0;

			continue;
			std::string tgaName = fmt::format("{}.{}.tga", fileNameBase, h);
			fh->tex = new r_tex_c(renderer->texMan, tgaName.c_str(), TF_NOMIPMAP);
		}
		else if (fh && sscanf(sub.c_str(), "GLYPH %u %u %u %d %d;", &x, &y, &w, &sl, &sr) == 5) {
			// Add glyph
			int ordinal = fh->numGlyph;
			if (fh->numGlyph >= 128) continue;
			f_glyph_s* glyph = &fh->glyphs[fh->numGlyph++];
			//glyph->tcLeft = (double)x / fh->tex->fileWidth;
			//glyph->tcRight = (double)(x + w) / fh->tex->fileWidth;
			//glyph->tcTop = (double)y / fh->tex->fileHeight;
			//glyph->tcBottom = (double)(y + fh->height) / fh->tex->fileHeight;
			glyph->width = w;
			glyph->spLeft = sl;
			glyph->spRight = sr;
		}
	}

#if 0
	std::filesystem::path glyphRoot(R"(C:\Temp\pob\glyphs)");
	for (int i = 0; i < numFontHeight; ++i) {
		f_fontHeight_s* fh = fontHeights[i];

		if (fontData) {
			const auto* info = &fontData->ttInfo;
			f_metrics_s& metrics = fh->metrics;
			metrics.scale = stbtt_ScaleForPixelHeight(info, (float)fh->height + 2);
			//metrics.scale = stbtt_ScaleForMappingEmToPixels(info, (float)fh->height);
			stbtt_GetFontVMetrics(info, &metrics.ascent, &metrics.descent, &metrics.lineGap);
			metrics.baseline = metrics.ascent * metrics.scale;

			int oversampleH = 3;
			int oversampleV = 1;
			metrics.oversampleH = oversampleH;
			metrics.oversampleV = oversampleV;

			for (int glyphIdx = 0; glyphIdx < info->numGlyphs; ++glyphIdx) {
				f_subpixelGlyph_s g;
				stbtt_GetGlyphBitmapBoxSubpixel(info, glyphIdx, metrics.scale * oversampleH, metrics.scale * oversampleV, 0, 0, &g.x0, &g.y0, &g.x1, &g.y1);
				g.dx = g.x1 - g.x0;
				g.dy = g.y1 - g.y0;

				int rgbaStride = g.dx * 4;
				std::vector<uint8_t> glyphBuf(g.dy * g.dx);
				std::vector<uint8_t> rgbaBuf(g.dy * rgbaStride);
				{
					stbtt_MakeGlyphBitmapSubpixel(info, glyphBuf.data(), g.dx, g.dy, g.dx, metrics.scale * oversampleH, metrics.scale * oversampleV, 0, 0, glyphIdx);
					for (int row = 0; row < g.dy; ++row) {
						for (int col = 0; col < g.dx; ++col) {
							uint8_t* dst = &rgbaBuf[row * rgbaStride + col * 4];
							std::fill_n(dst, 3, 0xFF);
							dst[3] = glyphBuf[row * g.dx + col];
						}
					}
				}

				auto pos = fh->sheet.AllocateRect(g.dx + 2, g.dy + 2);
				{
					auto [dstX, dstY] = pos;
					dstX += 1; // shift by border
					dstY += 1; // -"-
					for (int row = 0; row < g.dy; ++row) {
						auto dstRow = dstY + row;
						uint32_t* dstSpan = &fh->sheet.backingData[dstX + dstRow * fh->sheet.width];
						uint8_t* srcSpan = &rgbaBuf[row * rgbaStride];
						memcpy(dstSpan, srcSpan, g.dx * 4);
					}
					auto& spg = fh->spGlyphs[glyphIdx];
					spg.sub = g;
					spg.sheetPos = { dstX, dstY };
					stbtt_GetGlyphHMetrics(info, glyphIdx, &spg.advance, &spg.lsb);
				}
			}
		}

		auto glyphPath = glyphRoot / fmt::format("{}-{}.png", fontName, fh->height);
		auto& sheet = fh->sheet;
		//stbi_write_png(glyphPath.generic_string().c_str(), sheet.width, sheet.height, 4, sheet.backingData.data(), sheet.width * 4);

		image_c img;
		img.dat = (byte*)sheet.backingData.data();
		img.width = sheet.width;
		img.height = sheet.height;
		img.comp = 4;
		img.type = IMGTYPE_RGBA;
		fh->genTex = new r_tex_c(renderer->texMan, &img, TF_NOMIPMAP);
		fh->genTex->status = r_tex_c::DONE;
		img.dat = nullptr;
	}
#endif

	// Generate mapping of text height to font height
	fontHeightMap = new int[maxHeight + 1];
	memset(fontHeightMap, 0, sizeof(int) * (maxHeight + 1));
	for (int i = 0; i < numFontHeight; i++) {
		int gh = fontHeights[i]->height;
		for (int h = gh; h <= maxHeight; h++) {
			fontHeightMap[h] = i;
		}
		if (i > 0) {
			int belowH = fontHeights[i - 1]->height;
			int lim = (gh - belowH - 1) / 2;
			for (int b = 0; b < lim; b++) {
				fontHeightMap[gh - b - 1] = i;
			}
		}
	}
}

r_font_c::~r_font_c()
{
	// Delete textures
	for (int i = 0; i < numFontHeight; i++) {
		delete fontHeights[i]->tex;
		delete fontHeights[i]->genTex;
		delete fontHeights[i];
	}
	delete fontHeightMap;
	delete fontData;
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

int r_font_c::StringWidthInternal(f_dynamicFontHeight_c* dfh, std::u32string_view str)
{
	FT_Int32 loadFlags = FT_LOAD_NO_BITMAP | FT_LOAD_TARGET_LIGHT | FT_LOAD_ADVANCE_ONLY;
	double width = 0;
	while (!str.empty() && str[0] != U'\n') {
		int escLen = IsColorEscape(str);
		if (escLen) {
			str = str.substr(escLen);
		}
		else if (str[0] == U'\t') {
			FT_Load_Char(dfh->ftFace, U' ', loadFlags);
			auto spWidth = FT_CEIL(dfh->ftFace->glyph->advance.x);
			width += spWidth * 4;
			str = str.substr(1);
		}
		else {
			FT_Load_Char(dfh->ftFace, str[0], loadFlags);
			auto glyph = dfh->ftFace->glyph;
			auto advance = FT_CEIL(glyph->advance.x);
			width += advance;
			str = str.substr(1);
			if (!str.empty()) {
				// Kern to next glyph if any
				auto glyphIdx = glyph->glyph_index;
				auto nextGlyphIdx = FT_Get_Char_Index(dfh->ftFace, str[0]);
				FT_Vector kerning{};
				FT_Get_Kerning(dfh->ftFace, glyphIdx, nextGlyphIdx, FT_KERNING_DEFAULT, &kerning);
				width += FT_CEIL(kerning.x);
			}
		}
	}
	return (int)std::ceil(width);
}

int r_font_c::StringWidth(int height, const char* str)
{
	auto codepoints = ztd::text::transcode(std::string_view(str), ztd::text::utf8, ztd::text::utf32);
	std::u32string_view tail = codepoints;
	f_fontHeight_s* fh = fontHeights[height > maxHeight ? (numFontHeight - 1) : fontHeightMap[height]];
	f_dynamicFontHeight_c* dfh = dynFont->GetHeightInstance(height);
	int max = 0;
	while (!tail.empty()) {
		if (tail[0] != U'\n') {
			int lw = (int)(StringWidthInternal(dfh, tail));
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

const char* r_font_c::StringCursorInternal(f_fontHeight_s* fh, const char* str, int curX)
{
	int x = 0;
	while (*str && *str != '\n') {
		int escLen = IsColorEscape(str);
		if (escLen) {
			str += escLen;
		}
		else if (*str == '\t') {
			int spWidth = fh->glyphs[' '].width + fh->glyphs[' '].spLeft + fh->glyphs[' '].spRight;
			x += spWidth << 1;
			if (curX <= x) {
				break;
			}
			x += spWidth << 1;
			str++;
		}
		else {
			x += fh->glyphs[*str].width + fh->glyphs[*str].spLeft + fh->glyphs[*str].spRight;
			if (curX <= x) {
				break;
			}
			str++;
		}
	}
	return str;
}

int	r_font_c::StringCursorIndex(int height, const char* str, int curX, int curY)
{
	f_fontHeight_s* fh = fontHeights[height > maxHeight ? (numFontHeight - 1) : fontHeightMap[height]];
	int lastIndex = 0;
	int lineY = height;
	curX = (int)(curX / (double)height * fh->height);
	const char* lptr = str;
	while (1) {
		lastIndex = (int)(StringCursorInternal(fh, lptr, curX) - str);
		if (curY <= lineY) {
			break;
		}
		const char* nptr = strchr(lptr, '\n');
		if (nptr) {
			lptr = nptr + 1;
		}
		else {
			break;
		}
		lineY += height;
	}
	return lastIndex;
}

void r_font_c::DrawTextLine(scp_t pos, int align, int height, col4_t col, const char* rawStr)
{
	auto codepoints = ztd::text::transcode(std::string_view(rawStr), ztd::text::utf8, ztd::text::utf32);
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

	// Find best height to use
	f_fontHeight_s* fh = fontHeights[height > maxHeight ? (numFontHeight - 1) : fontHeightMap[height]];
	f_dynamicFontHeight_c* dfh = dynFont->GetHeightInstance(height);

	// Calculate the string position
	double x = pos[X];
	double y = pos[Y];
	if (align != F_LEFT) {
		// Calculate the real width of the string
		double width = StringWidthInternal(dfh, tail);
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
		renderer->curLayer->Bind(dfh->dummyTex.get());

		while (!tail.empty() && tail[0] != U'\n') {
			char32_t ch = tail[0];
			auto glyphIdx = FT_Get_Char_Index(dfh->ftFace, ch);

			if (int escLen = IsColorEscape(tail)) {
				ReadColorEscape(tail, col);
				col[3] = 1.0f;
				renderer->curLayer->Color(col);
				tail = tail.substr(escLen);
				continue;
			}

			if (tail[0] == U'\t') {
				// TODO(LV): Handle tabs
				err = FT_Load_Glyph(dfh->ftFace, glyphIdx, loadFlags);
				float advanceX = dfh->ftFace->glyph->advance.x / 64.0f;
				x += advanceX * 4;
				tail = tail.substr(1);
				continue;
			}

			// Skip characters without glyphs - maybe filter out control characters and draw tofu instead?
			//if (glyphIdx == 0) {
			//	tail = tail.substr(1);
			//	continue;
			//}

			err = FT_Load_Glyph(dfh->ftFace, glyphIdx, loadFlags);

			auto glyph = dfh->ftFace->glyph;
			double advanceX = glyph->advance.x / 64.0f;
			double tcLeft = 0.0f, tcRight = 1.0f, tcTop = 0.0f, tcBottom = 1.0f;
			double dstX0 = x + 1, dstX1 = x + advanceX - 1;
			double dstY0 = y + 1, dstY1 = dstY0 + height - 1;
			auto vp = renderer->curViewport;
			auto lerp = [](auto a, auto b, auto k) { return a * (1.0 - k) + b * k; };
			//dstX0 = lerp(vp.x, vp.x + vp.width, 0.25f);
			//dstX1 = lerp(vp.x, vp.x + vp.width, 0.75f);
			//dstY0 = lerp(vp.y, vp.y + vp.height, 0.25f);
			//dstY1 = lerp(vp.y, vp.y + vp.height, 0.75f);
			dstX0 = x + glyph->bitmap_left;
			dstX1 = dstX0 + glyph->bitmap.width;
			dstY0 = y + FT_CEIL(dfh->ftMetrics.ascender) - glyph->bitmap_top;
			dstY1 = dstY0 + glyph->bitmap.rows;
			auto tex = dfh->glyphTextures[glyphIdx];
			renderer->curLayer->Bind(tex.get());
			renderer->curLayer->Quad(
				tcLeft, tcTop, dstX0, dstY0,
				tcRight, tcTop, dstX1, dstY0,
				tcRight, tcBottom, dstX1, dstY1,
				tcLeft, tcBottom, dstX0, dstY1
			);

			x += advanceX;
			tail = tail.substr(1);

			if (!tail.empty()) {
				int nextGlyphIdx = FT_Get_Char_Index(dfh->ftFace, tail[0]);
				FT_Vector kerning{};
				err = FT_Get_Kerning(dfh->ftFace, glyphIdx, nextGlyphIdx, FT_KERNING_DEFAULT, &kerning);
				if (!err) {
					x += FT_CEIL(kerning.x);
				}
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
	const char* lptr = str;
	while (*lptr) {
		if (*lptr != '\n') {
			DrawTextLine(pos, align, height, col, lptr);
		}
		pos[Y] += height;
		const char* nptr = strchr(lptr, '\n');
		if (nptr) {
			lptr = nptr + 1;
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
