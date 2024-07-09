// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Render Font Header
//

#include <deque>
#include <memory>
#include <optional>
#include <string_view>
#include <unordered_map>

// =======
// Classes
// =======

class f_dynamicFont_c;
class f_dynamicFontHeight_c;

struct f_glyphAllocation_s {
	int sheetIndex;
	float tcX0, tcX1, tcY0, tcY1;
};

// Dynamic atlas for fonts
class r_fontAtlas_c {
public:
	r_fontAtlas_c(class r_renderer_c* renderer);
	~r_fontAtlas_c();
	r_fontAtlas_c(const r_fontAtlas_c&) = delete;
	r_fontAtlas_c& operator = (const r_fontAtlas_c&) = delete;

	using RectHandle = size_t;
	using SheetHandle = size_t;

	RectHandle AllocateGlyphRect(const uint8_t* sourceBitmapData, int bitmapWidth, int bitmapHeight);
	void PackNewRects();
	const f_glyphAllocation_s* LookupRect(RectHandle rh);
	r_tex_c* LookupSheet(SheetHandle sh);

private:
	void UpdateTexture();
	void PushBlankTexture();
	bool NeedPacking() const;
	r_renderer_c* renderer{};

	struct Request {
		uintptr_t stageOffset;
		int width, height;
	};
	std::vector<Request> requestedRects;
	size_t requestBaseIndex{};
	std::vector<uint8_t> stagedBitmapStorage;

	std::shared_ptr<class f_rectPackState_c> packState;

	std::vector<f_glyphAllocation_s> allocatedRects;

	std::vector<uint8_t> cpuTex;
	std::vector<std::shared_ptr<r_tex_c>> textures;
};

class f_fontStack_c {
public:
	explicit f_fontStack_c(std::vector<f_dynamicFontHeight_c*> heights);

	struct f_glyphMapping_s GlyphMappingFromChar(char32_t ch) const;

	double Baseline() const;
	f_dynamicFontHeight_c* Font(size_t fontIdx) const;
	size_t FontCount() const;
	
	struct f_dynMetrics_s MetricsForChar(char32_t ch) const;
	struct f_dynMetrics_s MetricsForGlyphMapping(const f_glyphMapping_s& gm) const;
	struct f_dynKerning_s KerningForChars(char32_t ch0, char32_t ch1) const;
	struct f_dynKerning_s KerningForGlyphMapping(const f_glyphMapping_s& gm0, const f_glyphMapping_s& gm1) const;

	struct f_glyphSprite_s SpriteForChar(char32_t ch) const;
	struct f_glyphSprite_s SpriteForGlyphMapping(f_glyphMapping_s gm) const;

private:
	std::vector<f_dynamicFontHeight_c*> heights;
};

// Font
class r_font_c {
public:
	r_font_c(class r_renderer_c* renderer, const char* fontName);
	~r_font_c();

	int		StringWidth(int height, std::u32string_view str);
	int		StringCursorIndex(int height, std::u32string_view str, int curX, int curY);
	void	Draw(scp_t pos, int align, int height, col4_t col, std::u32string_view str);
	void	FDraw(scp_t pos, int align, int height, col4_t col, const char* fmt, ...);
	void	VDraw(scp_t pos, int align, int height, col4_t col, const char* fmt, va_list va);

	static void RunStatisticsUI(bool* show);

private:
	friend f_fontStack_c;
	int		StringWidthInternal(class f_fontStack_c* stack, std::u32string_view codepoints);
	std::u32string_view	StringCursorInternal(class f_fontStack_c* fh, std::u32string_view codepoints, int curX);
	void	DrawTextLine(scp_t pos, int align, int height, col4_t col, std::u32string_view codepoints);
	
	f_fontStack_c* FetchFontStack(int height);
	std::vector<class f_dynamicFontHeight_c*> FetchFontHeights(int height);

	class r_renderer_c* renderer = nullptr;
	std::vector<std::shared_ptr<struct f_fontData_s>> perFontData;
	std::vector<std::shared_ptr<f_dynamicFont_c>> dynFonts;
	std::unordered_map<int, std::shared_ptr<f_fontStack_c>> fontStackForHeight;
};
