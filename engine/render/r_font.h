// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Render Font Header
//

#include <deque>
#include <memory>
#include <unordered_map>

// =======
// Classes
// =======

class f_dynamicFont_c;

// Font
class r_font_c {
public:
	r_font_c(class r_renderer_c* renderer, const char* fontName);
	~r_font_c();

	int		StringWidth(int height, const char* str);
	int		StringCursorIndex(int height, const char* str, int curX, int curY);
	void	Draw(scp_t pos, int align, int height, col4_t col, const char* str);
	void	FDraw(scp_t pos, int align, int height, col4_t col, const char* fmt, ...);
	void	VDraw(scp_t pos, int align, int height, col4_t col, const char* fmt, va_list va);

private:
	int		StringWidthInternal(class f_dynamicFontHeight_c* fh, std::u32string_view str);
	const char*	StringCursorInternal(struct f_fontHeight_s* fh, const char* str, int curX);
	void	DrawTextLine(scp_t pos, int align, int height, col4_t col, const char* str);

	class r_renderer_c* renderer = nullptr;
	int		numFontHeight = 0;
	struct f_fontHeight_s *fontHeights[32] = {};
	int		maxHeight = 0;
	int*	fontHeightMap = nullptr;
	struct f_fontData_s* fontData = nullptr;
	std::shared_ptr<f_dynamicFont_c> dynFont;
};
