// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Render Main Header
//

// =============
// Configuration
// =============

#define R_MAXSHADERS 65536

#include <array>
#include <chrono>
#include <deque>
#include <imgui.h>
#include <map>
#include <unordered_set>
#include <vector>

// =======
// Classes
// =======

// Render viewport
struct r_viewport_s {
	int	x;
	int	y;
	int	width;
	int height;
};

struct r_layerId_s
{
	int layer{};
	int subLayer{};

	auto operator<=>(const r_layerId_s&) const = default;
};

// Render layer
class r_layer_c {
public:
	std::vector<std::byte> cmdStorage;
	size_t	cmdCursor{};
	size_t	numCmd{};
	std::unordered_set< std::shared_ptr< r_tex_c > > referencedTextures; // keeps textures alive for the duration of the frame

	r_layerId_s id;

	r_layer_c(r_renderer_c* renderer, r_layerId_s id);
	r_layer_c(r_renderer_c* renderer, int i_layer, int i_subLayer);
	~r_layer_c();

	void	SetViewport(r_viewport_s* viewport);
	void	SetBlendMode(int mode);
	void	Bind(const std::shared_ptr<r_tex_c>& tex);
	void	Color(col4_t col);
	void	Quad(float s0, float t0, float x0, float y0, float s1, float t1, float x1, float y1, float s2, float t2, float x2, float y2, float s3, float t3, float x3, float y3, int stackLayer = 0, int maskLayer = -1);
	bool	Render();
	void    Discard();

	struct CmdHandle {
		uint32_t offset;
		struct r_layerCmd_s* cmd;
	};

	CmdHandle GetFirstCommand();
	bool GetNextCommand(CmdHandle& handle);

private:
	r_renderer_c* renderer;

	struct r_layerCmd_s* NewCommand(size_t size);
};

// Renderer Main Class
class r_renderer_c: public r_IRenderer, public conCmdHandler_c {
public:
	// Interface
	void	Init(r_featureFlag_e features);
	void	Shutdown();

	void	BeginFrame();
	void	EndFrame();
	
	r_shaderHnd_c* RegisterShader(std::u8string_view shname, int flags);
	r_shaderHnd_c* RegisterShaderFromImage(std::unique_ptr<image_c> img, int flags);
	void	GetShaderImageSize(r_shaderHnd_c* hnd, int &width, int &height);
	void	SetShaderLoadingPriority(r_shaderHnd_c* hnd, int pri);
	void	PumpShaders();
	void	PurgeShaders();
	int		GetTexAsyncCount();

	void	SetClearColor(const col4_t col);
	void	SetDrawLayer(int layer, int subLayer = 0);
	void	SetDrawSubLayer(int subLayer);
	int		GetDrawLayer();
	void	SetViewport(int x = 0, int y = 0, int width = 0, int height = 0);
	void	SetBlendMode(int mode);
	void	DrawColor(const col4_t col = NULL);
	void	DrawColor(dword col);
	void	GetDrawColor(col4_t color);
	void	DrawImage(r_shaderHnd_c* hnd, glm::vec2 pos, glm::vec2 extent, glm::vec2 uv1 = { 0, 0 }, glm::vec2 uv2 = { 1, 1 }, int stackLayer = 0, std::optional<int> maskLayer = {});
	void	DrawImageQuad(r_shaderHnd_c* hnd, glm::vec2 p0, glm::vec2 p1, glm::vec2 p2, glm::vec2 p3, glm::vec2 uv0 = { 0, 0 }, glm::vec2 uv1 = { 1, 0 }, glm::vec2 uv2 = { 1, 1 }, glm::vec2 uv3 = { 0, 1 }, int stackLayer = 0, std::optional<int> maskLayer = {});
	void	DrawString(float x, float y, int align, int height, const col4_t col, int font, std::string_view str);
	void	DrawStringFormat(float x, float y, int align, int height, const col4_t col, int font, const char* fmt, ...);
	int		DrawStringWidth(int height, int font, const char* str);
	int		DrawStringCursorIndex(int height, int font, const char* str, int curX, int curY);

	int		VirtualScreenWidth();
	int		VirtualScreenHeight();
	float	VirtualScreenScaleFactor();
	void	SetDpiScaleOverridePercent(int percent);
	int		DpiScaleOverridePercent() const;
	int		VirtualMap(int properValue);
	int		VirtualUnmap(int mappedValue);

	void	ToggleDebugImGui();

	// Encapsulated
	r_renderer_c(sys_IMain* sysHnd);

	BorrowedInterfacePtr<sys_IMain> sys = nullptr;

	std::shared_ptr<r_api_c> api;
	std::shared_ptr<class r_stateGL_s> stateGL;
	std::shared_ptr<class r_stateDX_s> stateDX;

	InterfacePtr<r_ITexManager> texMan = nullptr;	// Texture manager interface

	conVar_c*	r_compress = nullptr;
	conVar_c*	r_screenshotFormat = nullptr;
	conVar_c*	r_layerDebug = nullptr;
	conVar_c*   r_layerOptimize = nullptr;
	conVar_c*   r_layerShuffle = nullptr;
	conVar_c*	r_elideFrames = nullptr;
	conVar_c*	r_drawCull = nullptr;

	r_shaderHnd_c* whiteImage = nullptr;	// White image
	r_shaderHnd_c* blackImage = nullptr;	// Black image

	ImGuiContext* imguiCtx = nullptr;

	r_font_c* fonts[F_NUMFONTS] = {}; // Font objects

	col4_t	drawColor = {};		// Current draw color

	r_viewport_s curViewport; // Current viewport
	int		curBlendMode = 0;	// Current blend mode

	std::vector<std::weak_ptr<class r_shader_c>> shaderList;

	int		numLayer = 0;
	int		layerListSize = 0;
	std::map<r_layerId_s, std::shared_ptr<r_layer_c>> layerList;
	std::shared_ptr<r_layer_c> curLayer;

	bool apiDpiAware{};
	int dpiScaleOverridePercent = 0;
	int	presentRtt = 0;

	uint64_t lastFrameHash{};

	uint64_t totalFrames{};
	uint64_t drawnFrames{};

	struct FrameStats {
		std::deque<float> midFrameStepDurations;
		std::deque<float> endFrameStepDurations;
		std::deque<float> wholeFrameDurations;
		size_t historyLength = 128;

		void AppendDuration(std::deque<float> FrameStats::*series, std::chrono::duration<float> duration) {
			auto& coll = this->*series;
			if (coll.size() >= historyLength) {
				size_t excess = coll.size() + 1 - historyLength;
				coll.erase(coll.begin(), coll.begin() + excess);
			}
			coll.push_back(duration.count());
		}
	};

	std::chrono::time_point<std::chrono::steady_clock> beginFrameToc;
	FrameStats frameStats;

	bool	elideFrames = false;
	bool	inhibitElision = false;
	bool	debugImGui = false;
	bool	debugLayers = false;

	int		takeScreenshot = 0;
	void	DoScreenshot(image_c* i, int type, std::u8string_view ext);

	void	C_Screenshot(IConsole* conHnd, args_c &args);

	size_t GetDrawRenderTarget();
	size_t GetPresentRenderTarget();
};
