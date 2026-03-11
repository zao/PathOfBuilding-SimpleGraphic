// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Render Main Header
//

// =============
// Configuration
// =============

#define R_MAXSHADERS 65536
constexpr const bool debug_d3d11 = false;

#include <array>
#include <chrono>
#include <deque>
#include <imgui.h>
#include <map>
#include <vector>

#include <d3d11.h>
#include <d3dcompiler.h>

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

// Render layer
class r_layer_c {
public:
	std::vector<std::byte> cmdStorage;
	size_t	cmdCursor{};
	size_t	numCmd{};

	int		layer;
	int		subLayer;

	r_layer_c(r_renderer_c* renderer, int i_layer, int i_subLayer);
	~r_layer_c();

	void	SetViewport(r_viewport_s* viewport);
	void	SetBlendMode(int mode);
	void	Bind(r_tex_c* tex);
	void	Color(col4_t col);
	void	Quad(float s0, float t0, float x0, float y0, float s1, float t1, float x1, float y1, float s2, float t2, float x2, float y2, float s3, float t3, float x3, float y3, int stackLayer = 0, int maskLayer = -1);
	void	Render(class RenderStrategy&);
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

class r_dx11_c
{
public:
	r_dx11_c(sys_IMain* sys);

	sys_IMain* sys{};

	CComPtr<ID3D11Device> device;
	CComPtr<ID3D11DeviceContext> ctx;
	CComPtr<ID3DUserDefinedAnnotation> annotation;
	CComPtr<IDXGISwapChain> swap_chain;
	CComPtr<ID3D11RenderTargetView> swap_rtv;
	D3D_FEATURE_LEVEL feature_level{};

	DXGI_SWAP_CHAIN_DESC scd{};
	std::unordered_map<r_blendMode_e, CComPtr<ID3D11BlendState>> blendStates;

	void ResizeIfNeeded(glm::ivec2 size);
};

// Renderer Main Class
class r_renderer_c: public r_IRenderer, public conCmdHandler_c {
public:
	// Interface
	void	Init(r_featureFlag_e features);
	void	Shutdown();

	void	BeginFrame();
	void	EndFrame();
	
	r_shaderHnd_c* RegisterShader(std::string_view shname, int flags);
	r_shaderHnd_c* RegisterShaderFromImage(std::unique_ptr<image_c> img, int flags);
	void	GetShaderImageSize(r_shaderHnd_c* hnd, int &width, int &height);
	void	SetShaderLoadingPriority(r_shaderHnd_c* hnd, int pri);
	void	PumpShaders();
	void	PurgeShaders();
	int		GetTexAsyncCount();

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
	void	DrawString(float x, float y, int align, int height, const col4_t col, int font, const char* str);
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

	sys_IMain* sys = nullptr;

	std::shared_ptr<r_dx11_c> dx11;

	r_ITexManager* texMan = nullptr;	// Texture manager interface

	const bool	texNonPOT = true;			// Non power-of-2 textures supported?
	const dword	texMaxDim = 16384u;				// Maximum texture dimension
	const bool	texBC7 = true;				// BC7 textures supported?

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

	r_viewport_s curViewport{}; // Current viewport
	int		curBlendMode = 0;	// Current blend mode

	int		numShader = 0;
	class r_shader_c *shaderList[R_MAXSHADERS] = {};

	struct ShaderProgram
	{
		r_dx11_c* dx11{};
		CComPtr<ID3D11VertexShader> vs;
		CComPtr<ID3D11PixelShader> ps;

		// TODO(zao): store binding information here from reflection
		CComPtr<ID3DBlob> vsBytecode;
		CComPtr<ID3D11ShaderReflection> vsReflect, psReflect;
		D3D11_SHADER_DESC vsDesc, psDesc;
	};

	ShaderProgram tintedTextureProgram{};

	int		numLayer = 0;
	int		layerListSize = 0;
	r_layer_c** layerList = nullptr;
	r_layer_c* curLayer = nullptr;

	int		layerCmdBinCount = 0;
	int		layerCmdBinSize = 0;
	struct r_layerCmd_s** layerCmdBin = nullptr;

	struct RenderTarget {
		int		width = -1, height = -1;
		CComPtr<ID3D11Texture2D> colorTexture;
		CComPtr<ID3D11RenderTargetView> rtv;
		CComPtr<ID3D11ShaderResourceView> srv;
		CComPtr<ID3D11SamplerState> colorSampler;

		CComPtr<ID3D11VertexShader> vs;
		CComPtr<ID3D11PixelShader> ps;
		CComPtr<ID3D11InputLayout> inputLayout;
		D3D11_SHADER_INPUT_BIND_DESC colorTextureBind;
		D3D11_SHADER_INPUT_BIND_DESC colorSamplerBind;

		CComPtr<ID3D11BlendState> blendState;
		CComPtr<ID3D11DepthStencilState> depthState;
		CComPtr<ID3D11RasterizerState> rasterState;
	};

	bool apiDpiAware{};
	int dpiScaleOverridePercent = 0;
	RenderTarget rttMain[2];
	CComPtr<ID3D11SamplerState> rttIntegerScalingSampler, rttLinearScalingSampler;
	int	presentRtt = 0;

	std::vector<uint8_t> lastFrameHash{};

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

	struct SamplerStateCache
	{
		struct Parameters
		{
			D3D11_FILTER Filter;
			D3D11_TEXTURE_ADDRESS_MODE AddressU;
			D3D11_TEXTURE_ADDRESS_MODE AddressV;
			UINT MaxAnisotropy;

			bool operator < (const Parameters& rhs) const noexcept;
		};
		CComPtr<ID3D11SamplerState> MakeState(Parameters desc);

		CComPtr<ID3D11Device> device;
		std::map<Parameters, CComPtr<ID3D11SamplerState>> samplerStates;
	};

	SamplerStateCache samplerStateCache;

	std::chrono::time_point<std::chrono::steady_clock> beginFrameToc;
	FrameStats frameStats;

	bool	elideFrames = false;
	bool	inhibitElision = false;
	bool	debugImGui = false;
	bool	debugLayers = false;

	int		takeScreenshot = 0;
	void	DoScreenshot(image_c* i, int type, const char* ext);

	void	C_Screenshot(IConsole* conHnd, args_c &args);

	RenderTarget& GetDrawRenderTarget();
	RenderTarget& GetPresentRenderTarget();
};
