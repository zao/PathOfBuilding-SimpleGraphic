// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Render Main
//

#define GLAD_GLES2_IMPLEMENTATION
#define IMGUI_DEFINE_MATH_OPERATORS
#include "r_local.h"

#include "common/base64.h"

#include <algorithm>
#include <array>
#include <filesystem>
#include <fmt/chrono.h>
#include <future>
#include <gsl/span>
#include <map>
#include <numeric>
#include <random>
#include <sstream>
#include <vector>

#include <d3d11_1.h>
#include <dxgi.h>
#include <d3dcompiler.h>
#include <atlbase.h>
#include <atlcom.h>
#include <comdef.h>
#include <glm/gtc/type_ptr.hpp>

#include <imgui_impl_glfw.h>
#include <imgui_impl_dx11.h>
#include <imgui_stdlib.h>

static uint64_t MurmurHash64A(void const* data, int len, uint64_t seed);

// =======
// Classes
// =======

enum r_takeScreenshot_e {
	R_SSNONE,
	R_SSTGA,
	R_SSJPEG,
	R_SSPNG
};

// ============
// Shader Class
// ============

class r_shader_c {
public:
	r_renderer_c* renderer;
	std::string name;
	dword		nameHash;
	int			refCount;
	r_tex_c* tex;

	r_shader_c(r_renderer_c* renderer, std::string_view shname, int flags);
	r_shader_c(r_renderer_c* renderer, std::string_view shname, int flags, std::unique_ptr<image_c> img);
	~r_shader_c();
};

r_shader_c::r_shader_c(r_renderer_c* renderer, std::string_view shname, int flags)
	: renderer(renderer)
{
	name = shname;
	nameHash = StringHash(name.c_str(), 0xFFFF);
	refCount = 0;
	tex = new r_tex_c(renderer->texMan, name, flags);
	if (tex->error) {
		renderer->sys->con->Warning("couldn't load texture '%s'", name.c_str());
	}
}

r_shader_c::r_shader_c(r_renderer_c* renderer, std::string_view shname, int flags, std::unique_ptr<image_c> img)
	: renderer(renderer)
{
	name = shname;
	nameHash = StringHash(name.c_str(), 0xFFFF);
	refCount = 0;
	tex = new r_tex_c(renderer->texMan, std::move(img), flags);
}

r_shader_c::~r_shader_c()
{
	delete tex;
}

// ===================
// Shader Handle Class
// ===================

r_shaderHnd_c::r_shaderHnd_c(r_shader_c* sh)
	: sh(sh)
{
	sh->refCount++;
}

r_shaderHnd_c::~r_shaderHnd_c()
{
	sh->refCount--;
	if (sh->refCount == 0) {
		sh->tex->AbortLoad();
	}
}

struct Mat4 {
	float m[16];

	float const* data() const {
		return m;
	}
};

Mat4 OrthoMatrix(double left, double right, double bottom, double top, double nearVal, double farVal)
{
	Mat4 ret;
	std::fill_n(ret.m, 16, 0.0f);
	ret.m[0] = (float)(2.0f / (right - left));
	ret.m[5] = (float)(2.0f / (top - bottom));
	ret.m[10] = (float)(-2.0f / (farVal - nearVal));
	ret.m[12] = (float)-((right + left) / (right - left));
	ret.m[13] = (float)-((top + bottom) / (top - bottom));
	ret.m[14] = (float)-((farVal + nearVal) / (farVal - nearVal));
	ret.m[15] = 1.0f;
	return ret;
}

// =================
// Layer queue class
// =================

struct r_layerCmd_s {
	enum Command {
		VIEWPORT,
		BLEND,
		BIND,
		COLOR,
		QUAD,
	} cmd;
};

struct r_layerCmdViewport_s {
	r_layerCmd_s::Command cmd;
	r_viewport_s viewport;
};

struct r_layerCmdBlend_s {
	r_layerCmd_s::Command cmd;
	int blendMode;
};

struct r_layerCmdBind_s {
	r_layerCmd_s::Command cmd;
	r_tex_c* tex;
};

struct r_layerCmdColor_s {
	r_layerCmd_s::Command cmd;
	col4_t col;
};

struct r_layerCmdQuad_s {
	r_layerCmd_s::Command cmd;
	struct {
		float s[4];
		float t[4];
		float x[4];
		float y[4];
		int stackLayer, maskLayer;
	} quad;
};

r_layer_c::r_layer_c(r_renderer_c* renderer, int layer, int subLayer)
	: renderer(renderer), layer(layer), subLayer(subLayer)
{
	cmdStorage.resize(1ull << 23);
	cmdCursor = 0;
	numCmd = 0;
}

r_layer_c::~r_layer_c()
{
}

static size_t CommandSize(r_layerCmd_s::Command cmd, size_t extraSize = 0) {
	using Tag = r_layerCmd_s::Command;
	switch (cmd) {
	case Tag::VIEWPORT: return sizeof(r_layerCmdViewport_s);
	case Tag::BLEND: return sizeof(r_layerCmdBlend_s);
	case Tag::BIND: return sizeof(r_layerCmdBind_s);
	case Tag::COLOR: return sizeof(r_layerCmdColor_s);
	case Tag::QUAD: return sizeof(r_layerCmdQuad_s);
	default:
		abort();
	}
}

r_layer_c::CmdHandle r_layer_c::GetFirstCommand()
{
	CmdHandle ret{};
	ret.offset = 0;
	if (cmdCursor > 0) {
		ret.cmd = (r_layerCmd_s*)cmdStorage.data();
	}
	return ret;
}

bool r_layer_c::GetNextCommand(r_layer_c::CmdHandle& handle)
{
	if (handle.cmd == nullptr) {
		return false;
	}
	handle.offset += (uint32_t)CommandSize(handle.cmd->cmd);
	if (handle.offset >= cmdCursor) {
		handle.cmd = nullptr;
		return false;
	}
	handle.cmd = (r_layerCmd_s*)(cmdStorage.data() + handle.offset);
	return true;
}

r_layerCmd_s* r_layer_c::NewCommand(size_t size)
{
	size_t const cmdEnd = cmdCursor + size;
	if (cmdEnd >= cmdStorage.size()) {
		return nullptr;
	}
	auto *ret = (r_layerCmd_s*)(cmdStorage.data() + cmdCursor);
	cmdCursor = cmdEnd;
	++numCmd;
	return ret;
}

void r_layer_c::SetViewport(r_viewport_s* viewport)
{
	if (auto* cmd = (r_layerCmdViewport_s*)NewCommand(CommandSize(r_layerCmd_s::VIEWPORT))) {
		cmd->cmd = r_layerCmd_s::VIEWPORT;
		cmd->viewport.x = viewport->x;
		cmd->viewport.y = viewport->y;
		cmd->viewport.width = viewport->width;
		cmd->viewport.height = viewport->height;
	}
}

void r_layer_c::SetBlendMode(int mode)
{
	if (auto* cmd = (r_layerCmdBlend_s*)NewCommand(CommandSize(r_layerCmd_s::BLEND))) {
		cmd->cmd = r_layerCmd_s::BLEND;
		cmd->blendMode = mode;
	}
}

void r_layer_c::Bind(r_tex_c* tex)
{
	if (auto* cmd = (r_layerCmdBind_s*)NewCommand(CommandSize(r_layerCmd_s::BIND))) {
		cmd->cmd = r_layerCmd_s::BIND;
		cmd->tex = tex;
	}
}

void r_layer_c::Color(col4_t col)
{
	if (auto* cmd = (r_layerCmdColor_s*)NewCommand(CommandSize(r_layerCmd_s::COLOR))) {
		cmd->cmd = r_layerCmd_s::COLOR;
		Vector4Copy(col, cmd->col);
	}
}

void r_layer_c::Quad(float s0, float t0, float x0, float y0, float s1, float t1, float x1, float y1, float s2, float t2, float x2, float y2, float s3, float t3, float x3, float y3, int stackLayer, int maskLayer)
{
	if (auto* cmd = (r_layerCmdQuad_s*)NewCommand(CommandSize(r_layerCmd_s::QUAD))) {
		cmd->cmd = r_layerCmd_s::QUAD;
		cmd->quad.s[0] = s0; cmd->quad.s[1] = s1; cmd->quad.s[2] = s2; cmd->quad.s[3] = s3;
		cmd->quad.t[0] = t0; cmd->quad.t[1] = t1; cmd->quad.t[2] = t2; cmd->quad.t[3] = t3;
		cmd->quad.x[0] = x0; cmd->quad.x[1] = x1; cmd->quad.x[2] = x2; cmd->quad.x[3] = x3;
		cmd->quad.y[0] = y0; cmd->quad.y[1] = y1; cmd->quad.y[2] = y2; cmd->quad.y[3] = y3;
		cmd->quad.stackLayer = stackLayer;
		cmd->quad.maskLayer = maskLayer;
	}
}

// =================
// Geometric queries
// =================

struct r_aabb_s {
	float lo[2];
	float hi[2];
};

r_aabb_s AabbFromCmdQuad(decltype(r_layerCmdQuad_s::quad)& q, r_viewport_s& vp)
{
	r_aabb_s r{
		{+FLT_MAX, +FLT_MAX},
		{-FLT_MAX, -FLT_MAX},
	};
	for (size_t i = 0; i < 4; ++i) {
		r.lo[0] = (std::min)(r.lo[0], (float)q.x[i]);
		r.lo[1] = (std::min)(r.lo[1], (float)q.y[i]);
		r.hi[0] = (std::max)(r.hi[0], (float)q.x[i]);
		r.hi[1] = (std::max)(r.hi[1], (float)q.y[i]);
	}
	r.lo[0] += vp.x;
	r.lo[1] += vp.y;
	r.hi[0] += vp.x;
	r.hi[1] += vp.y;
	return r;
}

r_aabb_s AabbFromViewport(r_viewport_s& vp)
{
	r_aabb_s r{
		{(float)vp.x, (float)vp.y },
		{(float)(vp.x + vp.width), (float)(vp.y + vp.height) },
	};
	return r;
}

bool AabbAabbIntersects(r_aabb_s& a, r_aabb_s& b)
{
	// A.lo <= B.hi && A.hi >= B.lo
	return a.lo[0] <= b.hi[0] && a.lo[1] <= b.hi[1] && a.hi[0] >= b.lo[0] && a.hi[1] >= b.lo[1];
}

struct Vertex {
	float x, y;
	float u, v;
	float r, g, b, a;
	float viewX, viewY, viewW, viewH;
	float texId, stackIdx, maskIdx;
};

struct Batch {
	explicit Batch(r_renderer_c::ShaderProgram& prog);
	Batch(Batch&& rhs);
	Batch& operator = (Batch&& rhs);
	Batch(Batch const&) = delete;
	Batch& operator = (Batch const&) = delete;
	~Batch();

	r_renderer_c::ShaderProgram* prog;

	std::vector<Vertex> vertices;
	CComPtr<ID3D11InputLayout> inputLayout;

	void Execute();
};

Batch::Batch(r_renderer_c::ShaderProgram& prog)
	: prog(&prog)
{
	std::array<D3D11_INPUT_ELEMENT_DESC, 5> ieds{
		D3D11_INPUT_ELEMENT_DESC{"POSITION", 0, DXGI_FORMAT_R32G32_FLOAT, 0, offsetof(Vertex, x), D3D11_INPUT_PER_VERTEX_DATA, 0},
		D3D11_INPUT_ELEMENT_DESC{"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, offsetof(Vertex, u), D3D11_INPUT_PER_VERTEX_DATA, 0},
		D3D11_INPUT_ELEMENT_DESC{"TINT", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, offsetof(Vertex, r), D3D11_INPUT_PER_VERTEX_DATA, 0},
		D3D11_INPUT_ELEMENT_DESC{"VIEWPORT", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, offsetof(Vertex, viewX), D3D11_INPUT_PER_VERTEX_DATA, 0},
		D3D11_INPUT_ELEMENT_DESC{"TEX_ID", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, offsetof(Vertex, texId), D3D11_INPUT_PER_VERTEX_DATA, 0},
	};
	prog.dx11->device->CreateInputLayout(ieds.data(), ieds.size(), prog.vsBytecode->GetBufferPointer(), prog.vsBytecode->GetBufferSize(), &inputLayout);
}

Batch::Batch(Batch&& rhs)
	: prog(rhs.prog)
	, vertices(std::move(rhs.vertices))
	, inputLayout(inputLayout)
{
}

Batch& Batch::operator = (Batch&& rhs) {
	prog = rhs.prog;
	vertices = std::move(rhs.vertices);
	inputLayout = rhs.inputLayout;

	return *this;
}

Batch::~Batch() {}

void Batch::Execute()
{
	if (vertices.empty()) {
		return;
	}

	auto* dev = prog->dx11->device.p;
	auto* ctx = prog->dx11->ctx.p;
	
	gsl::span<const Vertex> verts = vertices;
	D3D11_BUFFER_DESC vb_desc{};
	vb_desc.ByteWidth = verts.size_bytes();
	vb_desc.Usage = D3D11_USAGE_IMMUTABLE;
	vb_desc.BindFlags = D3D11_BIND_VERTEX_BUFFER;

	D3D11_SUBRESOURCE_DATA vb_srd{verts.data()};

	CComPtr<ID3D11Buffer> vb;
	dev->CreateBuffer(&vb_desc, &vb_srd, &vb);
	const UINT stride = sizeof(Vertex);
	const UINT offset = 0u;
	ctx->IASetInputLayout(inputLayout);
	ctx->IASetVertexBuffers(0, 1, &vb.p, &stride, &offset);
	ctx->Draw(verts.size(), 0);
	vertices.clear();
}

struct RenderStrategy {
	virtual ~RenderStrategy() = default;

	virtual void ProcessCommand(r_layerCmd_s* cmd) = 0;
	virtual void Flush() = 0;
	virtual void SetShowStats(bool showStats) { showStats_ = showStats; }

protected:
	bool showStats_{};
};

static std::map<r_blendMode_e, char const*> const s_blendModeString{
	{RB_ALPHA, "RB_ALPHA"},
	{RB_PRE_ALPHA, "RB_PRE_ALPHA"},
	{RB_ADDITIVE, "RB_ADDITIVE"},
};

struct AdjacentMergeStrategy : RenderStrategy {
	AdjacentMergeStrategy(r_layer_c* layer, r_renderer_c* renderer, r_renderer_c::ShaderProgram& prog)
		: layer_(layer), renderer_(renderer), prog_(prog), batch_(prog)
	{
		D3D11_SHADER_INPUT_BIND_DESC tex_bind_desc{};
		prog.psReflect->GetResourceBindingDescByName("s_tex", &tex_bind_desc);
		texLocs_.push_back(tex_bind_desc.BindPoint);
		batchTextureCap_ = texLocs_.size();

		D3D11_SHADER_INPUT_BIND_DESC vs_cb_bind_desc{};
		prog.vsReflect->GetResourceBindingDescByName("CB", &vs_cb_bind_desc);
		frameCbLoc_ = vs_cb_bind_desc.BindPoint;
		struct FrameCbGpu
		{
			glm::mat4 mvpMatrix;
		};
		
		FrameCbGpu frame_cb_gpu{};
		{
			auto& vid = renderer_->sys->video->vid;
			float fbScaleX = vid.fbSize[0] / (float)vid.size[0];
			float fbScaleY = vid.fbSize[1] / (float)vid.size[1];
			int virtualW = renderer_->VirtualScreenWidth();
			int virtualH = renderer_->VirtualScreenHeight();
			// TODO(zao): set up render state like viewport
			//glViewport(0, 0, virtualW, virtualH);
			Mat4 mvpMatrix = OrthoMatrix(0, virtualW, virtualH, 0, -9999, 9999);
			frame_cb_gpu.mvpMatrix = glm::make_mat4(mvpMatrix.data());
		}
		D3D11_BUFFER_DESC cb_desc{};
		cb_desc.ByteWidth = sizeof(FrameCbGpu);
		cb_desc.Usage = D3D11_USAGE_IMMUTABLE;
		cb_desc.BindFlags = D3D11_BIND_CONSTANT_BUFFER;

		D3D11_SUBRESOURCE_DATA cb_srd{&frame_cb_gpu};
		CComPtr<ID3D11Buffer> frame_cb;
		auto* dev = renderer->dx11->device.p;
		auto* ctx = renderer->dx11->ctx.p;
		dev->CreateBuffer(&cb_desc, &cb_srd, &frame_cb);

		ctx->IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
		ctx->VSSetConstantBuffers(frameCbLoc_, 1, &frame_cb.p);
		ctx->VSSetShader(prog.vs, nullptr, 0);
		ctx->PSSetShader(prog.ps, nullptr, 0);
	}

	~AdjacentMergeStrategy() {
	}

	struct BatchKey {
		int blendMode = -1;

		bool operator < (BatchKey const& rhs) const {
			return blendMode < rhs.blendMode;
		}

		bool operator == (BatchKey const& rhs) const {
			return !(*this < rhs) && !(rhs < *this);
		}

		bool operator != (BatchKey const& rhs) const {
			return !(*this == rhs);
		}
	};

	void ProcessCommand(r_layerCmd_s* cmd) override {
		switch (cmd->cmd) {
		case r_layerCmd_s::VIEWPORT: {
			auto* c = (r_layerCmdViewport_s*)cmd;
			nextViewport_ = c->viewport;
			if (showStats_) {
				// ImGui::Text("VIEWPORT: %dx%d @ %d,%d", c->viewport.width, c->viewport.height, c->viewport.x, c->viewport.y);
			}
		} break;
		case r_layerCmd_s::BLEND: {
			auto* c = (r_layerCmdBlend_s*)cmd;
			latchKey_.blendMode = c->blendMode;
			if (showStats_) {
				// ImGui::Text("BLEND: %s", s_blendModeString.at((r_blendMode_e)c->blendMode));
			}
		} break;
		case r_layerCmd_s::BIND: {
			auto* c = (r_layerCmdBind_s*)cmd;
			nextTex_ = c->tex;
			if (showStats_) {
				// ImGui::Text("TEX: %s", c->tex->fileName.c_str());
			}
		} break;
		case r_layerCmd_s::COLOR: {
			auto* c = (r_layerCmdColor_s*)cmd;
			std::copy_n(c->col, 4, tint_.data());
		} break;
		case r_layerCmd_s::QUAD: {
			auto* c = (r_layerCmdQuad_s*)cmd;
			if (showStats_) {
				// ImGui::Text("QUAD");
			}

			// Cull the quad first before it influences any boundary cuts.
			if (!!renderer_->r_drawCull->intVal) {
				auto a = AabbFromCmdQuad(c->quad, nextViewport_);
				auto b = AabbFromViewport(nextViewport_);
				bool intersects = AabbAabbIntersects(a, b);
				if (!intersects) {
					break;
				}
			}

			// If the current batch is incompatible key-wise, dispatch it to get a fresh
			// batch to grow in.
			if (!batch_.batch.vertices.empty() && batch_.key != latchKey_) {
				Dispatch();
			}
			batch_.key = latchKey_;

			// Check current (and only) batch if the texture set has the latched texture.
			// If it's there, use its index as vertex attribute.
			// If it's not, insert it if room, otherwise dispatch batch and prepare a fresh one.
			size_t texSlot{};
			{
				auto& textures = batch_.textures;
				auto texI = std::find(textures.begin(), textures.end(), nextTex_);
				if (texI == textures.end()) {
					if (textures.size() == batchTextureCap_) {
						Dispatch();
					}
					texI = textures.insert(textures.end(), nextTex_);
				}
				texSlot = std::distance(textures.begin(), texI);
			}

			Vertex quad[4]{};
			for (int v = 0; v < 4; v++) {
				auto& q = quad[v];
				auto& vp = nextViewport_;
				q.u = c->quad.s[v];
				q.v = c->quad.t[v];
				q.x = c->quad.x[v];
				q.y = c->quad.y[v];
				q.r = tint_[0];
				q.g = tint_[1];
				q.b = tint_[2];
				q.a = tint_[3];
				q.viewX = (float)vp.x;
				q.viewY = (float)vp.y;
				q.viewW = (float)vp.width;
				q.viewH = (float)vp.height;
				q.texId = (float)texSlot;
				q.stackIdx = (float)c->quad.stackLayer;
				q.maskIdx = (float)c->quad.maskLayer;
			}
			// 3-2
			// |/|
			// 0-1
			size_t indices[] = { 0, 1, 2, 0, 2, 3 };
			for (auto idx : indices) {
				batch_.batch.vertices.push_back(quad[idx]);
			}
			totalVertexCount_ += std::size(indices);
		} break;
		}
	}

	void Flush() {
		if (!batch_.batch.vertices.empty()) {
			Dispatch();
		}
		if (showStats_) {
			ImGui::BulletText("Layer %d:%d - %d batches", layer_->layer, layer_->subLayer, batchIndex);
		}
	}

private:
	void Dispatch() {
		auto* dev = renderer_->dx11->device.p;
		auto* ctx = renderer_->dx11->ctx.p;

		auto& batch = batch_.batch;
		auto& textures = batch_.textures;
		size_t vertexCount = batch.vertices.size();

		auto& key = batch_.key;
		auto& lastKey = lastDispatchKey_;

		if (showStats_) {
			ImGui::Text("Batch %d", batchIndex);
			ImGui::Text("%d verts", vertexCount);
		}

		// TODO(zao): set up blend mode
		if (!lastKey || lastKey->blendMode != key.blendMode) {
			if (showStats_) {
				ImGui::Text("New blend mode %s", s_blendModeString.at((r_blendMode_e)key.blendMode));
			}
			//switch (key.blendMode) {
			//case RB_ALPHA:
			//	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			//	break;
			//case RB_PRE_ALPHA:
			//	glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
			//	break;
			//case RB_ADDITIVE:
			//	glBlendFunc(GL_ONE, GL_ONE);
			//	break;
			//}
		}
		{
			const auto& dev_ctx = renderer_->dx11->ctx;
			std::vector<ID3D11ShaderResourceView*> srvs(texLocs_.size());
			for (size_t i = 0, numTex = texLocs_.size(); i < numTex; ++i) {
				if (i < textures.size()) {
					auto tex = textures[i];
					srvs[i] = tex->GetShaderResourceView();
					if (showStats_) {
						ImGui::Text("New tex %d (%s)", tex->texId, tex->fileName.c_str());
					}
				}
			}
			dev_ctx->PSSetShaderResources(0, (UINT)srvs.size(), srvs.data());
		}

		batch.Execute();

		lastDispatchKey_ = key;
		batch_.batch.vertices.clear();
		batch_.textures.clear();

		// TODO(zao): unbind stuff?

		batchIndex += 1;
	}

	r_layer_c* layer_{};
	r_renderer_c* renderer_{};
	r_renderer_c::ShaderProgram& prog_;
	std::vector<UINT> texLocs_;
	UINT frameCbLoc_{};

	CComPtr<ID3D11Buffer> frameCb_;

	size_t batchTextureCap_{};

	struct TexturedBatch {
		explicit TexturedBatch(r_renderer_c::ShaderProgram& prog) : batch(prog) {
			textures.reserve(1ull << 20);
		}

		BatchKey key{};
		Batch batch;
		std::vector<r_tex_c*> textures;
	};

	BatchKey latchKey_{};
	r_viewport_s nextViewport_{};
	r_tex_c* nextTex_{};
	std::optional<BatchKey> lastDispatchKey_;
	TexturedBatch batch_;

	std::array<float, 4> tint_{ 1.0f, 1.0f, 1.0f, 1.0f };

	size_t totalVertexCount_ = 0;
	size_t batchIndex = 0;
};

void r_layer_c::Render()
{
	int const optLevel = renderer->r_layerOptimize->intVal;
	bool const shuffle = renderer->r_layerShuffle->intVal == 1;

	std::unique_ptr<RenderStrategy> strat(new AdjacentMergeStrategy(this, renderer, renderer->tintedTextureProgram));

	if (ID3DUserDefinedAnnotation* annotation = renderer->dx11->annotation; annotation && annotation->GetStatus())
	{
		std::wostringstream oss;
		oss << "Layer " << layer << ", sub-layer " << subLayer;
		annotation->BeginEvent(oss.str().c_str());
	}

	if (strat) {
		bool showStats{};
		if (renderer->debugLayers) {
			if (ImGui::Begin("Layers", &renderer->debugLayers)) {
				std::string heading = fmt::format("Layer {}:{}", layer, subLayer);
				showStats = ImGui::CollapsingHeader(heading.c_str(), ImGuiTreeNodeFlags_DefaultOpen);
			}
		}
		strat->SetShowStats(showStats);

		for (CmdHandle cmdH = GetFirstCommand(); cmdH.cmd != nullptr; GetNextCommand(cmdH)) {
			strat->ProcessCommand(cmdH.cmd);
		}

		strat->Flush();

		if (renderer->debugLayers) {
			ImGui::End();
		}
	}

	if (ID3DUserDefinedAnnotation* annotation = renderer->dx11->annotation; annotation && annotation->GetStatus())
	{
		annotation->EndEvent();
	}
}

void r_layer_c::Discard()
{
	cmdCursor = 0;
	numCmd = 0;
}

// =====================
// r_IRenderer Interface
// =====================

r_IRenderer* r_IRenderer::GetHandle(sys_IMain* sysHnd)
{
	return new r_renderer_c(sysHnd);
}

void r_IRenderer::FreeHandle(r_IRenderer* hnd)
{
	delete (r_renderer_c*)hnd;
}

r_renderer_c::r_renderer_c(sys_IMain* sysHnd)
	: conCmdHandler_c(sysHnd->con), sys(sysHnd)
{
	r_compress = sys->con->Cvar_Add("r_compress", CV_ARCHIVE, "0");
	r_screenshotFormat = sys->con->Cvar_Add("r_screenshotFormat", CV_ARCHIVE, "jpg");
	r_layerDebug = sys->con->Cvar_Add("r_layerDebug", CV_ARCHIVE, "0");
	r_layerOptimize = sys->con->Cvar_Add("r_layerOptimize", CV_ARCHIVE | CV_CLAMP, "1", 0, 1);
	r_layerShuffle = sys->con->Cvar_Add("r_layerShuffle", CV_ARCHIVE | CV_CLAMP, "0", 0, 1);
	r_elideFrames = sys->con->Cvar_Add("r_elideFrames", CV_ARCHIVE | CV_CLAMP, "1", 0, 1);
	r_drawCull = sys->con->Cvar_Add("r_drawCull", CV_ARCHIVE | CV_CLAMP, "1", 0, 1);

	Cmd_Add("screenshot", 0, "[<format>]", this, &r_renderer_c::C_Screenshot);
}

static const std::string s_tintedTextureVertexSource = R"(// Vertex shader for tinted 2D sprites
cbuffer FrameCB : register(b0)
{
    float4x4 mvpMatrix;
};

struct VSInput
{
    float2 vertex: POSIION0;
    float2 texcoord : TEXCOORD0;
    float4 tint : TINT;
    float4 viewport : VIEWPORT;
    float3 texId : TEX_ID;
};

struct PSInput
{
    float4 screenPos : SV_Position;
    float2 texcoord : TEXCOORD0;
    float4 tint : TINT;
    float4 viewport : VIEWPORT;
    float3 texId : TEX_ID;
};

PSInput VSMain(VSInput input)
{
    PSInput result;

    result.texcoord = input.texcoord;
    result.tint = input.tint;
    result.texId = input.texId;
    float2 vp0 = input.viewport.xy + float2(0.0, input.viewport.w);
    float2 vp1 = input.viewport.xy + float2(input.viewport.z, 0.0);
    result.viewport = float4(
        mul(mvpMatrix, float4(vp0, 0.0, 1.0)).xy,
        mul(mvpMatrix, float4(vp1, 0.0, 1.0)).xy);
    float4 pos = mul(mvpMatrix, float4(input.vertex + input.viewport.xy, 0.0, 1.0));
    result.screenPos = pos;
    return result;
}
)";

static const std::string s_tintedTexturePixelSource = R"(// Pixel shader for tinted 2D sprites
Texture2DArray s_tex : register(t0);
SamplerState s_smp : register(s0);

struct PSInput
{
    float4 screenPos : SV_Position;
    float2 texcoord : TEXCOORD0;
    float4 tint : TINT;
    float4 viewport : VIEWPORT;
    float3 texId : TEX_ID;
};

float4 ShadeColor(Texture2DArray tex, SamplerState smp, float2 texcoord, float3 texId)
{
    float4 color = tex.Sample(smp, float3(texcoord, texId.y));
    if (texId.z > -0.5)
        color *= tex.Sample(smp, float3(texcoord, texId.z));
    return color;
}

float4 PSMain(PSInput input) : SV_TARGET
{
    float x = input.screenPos.x, y = input.screenPos.y;
    if (x < input.viewport[0] || y < input.viewport[1] || x >= input.viewport[2] || y >= input.viewport[3])
        discard;

    float4 color = ShadeColor(s_tex, s_smp, input.texcoord, input.texId);
    return color * input.tint;
}
)";

static const std::string s_scaleVsSource = R"(// Vertex shader for upscaling a render target
struct PSOutput
{
    float4 position : SV_POSITION;
    float2 texcoord : TEXCOORD0;
};

PSOutput VSMain(float4 position : POSITION, float2 texcoord : TEXCOORD0)
{
	PSOutput result;
    result.position = position;
	result.texcoord = texcoord;
    return result;
}
)";

static const std::string s_scalePsSource = R"(// Pixel shader for upscaling a render target
Texture2D s_tex;
SamplerState s_smp;

float4 PSMain(float2 texcoord : TEXCOORD0) : SV_TARGET
{
	float3 color = s_tex.Sample(s_smp, texcoord).rgb;
	return float4(color, 1.0);
}
)";

// =============
// Init/Shutdown
// =============

void r_renderer_c::Init(r_featureFlag_e features)
{
	sys->con->PrintFunc("Render Init");

	apiDpiAware = !!(features & F_DPI_AWARE);

	timer_c timer;
	timer.Start();

	// Initialise DX11
	try
	{
		dx11 = std::make_shared<r_dx11_c>(sys);
	}
	catch (std::exception& e)
	{
		sys->Error(fmt::format("DX11 initialisation failed: {}", e.what()).c_str());
	}

	samplerStateCache.device = dx11->device;

	// Initialise texture manager
	texMan = r_ITexManager::GetHandle(this);

	// Initialise shader array
	numShader = 0;
	memset(shaderList, 0, sizeof(shaderList));

	const size_t maxTextureImageUnits = 32;

	struct ShaderCompileResult
	{
		HRESULT hr{S_OK};
		CComPtr<ID3DBlob> code_blob, error_blob;

		bool Success() const noexcept { return SUCCEEDED(hr); }
		_com_error Error() const { return hr; }
		std::string_view CompileErrors() const
		{
			if (error_blob)
				return static_cast<const char*>(error_blob->GetBufferPointer());
			return "";
		}
	};

	const auto CompileShaderSource = [&](std::string_view source, std::string filename, std::string entrypoint, std::string target_model) -> ShaderCompileResult
		{
			ShaderCompileResult ret{};
			ret.hr = D3DCompile(source.data(), source.size(), filename.c_str(), nullptr, nullptr, entrypoint.c_str(), target_model.c_str(),
				D3DCOMPILE_DEBUG | D3DCOMPILE_OPTIMIZATION_LEVEL3 | D3DCOMPILE_ENABLE_STRICTNESS, 0, &ret.code_blob, &ret.error_blob);
			return ret;
		};

	const auto ReflectBytecode = [&](const CComPtr<ID3DBlob>& blob) -> CComPtr<ID3D11ShaderReflection>
		{
			CComPtr<ID3D11ShaderReflection> ret;
			D3DReflect(blob->GetBufferPointer(), blob->GetBufferSize(), IID_PPV_ARGS(&ret));
			return ret;
		};

	// Initialise vertex programs
	{
		auto& prog = tintedTextureProgram;
		prog.dx11 = dx11.get();
		const auto vertexResult = CompileShaderSource(s_tintedTextureVertexSource, "TintedTextureVS.hlsl", "VSMain", "vs_5_0");
		const auto pixelResult = CompileShaderSource(s_tintedTexturePixelSource, "TintedTexturePS.hlsl", "PSMain", "ps_5_0");
		if (!vertexResult.Success())
			sys->Error(fmt::format("Failed to compile tinted vertex shader:\n{}", vertexResult.CompileErrors()).c_str());
		if (!pixelResult.Success())
			sys->Error(fmt::format("Failed to compile tinted pixel shader:\n{}", pixelResult.CompileErrors()).c_str());

		prog.vsBytecode = vertexResult.code_blob;

		if (HRESULT hr = dx11->device->CreateVertexShader(prog.vsBytecode->GetBufferPointer(), prog.vsBytecode->GetBufferSize(), nullptr, &prog.vs); FAILED(hr))
			sys->Error(fmt::format("Could not create tinted vertex shader:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());
		if (HRESULT hr = dx11->device->CreatePixelShader(pixelResult.code_blob->GetBufferPointer(), pixelResult.code_blob->GetBufferSize(), nullptr, &prog.ps); FAILED(hr))
			sys->Error(fmt::format("Could not create tinted pixel shader:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());

		// TODO(zao): reflect information from VS/PS for binding
		prog.vsReflect = ReflectBytecode(prog.vsBytecode);
		prog.psReflect = ReflectBytecode(pixelResult.code_blob);
	}

	// Initialise layer array
	numLayer = 1;
	layerListSize = 16;
	layerList = new r_layer_c * [layerListSize];
	layerList[0] = new r_layer_c(this, 0, 0);

	// Initialise layer command bin
	layerCmdBinCount = 0;
	layerCmdBinSize = 1024;
	layerCmdBin = new r_layerCmd_s * [layerCmdBinSize];

	takeScreenshot = R_SSNONE;

	D3D11_SAMPLER_DESC samplerDesc{};
	samplerDesc.Filter = D3D11_FILTER_MIN_MAG_MIP_POINT;
	samplerDesc.AddressU = samplerDesc.AddressV = samplerDesc.AddressW = D3D11_TEXTURE_ADDRESS_CLAMP;
	samplerDesc.ComparisonFunc = D3D11_COMPARISON_NEVER;
	samplerDesc.MinLOD = -FLT_MAX;
	samplerDesc.MaxLOD = +FLT_MAX;
	HRESULT hr = dx11->device->CreateSamplerState(&samplerDesc, &rttIntegerScalingSampler);
	samplerDesc.Filter = D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT;
	hr = dx11->device->CreateSamplerState(&samplerDesc, &rttLinearScalingSampler);
	
	// Set up DPI-scaling render target
	for (int i = 0; i < 2; ++i) {
		auto& rtt = rttMain[i];
		if (i > 0) {
			rtt = rttMain[0]; // Reuse shared parts like dimensions and program/locations.
		}
		
		if (i == 0) {
			const auto vertexResult = CompileShaderSource(s_scaleVsSource, "ScaleVS.hlsl", "VSMain", "vs_5_0");
			const auto pixelResult = CompileShaderSource(s_scalePsSource, "ScalePS.hlsl", "PSMain", "ps_5_0");
			if (!vertexResult.Success())
				sys->Error(fmt::format("Failed to compile upscale vertex shader:\n{}", vertexResult.CompileErrors()).c_str());
			if (!pixelResult.Success())
				sys->Error(fmt::format("Failed to compile upscale pixel shader:\n{}", pixelResult.CompileErrors()).c_str());

			if (HRESULT hr = dx11->device->CreateVertexShader(vertexResult.code_blob->GetBufferPointer(), vertexResult.code_blob->GetBufferSize(), nullptr, &rtt.vs); FAILED(hr))
				sys->Error(fmt::format("Could not create upscale vertex shader:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());
			if (HRESULT hr = dx11->device->CreatePixelShader(pixelResult.code_blob->GetBufferPointer(), pixelResult.code_blob->GetBufferSize(), nullptr, &rtt.ps); FAILED(hr))
				sys->Error(fmt::format("Could not create upscale pixel shader:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());

			std::array<D3D11_INPUT_ELEMENT_DESC, 2> ieds{
				D3D11_INPUT_ELEMENT_DESC{ "POSITION", 0, DXGI_FORMAT_R32G32B32A32_FLOAT, 0, D3D11_APPEND_ALIGNED_ELEMENT, D3D11_INPUT_PER_VERTEX_DATA, 0 },
				D3D11_INPUT_ELEMENT_DESC{ "TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, D3D11_APPEND_ALIGNED_ELEMENT, D3D11_INPUT_PER_VERTEX_DATA, 0 },
			};
			if (HRESULT hr = dx11->device->CreateInputLayout(ieds.data(), ieds.size(), vertexResult.code_blob->GetBufferPointer(), vertexResult.code_blob->GetBufferSize(), &rtt.inputLayout); FAILED(hr))
				sys->Error(fmt::format("Could not create upscale input layout:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());
			
			const auto vsRefl = ReflectBytecode(pixelResult.code_blob);
			if (HRESULT hr = vsRefl->GetResourceBindingDescByName("s_tex", &rtt.colorTextureBind); FAILED(hr))
				sys->Error(fmt::format("Could not find upscale texture binding:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());
			if (HRESULT hr = vsRefl->GetResourceBindingDescByName("s_smp", &rtt.colorSamplerBind); FAILED(hr))
				sys->Error(fmt::format("Could not find upscale sampler binding:\n{}", NarrowUTF8StringStd(_com_error(hr).ErrorMessage())).c_str());
		}
	}

	// Load render resources
	sys->con->Printf("Loading resources...\n");

	whiteImage = RegisterShader("@white", 0);
	blackImage = RegisterShader("@black", 0);

	imguiCtx = ImGui::CreateContext();
	ImGui::SetCurrentContext(imguiCtx);

	ImGui_ImplGlfw_InitForOpenGL((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplDX11_Init(dx11->device, dx11->ctx);

	fonts[F_FIXED] = new r_font_c(this, "Bitstream Vera Sans Mono");
	fonts[F_VAR] = new r_font_c(this, "Liberation Sans");
	fonts[F_VAR_BOLD] = new r_font_c(this, "Liberation Sans Bold");
	fonts[F_FONTIN_SC] = new r_font_c(this, "Fontin SmallCaps");
	fonts[F_FONTIN_SC_ITALIC] = new r_font_c(this, "Fontin SmallCaps Italic");
	fonts[F_FONTIN] = new r_font_c(this, "Fontin");
	fonts[F_FONTIN_ITALIC] = new r_font_c(this, "Fontin Italic");

	sys->con->Printf("Renderer initialised in %d msec.\n", timer.Get());
}

void r_renderer_c::Shutdown()
{
	sys->con->PrintFunc("Render Shutdown");

	sys->con->Printf("Unloading resources...\n");

	ImGui_ImplDX11_Shutdown();
	ImGui_ImplGlfw_Shutdown();
	ImGui::DestroyContext(imguiCtx);

	delete whiteImage;

	for (int f = 0; f < F_NUMFONTS; f++) {
		delete fonts[f];
	}

	for (int s = 0; s < numShader; s++) {
		delete shaderList[s];
	}

	for (int l = 0; l < numLayer; l++) {
		delete layerList[l];
	}
	delete layerList;
	for (int c = 0; c < layerCmdBinCount; c++) {
		delete layerCmdBin[c];
	}
	delete layerCmdBin;

	for (auto& rtt : rttMain) {
		rtt = {};
	}

	// Shutdown texture manager
	r_ITexManager::FreeHandle(texMan);

	// Shutdown OpenGL
	dx11.reset();

	sys->con->Printf("Renderer shutdown complete.\n");
}

// =================
// Render Management
// =================

void r_renderer_c::PumpShaders()
{
	texMan->ProcessPendingTextureUploads();
	for (size_t idx = 0; idx < numShader; ++idx)
		if (auto* sh = shaderList[idx])
			if (auto tex = sh->tex; tex && tex->status != r_tex_c::DONE) {
				inhibitElision = true;
				break;
			}
}

void r_renderer_c::BeginFrame()
{
	ImGui_ImplDX11_NewFrame();
	ImGui_ImplGlfw_NewFrame();
	ImGui::NewFrame();
	{
		auto& vid = sys->video->vid;

		dx11->ResizeIfNeeded(glm::make_vec2(vid.fbSize));

		int wNew = VirtualScreenWidth();
		int hNew = VirtualScreenHeight();
		bool const wantIntegerScaling = fmodf(vid.dpiScale, 1.0f) < 0.0005f;
		for (int i = 0; i < 2; ++i) {
			auto& rtt = rttMain[i];
			if (rtt.width != wNew || rtt.height != hNew) {
				HRESULT hr = S_OK;
				rtt.colorTexture.Release();
				rtt.srv.Release();
				rtt.rtv.Release();

				rtt.colorSampler = wantIntegerScaling ? rttIntegerScalingSampler : rttLinearScalingSampler;

				D3D11_TEXTURE2D_DESC texDesc{};
				texDesc.Width = wNew;
				texDesc.Height = hNew;
				texDesc.MipLevels = 1;
				texDesc.ArraySize = 1;
				texDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
				texDesc.SampleDesc = {1, 0};
				texDesc.Usage = D3D11_USAGE_DEFAULT;
				texDesc.BindFlags = D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE;
				texDesc.CPUAccessFlags = 0;
				texDesc.MiscFlags = 0;
				hr = dx11->device->CreateTexture2D(&texDesc, nullptr, &rtt.colorTexture);
				hr = dx11->device->CreateShaderResourceView(rtt.colorTexture, nullptr, &rtt.srv);
				hr = dx11->device->CreateRenderTargetView(rtt.colorTexture, nullptr, &rtt.rtv);

				rtt.width = wNew;
				rtt.height = hNew;
			}
		}
	}

	curLayer = layerList[0];

	SetViewport();
	SetBlendMode(RB_ALPHA);
	DrawColor();

	beginFrameToc = std::chrono::steady_clock::now();
}

static int layerCompFunc(const void* va, const void* vb)
{
	r_layer_c* a = *(r_layer_c**)va;
	r_layer_c* b = *(r_layer_c**)vb;
	if (a->layer < b->layer) {
		return -1;
	}
	else if (a->layer > b->layer) {
		return 1;
	}
	else if (a->subLayer < b->subLayer) {
		return -1;
	}
	else {
		return 1;
	}
}

void CVarSliderInt(char const* label, conVar_c* cvar) {
	int curOpt = cvar->intVal;
	if (ImGui::SliderInt(label, &curOpt, cvar->min, cvar->max, "%d", ImGuiSliderFlags_AlwaysClamp | ImGuiSliderFlags_NoInput)) {
		if (curOpt != cvar->intVal) {
			cvar->Set(curOpt);
		}
	}
}

void CVarCheckbox(char const* label, conVar_c* cvar) {
	bool checked = cvar->intVal == 1;
	if (ImGui::Checkbox(label, &checked)) {
		cvar->intVal = +checked;
	}
}

static std::string BinaryUnitPrefix(uint64_t quantity) {
	if (quantity < 1ull<<10) {
		return fmt::format("{} ", quantity);
	}
	if (quantity < 1ull << 20) {
		return fmt::format("{:0.2f} Ki", quantity / 1024.0);
	}
	if (quantity < 1ull << 30) {
		return fmt::format("{:0.2f} Mi", quantity / 1024.0 / 1024.0);
	}
	if (quantity < 1ull << 40) {
		return fmt::format("{:0.2f} Gi", quantity / 1024.0 / 1024.0 / 1024.0);
	}
	if (quantity < 1ull << 50) {
		return fmt::format("{:0.2f} Ti", quantity / 1024.0 / 1024.0 / 1024.0 / 1024.0);
	}
	if (quantity < 1ull << 60) {
		return fmt::format("{:0.2f} Ti", quantity / 1024.0 / 1024.0 / 1024.0 / 1024.0 / 1024.0);
	}
	return fmt::format("{:0.2f} Pi", quantity / 1024.0 / 1024.0 / 1024.0 / 1024.0 / 1024.0);
}

void r_renderer_c::EndFrame()
{
	inhibitElision = false;
	PumpShaders();

	std::chrono::time_point endFrameTic = std::chrono::steady_clock::now();
	frameStats.AppendDuration(&FrameStats::midFrameStepDurations, endFrameTic - beginFrameToc);

	static bool showDemo = false;
	static bool showMetrics = false;
	static bool showHash = false;
	static bool showTiming = false;
	if (debugImGui) {
		if (ImGui::Begin("Debug Hub", &debugImGui)) {
			if (ImGui::Button("ImGui Demo")) {
				showDemo = true;
			}
			if (ImGui::Button("Metrics")) {
				showMetrics = true;
			}
			if (ImGui::Button("Layers")) {
				debugLayers = true;
			}
		}
		ImGui::End();
	}

	if (showDemo) {
		ImGui::ShowDemoWindow(&showDemo);
	}
	if (showMetrics) {
		ImGui::ShowMetricsWindow(&showMetrics);
	}

	r_layer_c** layerSort = new r_layer_c * [numLayer];
	for (int l = 0; l < numLayer; l++) {
		layerSort[l] = layerList[l];
	}
	qsort(layerSort, numLayer, sizeof(r_layer_c*), layerCompFunc);
	if (r_layerDebug->intVal) {
		size_t totalCmd = 0;
		for (int l = 0; l < numLayer; l++) {
			totalCmd += layerSort[l]->numCmd;
			char str[1024];
			sprintf(str, "%zu (%4d,%4d) [%2d]", layerSort[l]->numCmd, layerSort[l]->layer, layerSort[l]->subLayer, l);
			float w = (float)DrawStringWidth(16, F_FIXED, str);
			DrawColor(0x7F000000);
			DrawImage(NULL, { (float)VirtualScreenWidth() - w, VirtualScreenHeight() - (l + 2) * 16.0f }, { w, 16 });
			DrawStringFormat(0, VirtualScreenHeight() - (l + 2) * 16.0f, F_RIGHT, 16, colorWhite, F_FIXED, str);
		}
		char str[1024];
		sprintf(str, "%zu", totalCmd);
		float w = (float)DrawStringWidth(16, F_FIXED, str);
		DrawColor(0xAF000000);
		DrawImage(NULL, { (float)VirtualScreenWidth() - w, VirtualScreenHeight() - 16.0f }, { w, 16 });
		DrawStringFormat(0, VirtualScreenHeight() - 16.0f, F_RIGHT, 16, colorWhite, F_FIXED, str);
	}

	std::optional<std::pair<int, int>> layerBreak;
	if (debugLayers) {
		if (ImGui::Begin("Layers", &debugLayers)) {
			ImGui::Text("Layers: %d", numLayer);
			ImGui::Text("%d out of %d frames drawn.", drawnFrames, totalFrames);
			CVarSliderInt("Optimization", r_layerOptimize);
			CVarCheckbox("Elide identical frames", r_elideFrames);
			ImGui::BeginDisabled(true);
			ImGui::Checkbox("Elision inhibited", &inhibitElision);
			ImGui::EndDisabled();
			CVarCheckbox("Draw command culling", r_drawCull);

			size_t totalFootprint{}, totalDenseFootprint{};
			for (int l = 0; l < numLayer; ++l) {
				size_t byteAcc{};
				auto layer = layerSort[l];
				size_t const numCmd = layer->numCmd;
				totalFootprint += numCmd * sizeof(r_layerCmdQuad_s); // legacy footprint
				totalDenseFootprint += layer->cmdCursor;
			}

			ImGui::Text("Total payload footprint: %sB", BinaryUnitPrefix(totalFootprint).c_str());
			ImGui::Text("Total dense footprint: %sB", BinaryUnitPrefix(totalDenseFootprint).c_str());

			size_t totalCmd{};
			if (ImGui::BeginTable("Layer stats", 7, ImGuiTableFlags_Borders | ImGuiTableFlags_SizingFixedFit)) {
				ImGui::TableSetupColumn("Index");
				ImGui::TableSetupColumn("Layer");
				ImGui::TableSetupColumn("Sublayer");
				ImGui::TableSetupColumn("Command count");
				ImGui::TableSetupColumn("Dense");
				ImGui::TableSetupColumn("Debug");
				ImGui::TableHeadersRow();
				for (int l = 0; l < numLayer; ++l) {
					auto layer = layerSort[l];
					ImGui::PushID(layer->layer);
					ImGui::PushID(layer->subLayer);
					totalCmd += layer->numCmd;
					ImGui::TableNextRow();
					ImGui::TableNextColumn();
					ImGui::Text("%d", l);
					ImGui::TableNextColumn();
					ImGui::Text("%d", layer->layer);
					ImGui::TableNextColumn();
					ImGui::Text("%d", layer->subLayer);
					ImGui::TableNextColumn();
					ImGui::Text("%d", layer->numCmd);
					ImGui::TableNextColumn();
					ImGui::Text("%sB", BinaryUnitPrefix(layer->cmdCursor).c_str());
					ImGui::TableNextColumn();
					if (ImGui::Button("Debug")) {
						layerBreak = { layer->layer, layer->subLayer };
					}
					ImGui::PopID();
					ImGui::PopID();
				}
				ImGui::EndTable();
			}
		}
		ImGui::End();
	}

	if (inhibitElision || elideFrames != !!r_elideFrames->intVal) {
		elideFrames = !!r_elideFrames->intVal;
		lastFrameHash.clear();
	}

	std::future<std::optional<std::vector<uint8_t>>> elidedFrameHashFut;
	if (elideFrames) {
		elidedFrameHashFut = std::async([&]() -> std::optional<std::vector<uint8_t>> {
			std::vector<uint8_t> commandDigest;

			for (auto lIdx = 0; lIdx < numLayer; ++lIdx) {
				auto layer = layerSort[lIdx];
				uint64_t subHash = MurmurHash64A(layer->cmdStorage.data(), (int)layer->cmdCursor, 0ull);
				uint8_t const* p = (uint8_t const*)&subHash;
				commandDigest.insert(commandDigest.end(), p, p + sizeof(subHash));
			}

			return commandDigest;
		});
	}
	else {
		std::promise<std::optional<std::vector<uint8_t>>> p;
		elidedFrameHashFut = p.get_future();
		p.set_value({});
	}

	elidedFrameHashFut.wait();

	++totalFrames;
	bool decideDraw = false;
	bool elideDraw = false;
	{
		auto* dev = dx11->device.p;
		auto* ctx = dx11->ctx.p;
		auto* rtv = GetDrawRenderTarget().rtv.p;
		glm::vec4 clearColor(0.0f, 0.0f, 0.0f, 1.0f);
		ctx->ClearRenderTargetView(rtv, glm::value_ptr(clearColor));
		ctx->OMSetRenderTargets(1, &GetDrawRenderTarget().rtv.p, nullptr);
		int l{};
		for (l = 0; l < numLayer; l++) {
			if (!decideDraw && elidedFrameHashFut.wait_for(std::chrono::milliseconds(0)) == std::future_status::ready) {
				decideDraw = true;
				auto commandDigest = elidedFrameHashFut.get();
				if (commandDigest) {
					if (*commandDigest == lastFrameHash) {
						elideDraw = true;
						break;
					}
					else {
						lastFrameHash = *commandDigest;
					}
				}
				else {
					lastFrameHash.clear();
				}
			}
			auto& layer = layerSort[l];
			if (layerBreak && layerBreak->first == layer->layer && layerBreak->second == layer->subLayer) {
#ifdef _WIN32
				DebugBreak();
#endif
			}
			layer->Render();
		}
		if (!elideDraw) {
			presentRtt = 1 - presentRtt;
			++drawnFrames;
		}
	}

	if (!decideDraw) {
		if (auto commandDigest = elidedFrameHashFut.get()) {
			lastFrameHash = *commandDigest;
		}
		else {
			lastFrameHash.clear();
		}
	}

	if (inhibitElision) {
		// If we explicitly inhibited elision due to things like incomplete textures, make sure that the next frame is drawn.
		lastFrameHash.clear();
	}

	for (int l = 0; l < numLayer; ++l) {
		layerSort[l]->Discard();
	}
	delete[] layerSort;

	{
		auto* dev = dx11->device.p;
		auto* ctx = dx11->ctx.p;
		auto& rtt = GetPresentRenderTarget();

		glm::vec4 clearColor{0.0f, 0.0f, 0.0f, 1.0f};
		ctx->ClearRenderTargetView(dx11->swap_rtv.p, glm::value_ptr(clearColor));
		ctx->OMSetRenderTargets(1, &dx11->swap_rtv.p, nullptr);

		float blitTriPos[] = {
			-1.0f, -1.0f, //
			3.0f, -1.0f, //
			-1.0f, 3.0f, //
		};
		float blitTriUV[] = {
			0.0f, 0.0f, //
			2.0f, 0.0f, //
			0.0f, 2.0f, //
		};

		// TODO(zao):
		// Blit current RT to swap-chain

		//glViewport(0, 0, sys->video->vid.fbSize[0], sys->video->vid.fbSize[1]);
		//glUseProgram(rtt.blitProg);
		//glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, std::data(blitTriPos));
		//glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, std::data(blitTriUV));
		//glEnableVertexAttribArray(0);
		//glEnableVertexAttribArray(1);
		//glBindTexture(GL_TEXTURE_2D, rtt.colorTexture);
		//glUniform1i(rtt.blitSampleLocColour, 0);
		//glDrawArrays(GL_TRIANGLES, 0, 3);
		//glBindTexture(GL_TEXTURE_2D, 0);
		//glUseProgram(0);
	}

	if (showHash) {
		if (ImGui::Begin("Hash")) {
			char* b64{};
			size_t b64Len{};
			Base64UrlEncode((char const*)lastFrameHash.data(), lastFrameHash.size(), &b64, &b64Len);
			ImGui::Text("%s", b64);
			free(b64);
		}
		ImGui::End();
	}

	std::chrono::time_point endFrameToc = std::chrono::steady_clock::now();
	frameStats.AppendDuration(&FrameStats::endFrameStepDurations, endFrameToc - endFrameTic);
	
	if (showTiming) {
		if (ImGui::Begin("Timing")) {
			auto stepStatsUi = [&](std::string label, auto& seq) {
				auto [I, J] = std::minmax_element(seq.begin(), seq.end());
				ImGui::LabelText(fmt::format("{} min", label).c_str(), "%2.2f ms", *I * 1'000.0f);
				ImGui::LabelText(fmt::format("{} cur", label).c_str(), "%2.2f ms", seq.back() * 1'000.0f);
				ImGui::LabelText(fmt::format("{} max", label).c_str(), "%2.2f ms", *J * 1'000.0f);
				ImGui::PlotLines(label.c_str(),
					[](void* data, int idx) -> float { auto& dq = *(std::deque<float>*)data; return dq[idx]; },
					&seq, (int)seq.size(), 0, nullptr, 0.0f, 30.0f / 1000.0f);
				};
			stepStatsUi("MidFrame", frameStats.midFrameStepDurations);
			ImGui::Separator();
			stepStatsUi("EndFrame", frameStats.endFrameStepDurations);
		}
		ImGui::End();
	}

	ImGui::Render();
	ImGui_ImplDX11_RenderDrawData(ImGui::GetDrawData());

	// Swap output buffers
	dx11->swap_chain->Present(1, 0);

	// Take screenshot
	switch (takeScreenshot) {
	case R_SSTGA:
	{
		targa_c i(sys->con);
		DoScreenshot(&i, IMGTYPE_RGB, "tga");
	}
	break;
	case R_SSJPEG:
	{
		jpeg_c i(sys->con);
		DoScreenshot(&i, IMGTYPE_RGB, "jpg");
	}
	break;
	case R_SSPNG:
	{
		png_c i(sys->con);
		DoScreenshot(&i, IMGTYPE_RGB, "png");
	}
	break;
	}
	takeScreenshot = R_SSNONE;

	PurgeShaders();
}

// =================
// Shader Management
// =================

std::optional<int> r_shaderHnd_c::StackCount() const
{
	if (!sh || sh->tex->status != r_tex_c::Status::DONE)
		return {};
	return (int)sh->tex->stackLayers;
}

void r_renderer_c::PurgeShaders()
{
	// Delete released shaders
	for (int s = 0; s < numShader; s++) {
		if (shaderList[s] && shaderList[s]->refCount == 0 && shaderList[s]->tex->status == r_tex_c::DONE) {
			delete shaderList[s];
			shaderList[s] = NULL;
		}
	}
}

r_shaderHnd_c* r_renderer_c::RegisterShader(std::string_view shname, int flags)
{
	if (shname.empty()) {
		return NULL;
	}

	std::string name(shname);
	dword nameHash = StringHash(name, 0xFFFF);
	int newId = -1;
	for (int s = 0; s < numShader; s++) {
		if (!shaderList[s]) {
			newId = s;
		}
		else if (shaderList[s]->nameHash == nameHash && _stricmp(name.c_str(), shaderList[s]->name.c_str()) == 0 && shaderList[s]->tex->flags == flags) {
			// Shader already exists, return a new handle for it
			// Ensure texture is loaded as soon as possible
			shaderList[s]->tex->ForceLoad();
			return new r_shaderHnd_c(shaderList[s]);
		}
	}
	if (newId == -1) {
		if (numShader == R_MAXSHADERS) {
			sys->con->Warning("shader limit reached");
			return NULL;
		}
		newId = numShader++;
	}
	shaderList[newId] = new r_shader_c(this, shname, flags);
	return new r_shaderHnd_c(shaderList[newId]);
}

r_shaderHnd_c* r_renderer_c::RegisterShaderFromImage(std::unique_ptr<image_c> img, int flags)
{
	int newId = -1;
	for (int s = 0; s < numShader; s++) {
		if (!shaderList[s]) {
			newId = s;
			break;
		}
	}
	if (newId == -1) {
		if (numShader == R_MAXSHADERS) {
			sys->con->Warning("shader limit reached");
			return NULL;
		}
		newId = numShader++;
	}
	char shname[32];
	sprintf(shname, "data:%d", newId);
	shaderList[newId] = new r_shader_c(this, shname, flags, std::move(img));
	return new r_shaderHnd_c(shaderList[newId]);
}

void r_renderer_c::GetShaderImageSize(r_shaderHnd_c* hnd, int& width, int& height)
{
	if (hnd)
	{
		while (hnd->sh->tex->status < r_tex_c::SIZE_KNOWN) {
			Sleep(1);
		}
		width = hnd->sh->tex->fileWidth;
		height = hnd->sh->tex->fileHeight;
	}
	else {
		width = 0;
		height = 0;
	}
}

void r_renderer_c::SetShaderLoadingPriority(r_shaderHnd_c* hnd, int pri)
{
	if (hnd && hnd->sh->tex->status != r_tex_c::DONE) {
		hnd->sh->tex->loadPri = pri;
	}
}

int r_renderer_c::GetTexAsyncCount()
{
	return texMan->GetAsyncCount();
}

// ==========
// 2D Drawing
// ==========

void r_renderer_c::SetDrawLayer(int layer, int subLayer)
{
	if (layer == curLayer->layer && subLayer == curLayer->subLayer) {
		return;
	}
	r_layer_c* newCurLayer = NULL;
	for (int l = 0; l < numLayer; l++) {
		if (layerList[l]->layer == layer && layerList[l]->subLayer == subLayer) {
			newCurLayer = layerList[l];
			break;
		}
	}
	if (!newCurLayer) {
		if (numLayer == layerListSize) {
			layerListSize <<= 1;
			trealloc(layerList, layerListSize);
		}
		layerList[numLayer] = new r_layer_c(this, layer, subLayer);
		newCurLayer = layerList[numLayer];
		numLayer++;
	}
	curLayer = newCurLayer;
	curLayer->SetViewport(&curViewport);
	curLayer->SetBlendMode(curBlendMode);
}

void r_renderer_c::SetDrawSubLayer(int subLayer)
{
	SetDrawLayer(curLayer->layer, subLayer);
}

int r_renderer_c::GetDrawLayer()
{
	return curLayer->subLayer;
}

void r_renderer_c::SetViewport(int x, int y, int width, int height)
{
	if (height == 0) {
		auto& vid = sys->video->vid;
		width = VirtualScreenWidth();
		height = VirtualScreenHeight();
	}
	curViewport.x = x;
	curViewport.y = y;
	curViewport.width = width;
	curViewport.height = height;
	curLayer->SetViewport(&curViewport);
}

void r_renderer_c::SetBlendMode(int mode)
{
	curBlendMode = mode;
	curLayer->SetBlendMode(mode);
}

void r_renderer_c::DrawColor(const col4_t col)
{
	if (col) {
		Vector4Copy(col, drawColor);
	}
	else {
		drawColor[0] = 1.0f;
		drawColor[1] = 1.0f;
		drawColor[2] = 1.0f;
		drawColor[3] = 1.0f;
	}
}

void r_renderer_c::DrawColor(dword col)
{
	drawColor[0] = ((col >> 16) & 0xFF) / 255.0f;
	drawColor[1] = ((col >> 8) & 0xFF) / 255.0f;
	drawColor[2] = (col & 0xFF) / 255.0f;
	drawColor[3] = (col >> 24) / 255.0f;
}

void r_renderer_c::GetDrawColor(col4_t color)
{
	color[0] = drawColor[0];
	color[1] = drawColor[1];
	color[2] = drawColor[2];
	color[3] = drawColor[3];
}

void r_renderer_c::DrawImage(r_shaderHnd_c* hnd, glm::vec2 pos, glm::vec2 extent, glm::vec2 uv1, glm::vec2 uv2, int stackLayer, std::optional<int> maskLayer)
{
	DrawImageQuad(hnd,
		pos,
		pos + glm::vec2{ extent.x, 0 },
		pos + extent,
		pos + glm::vec2{ 0, extent.y },
		uv1,
		{ uv2.s, uv1.t },
		uv2,
		{ uv1.s, uv2.t },
		stackLayer, maskLayer);
}

void r_renderer_c::DrawImageQuad(r_shaderHnd_c* hnd, glm::vec2 p0, glm::vec2 p1, glm::vec2 p2, glm::vec2 p3, glm::vec2 uv0, glm::vec2 uv1, glm::vec2 uv2, glm::vec2 uv3, int stackLayer, std::optional<int> maskLayer)
{
	if (hnd) {
		curLayer->Bind(hnd->sh->tex);
		stackLayer = clamp(stackLayer, 0, (int)hnd->sh->tex->stackLayers - 1);
	}
	else {
		curLayer->Bind(whiteImage->sh->tex);
		stackLayer = 0;
	}
	curLayer->Color(drawColor);
	curLayer->Quad(
		uv0.s, uv0.t, p0.x, p0.y,
		uv1.s, uv1.t, p1.x, p1.y,
		uv2.s, uv2.t, p2.x, p2.y,
		uv3.s, uv3.t, p3.x, p3.y,
		stackLayer, maskLayer.value_or(-1));
}

void r_renderer_c::DrawString(float x, float y, int align, int height, const col4_t col, int font, const char* str)
{
	auto idxStr = IndexUTF8ToUTF32(str);
	if (font < 0 || font >= F_NUMFONTS) {
		font = F_FIXED;
	}

	scp_t pos = { x, y };
	if (col) {
		col4_t tcol;
		Vector4Copy(col, tcol);
		fonts[font]->Draw(pos, align, height, tcol, idxStr.text);
	}
	else {
		fonts[font]->Draw(pos, align, height, drawColor, idxStr.text);
	}
}

void r_renderer_c::DrawStringFormat(float x, float y, int align, int height, const col4_t col, int font, const char* fmt, ...)
{
	if (font < 0 || font >= F_NUMFONTS) {
		font = F_FIXED;
	}

	va_list va;
	va_start(va, fmt);

	scp_t pos = { x, y };
	if (col) {
		col4_t tcol;
		Vector4Copy(col, tcol);
		fonts[font]->VDraw(pos, align, height, tcol, fmt, va);
	}
	else {
		fonts[font]->VDraw(pos, align, height, drawColor, fmt, va);
	}

	va_end(va);
}

int	r_renderer_c::DrawStringWidth(int height, int font, const char* str)
{
	if (!*str) {
		return 0;
	}
	auto idxStr = IndexUTF8ToUTF32(str);
	if (font < 0 || font >= F_NUMFONTS) {
		font = F_FIXED;
	}
	return fonts[font]->StringWidth(height, idxStr.text);
}

int r_renderer_c::DrawStringCursorIndex(int height, int font, const char* str, int curX, int curY)
{
	if (!*str) {
		return 0;
	}
	std::string_view narrowView(str);
	auto idxStr = IndexUTF8ToUTF32(narrowView);
	if (font < 0 || font >= F_NUMFONTS) {
		font = F_FIXED;
	}
	size_t index = fonts[font]->StringCursorIndex(height, idxStr.text, curX, curY);
	if (index < idxStr.sourceCodeUnitOffsets.size()) {
		return (int)idxStr.sourceCodeUnitOffsets[index];
	}
	return (int)narrowView.size();
}

// ==============
// Virtual screen
// ==============

int r_renderer_c::VirtualScreenWidth() {
	int const properWidth = apiDpiAware ? sys->video->vid.fbSize[0] : sys->video->vid.size[0];
	return VirtualMap(properWidth);
}

int r_renderer_c::VirtualScreenHeight() {
	int const properHeight = apiDpiAware ? sys->video->vid.fbSize[1] : sys->video->vid.size[1];
	return VirtualMap(properHeight);
}

float r_renderer_c::VirtualScreenScaleFactor() {
	if (apiDpiAware) {
		if (dpiScaleOverridePercent > 0) {
			return dpiScaleOverridePercent / 100.0f;
		}
		return sys->video->vid.dpiScale;
	}
	return 1.0f;
}

void r_renderer_c::SetDpiScaleOverridePercent(int percent) {
	dpiScaleOverridePercent = percent;
}

int r_renderer_c::DpiScaleOverridePercent() const {
	return dpiScaleOverridePercent;
}

int r_renderer_c::VirtualMap(int properValue) {
	if (apiDpiAware) {
		return properValue;
	}
	return static_cast<int>(properValue / sys->video->vid.dpiScale);
}

int r_renderer_c::VirtualUnmap(int mappedValue) {
	if (apiDpiAware) {
		return mappedValue;
	}
	return static_cast<int>(mappedValue * sys->video->vid.dpiScale);
}

// =====
// Debug
// =====

void r_renderer_c::ToggleDebugImGui() {
	debugImGui = !debugImGui;
}

// ===========
// Screenshots
// ===========

void r_renderer_c::C_Screenshot(IConsole* conHnd, args_c& args)
{
	const char* fmtName = args.argc >= 2 ? args.argv[1] : r_screenshotFormat->strVal.c_str();
	takeScreenshot = R_SSNONE;
	if (!_stricmp(fmtName, "tga")) {
		takeScreenshot = R_SSTGA;
	}
	else if (!_stricmp(fmtName, "jpg") || !_stricmp(fmtName, "jpeg")) {
		takeScreenshot = R_SSJPEG;
	}
	else if (!_stricmp(fmtName, "png")) {
		takeScreenshot = R_SSPNG;
	}
	else {
		conHnd->Warning("Unknown screenshot format '%s', valid formats: jpg, tga, png", fmtName);
	}
}

void r_renderer_c::DoScreenshot(image_c* i, int type, const char* ext)
{
	if (type != IMGTYPE_RGB) {
		return;
	}
	auto& rt = GetPresentRenderTarget();
	int const xs = rt.width;
	int const ys = rt.height;

	// Pixel reading only supports RGBA and an implementation-specific format.
	// Use RGBA for convenience as that's close enough to what we want to save in the end.
	int const readSize = xs * ys * 4;
	int const writeSize = xs * ys * 3;
	std::vector<byte> sbuf(readSize);

	CComPtr<ID3D11Texture2D> stageTex;
	D3D11_TEXTURE2D_DESC stageDesc{};
	stageDesc.Width = rt.width;
	stageDesc.Height = rt.height;
	stageDesc.MipLevels = 1;
	stageDesc.ArraySize = 1;
	stageDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
	stageDesc.SampleDesc = {1, 0};
	stageDesc.Usage = D3D11_USAGE_STAGING;
	stageDesc.BindFlags = 0;
	stageDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
	stageDesc.MiscFlags = 0;
	dx11->device->CreateTexture2D(&stageDesc, nullptr, &stageTex);

	// Read the front buffer
	dx11->ctx->CopyResource(stageTex.p, rt.colorTexture.p);

	// Flip and convert the image to RGB
	int const readSpan = xs * 4;
	int	const writeSpan = xs * 3;
	std::vector<byte> ss(writeSize);
	byte* p1 = sbuf.data();
	byte* p2 = ss.data();
	for (int y = 0; y < ys; ++y) {
		for (int x = 0; x < xs; ++x) {
			*p2++ = *p1++; // R
			*p2++ = *p1++; // G
			*p2++ = *p1++; // B
			p1++; // A
		}
	}
	sbuf.clear();

	// Set image info
	i->CopyRaw(IMGTYPE_RGB, xs, ys, ss.data());
	ss.clear();

	time_t curTime;
	time(&curTime);
	auto ssPath = std::filesystem::u8path(fmt::format(CFG_DATAPATH "Screenshots/{:%m%d%y_%H%M%S}.{}",
		*std::localtime(&curTime), ext));

	// Make folder if it doesn't exist
	std::error_code ec;
	std::filesystem::create_directories(ssPath.parent_path(), ec);
	if (ec) {
		sys->con->Print("Couldn't create screenshot folder!\n");
		return;
	}

	if (i->Save(ssPath)) {
		sys->con->Print("Couldn't write screenshot!\n");
		return;
	}
	sys->con->Print(fmt::format("Wrote screenshot to {}\n", ssPath.generic_u8string()).c_str());
}

r_renderer_c::RenderTarget& r_renderer_c::GetDrawRenderTarget()
{
	return rttMain[1 - presentRtt];
}

r_renderer_c::RenderTarget& r_renderer_c::GetPresentRenderTarget()
{
	return rttMain[presentRtt];
}

// ===========================================================
// MurmurHash implementation from public domain, obtained from
// https://github.com/explosion/murmurhash/blob/9281c4825c24e64476457db89fb1d39bf09b3d23/murmurhash/MurmurHash2.cpp
// ===========================================================

#if _WIN32
#define BIG_CONSTANT(x) (x)
#else
#define BIG_CONSTANT(x) (x##LLU)
#endif

static inline uint64_t MurmurHashGetBlock(const uint64_t* p)
{
#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
	return *p;
#else
	const uint8_t* c = (const uint8_t*)p;
	return (uint64_t)c[0] |
		(uint64_t)c[1] << 8 |
		(uint64_t)c[2] << 16 |
		(uint64_t)c[3] << 24 |
		(uint64_t)c[4] << 32 |
		(uint64_t)c[5] << 40 |
		(uint64_t)c[6] << 48 |
		(uint64_t)c[7] << 56;
#endif
}

uint64_t MurmurHash64A(const void* key, int len, uint64_t seed)
{
	const uint64_t m = BIG_CONSTANT(0xc6a4a7935bd1e995);
	const int r = 47;

	uint64_t h = seed ^ (len * m);

	const uint64_t* data = (const uint64_t*)key;
	const uint64_t* end = data + (len / 8);

	while (data != end)
	{
		uint64_t k = MurmurHashGetBlock(data++);

		k *= m;
		k ^= k >> r;
		k *= m;

		h ^= k;
		h *= m;
	}

	const unsigned char* data2 = (const unsigned char*)data;

	switch (len & 7)
	{
	case 7: h ^= uint64_t(data2[6]) << 48;
	case 6: h ^= uint64_t(data2[5]) << 40;
	case 5: h ^= uint64_t(data2[4]) << 32;
	case 4: h ^= uint64_t(data2[3]) << 24;
	case 3: h ^= uint64_t(data2[2]) << 16;
	case 2: h ^= uint64_t(data2[1]) << 8;
	case 1: h ^= uint64_t(data2[0]);
		h *= m;
	};

	h ^= h >> r;
	h *= m;
	h ^= h >> r;

	return h;
}

r_dx11_c::r_dx11_c(sys_IMain* sys)
	: sys(sys)
{
	HRESULT hr = S_OK;
	const UINT device_flags = 0u;
	const std::array<D3D_FEATURE_LEVEL, 2> feature_levels{
		D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0
	};

	{
		auto& video = sys->video;
		auto& bd = scd.BufferDesc;
		bd.Width = (UINT)video->vid.fbSize[0];
		bd.Height = (UINT)video->vid.fbSize[1];
		bd.Format = DXGI_FORMAT_R8G8B8A8_UNORM;

		auto& sd = scd.SampleDesc;
		sd.Count = 1;

		scd.BufferUsage = DXGI_USAGE_BACK_BUFFER | DXGI_USAGE_RENDER_TARGET_OUTPUT;
		scd.BufferCount = 2;
		scd.OutputWindow = (HWND)video->GetNativeWindowHandle();
		scd.Windowed = TRUE;
		scd.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
	};

	auto TryCreate = [&](D3D_DRIVER_TYPE driver_type) -> HRESULT
		{
			return D3D11CreateDeviceAndSwapChain(nullptr, driver_type, nullptr, device_flags, feature_levels.data(), feature_levels.size(), D3D11_SDK_VERSION, &scd, &swap_chain, &device, &feature_level, &ctx);
		};
	hr = TryCreate(D3D_DRIVER_TYPE_HARDWARE);
	if (!SUCCEEDED(hr)) {
		hr = TryCreate(D3D_DRIVER_TYPE_WARP);
		if (!SUCCEEDED(hr)) {
			throw std::runtime_error(NarrowUTF8StringStd(_com_error(hr).ErrorMessage()));
		}
	}

	hr = ctx.QueryInterface(&annotation);

	ResizeIfNeeded({});
}

void r_dx11_c::ResizeIfNeeded(glm::ivec2 size)
{
	if (scd.BufferDesc.Width != size.x || scd.BufferDesc.Height != size.y) {
		swap_rtv.Release();
		swap_chain->ResizeBuffers(0, 0, 0, DXGI_FORMAT_UNKNOWN, scd.Flags);
		CComPtr<ID3D11Resource> back_buffer;
		swap_chain->GetBuffer(0, IID_PPV_ARGS(&back_buffer));
		device->CreateRenderTargetView(back_buffer.p, nullptr, &swap_rtv);
	}
}

CComPtr<ID3D11SamplerState> r_renderer_c::SamplerStateCache::MakeState(r_renderer_c::SamplerStateCache::Parameters params)
{
	auto I = samplerStates.find(params);
	if (I == samplerStates.end())
	{
		D3D11_SAMPLER_DESC desc{};
		desc.Filter = params.Filter;
		desc.AddressU = params.AddressU;
		desc.AddressV = params.AddressV;
		desc.AddressW = D3D11_TEXTURE_ADDRESS_CLAMP;
		desc.MipLODBias = 0.0f;
		desc.MaxAnisotropy = params.MaxAnisotropy;
		desc.ComparisonFunc = D3D11_COMPARISON_NEVER;
		std::fill_n(desc.BorderColor, 4, 0.0f);
		desc.MinLOD = std::numeric_limits<float>::lowest();
		desc.MaxLOD = std::numeric_limits<float>::max();

		CComPtr<ID3D11SamplerState> state;
		HRESULT hr = device->CreateSamplerState(&desc, &state);
		if (!hr)
			return {};
		I = samplerStates.emplace(std::move(params), std::move(state)).first;
	}
	return I->second;
}

bool r_renderer_c::SamplerStateCache::Parameters::operator<(const Parameters& rhs) const noexcept
{
	if (Filter != rhs.Filter) return Filter < rhs.Filter;
	if (AddressU != rhs.AddressU) return AddressU < rhs.AddressU;
	if (AddressV != rhs.AddressV) return AddressV < rhs.AddressV;
	return MaxAnisotropy < rhs.MaxAnisotropy;
}
