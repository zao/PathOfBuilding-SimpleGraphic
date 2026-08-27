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
#include <map>
#include <numeric>
#include <random>
#include <ranges>
#include <sstream>
#include <vector>

#include <imgui_stdlib.h>

#include "r_api_angle.h"
#include "r_api_dx11.h"

#include <xxh3.h>

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
	std::u8string name;
	uint64_t nameHash;
	std::shared_ptr<r_tex_c> tex;

	r_shader_c(r_renderer_c* renderer, std::u8string_view shname, int flags);
	r_shader_c(r_renderer_c* renderer, std::u8string_view shname, int flags, std::unique_ptr<image_c> img);
};

r_shader_c::r_shader_c(r_renderer_c* renderer, std::u8string_view shname, int flags)
	: renderer(renderer)
{
	name = shname;
	nameHash = StringHash(name);
	tex = r_tex_c::CreateFromPath(renderer->texMan.get(), name, flags);
	if (tex->error) {
		renderer->sys->con->Warning(fmt::format(u8"couldn't load texture '{}'", name));
	}
}

r_shader_c::r_shader_c(r_renderer_c* renderer, std::u8string_view shname, int flags, std::unique_ptr<image_c> img)
	: renderer(renderer)
{
	name = shname;
	nameHash = StringHash(name);
	tex = r_tex_c::CreateFromImage(renderer->texMan.get(), std::move(img), flags);
}

// ===================
// Shader Handle Class
// ===================

r_shaderHnd_c::r_shaderHnd_c(std::shared_ptr<r_shader_c>&& sh)
	: sh(sh)
{
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

#pragma pack(push, r_layerCmd, 1)
struct r_layerCmd_s {
	enum Command : uint8_t {
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
#pragma pack(pop, r_layerCmd)

r_layer_c::r_layer_c(r_renderer_c* renderer, r_layerId_s id)
	: renderer(renderer), id(id)
{
	cmdStorage.resize(1ull << 23);
	cmdCursor = 0;
	numCmd = 0;
}

r_layer_c::r_layer_c(r_renderer_c* renderer, int layer, int subLayer)
	: r_layer_c(renderer, r_layerId_s{layer, subLayer})
{}

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

void r_layer_c::Bind(const std::shared_ptr<r_tex_c>& tex)
{
	if (auto* cmd = (r_layerCmdBind_s*)NewCommand(CommandSize(r_layerCmd_s::BIND))) {
		cmd->cmd = r_layerCmd_s::BIND;
		cmd->tex = tex.get();
		if (!referencedTextures.count(tex))
			referencedTextures.emplace(tex);
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
	explicit Batch(GLuint prog);
	Batch(Batch&& rhs);
	Batch& operator = (Batch&& rhs);
	Batch(Batch const&) = delete;
	Batch& operator = (Batch const&) = delete;
	~Batch();

	GLuint prog;
	GLint xyAttr;
	GLint uvAttr;
	GLint tintAttr;
	GLint viewportAttr;
	GLint texIdAttr;

	std::vector<Vertex> vertices;

	void Execute(GLuint sharedVbo, size_t vertexBase);
};

Batch::Batch(GLuint prog)
	: prog(prog)
{
	xyAttr = glGetAttribLocation(prog, "a_vertex");
	uvAttr = glGetAttribLocation(prog, "a_texcoord");
	tintAttr = glGetAttribLocation(prog, "a_tint");
	viewportAttr = glGetAttribLocation(prog, "a_viewport");
	texIdAttr = glGetAttribLocation(prog, "a_texId");
}

Batch::Batch(Batch&& rhs)
	: prog(rhs.prog)
	, xyAttr(rhs.xyAttr)
	, uvAttr(rhs.uvAttr)
	, tintAttr(rhs.tintAttr)
	, viewportAttr(rhs.viewportAttr)
	, texIdAttr(rhs.texIdAttr)
	, vertices(std::move(rhs.vertices))
{
}

Batch& Batch::operator = (Batch&& rhs) {
	prog = rhs.prog;
	xyAttr = rhs.xyAttr;
	uvAttr = rhs.uvAttr;
	tintAttr = rhs.tintAttr;
	viewportAttr = rhs.viewportAttr;
	texIdAttr = rhs.texIdAttr;
	vertices = std::move(rhs.vertices);

	return *this;
}

Batch::~Batch() {}

void Batch::Execute(GLuint sharedVbo, size_t vertexBase)
{
	if (vertices.empty()) {
		return;
	}

	glBindBuffer(GL_ARRAY_BUFFER, sharedVbo);
	auto dataPtr = (uint8_t const*)vertices.data();
	auto dataOff = vertexBase * sizeof(Vertex);
	auto dataSize = vertices.size() * sizeof(Vertex);
	glBufferSubData(GL_ARRAY_BUFFER, dataOff, dataSize, dataPtr);
	glVertexAttribPointer(xyAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, x));
	glVertexAttribPointer(uvAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, u));
	glVertexAttribPointer(tintAttr, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, r));
	glVertexAttribPointer(viewportAttr, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, viewX));
	glVertexAttribPointer(texIdAttr, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, texId));
	glEnableVertexAttribArray(xyAttr);
	glEnableVertexAttribArray(uvAttr);
	glEnableVertexAttribArray(tintAttr);
	glEnableVertexAttribArray(viewportAttr);
	glEnableVertexAttribArray(texIdAttr);
	glDrawArrays(GL_TRIANGLES, 0, (GLsizei)vertices.size());
	glDisableVertexAttribArray(xyAttr);
	glDisableVertexAttribArray(uvAttr);
	glDisableVertexAttribArray(tintAttr);
	glDisableVertexAttribArray(viewportAttr);
	glDisableVertexAttribArray(texIdAttr);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	vertices.clear();
}

struct RenderStrategy {
	virtual ~RenderStrategy() = default;

	virtual void ProcessCommand(r_layerCmd_s* cmd) = 0;
	virtual void Flush() = 0;
	virtual void SetShowStats(bool showStats) { showStats_ = showStats; }
	virtual bool UsedIncompleteTextures() const { return false; }

protected:
	bool showStats_{};
};

static std::map<r_blendMode_e, char const*> const s_blendModeString{
	{RB_ALPHA, "RB_ALPHA"},
	{RB_PRE_ALPHA, "RB_PRE_ALPHA"},
	{RB_ADDITIVE, "RB_ADDITIVE"},
};

struct AdjacentMergeStrategy : RenderStrategy {
	AdjacentMergeStrategy(r_layer_c* layer, r_renderer_c* renderer, GLuint prog)
		: layer_(layer), renderer_(renderer), prog_(prog), batch_(prog)
	{
		for (size_t i = 0;; ++i) {
			GLint loc = glGetUniformLocation(prog, fmt::format("s_tex[{}]", i).c_str());
			if (loc == -1) {
				break;
			}
			texLocs_.push_back(loc);
		}
		mvpMatrixLoc_ = glGetUniformLocation(prog_, "mvp_matrix");
		batchTextureCap_ = texLocs_.size();
		glGenBuffers(1, &vbo_);
	}

	~AdjacentMergeStrategy() {
		glDeleteBuffers(1, &vbo_);
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
			ImGui::BulletText("Layer %d:%d - %d batches", layer_->id.layer, layer_->id.subLayer, batchIndex);
		}
	}

	bool UsedIncompleteTextures() const override { return usedIncompleteTextures; };

private:
	void Dispatch() {
		glBindBuffer(GL_ARRAY_BUFFER, vbo_);
		auto& batch = batch_.batch;
		auto& textures = batch_.textures;
		size_t vertexCount = batch.vertices.size();
		glBufferData(GL_ARRAY_BUFFER, vertexCount * sizeof(Vertex), nullptr, GL_STREAM_DRAW);
		glUseProgram(prog_);

		auto& key = batch_.key;
		auto& lastKey = lastDispatchKey_;

		if (showStats_) {
			ImGui::Text("Batch %d", batchIndex);
			ImGui::Text("%d verts", batch.vertices.size());
		}

		{
			auto& vid = renderer_->sys->video->vid;
			float fbScaleX = vid.fbSize[0] / (float)vid.size[0];
			float fbScaleY = vid.fbSize[1] / (float)vid.size[1];
			int virtualW = renderer_->VirtualScreenWidth();
			int virtualH = renderer_->VirtualScreenHeight();
			glViewport(0, 0, virtualW, virtualH);
			Mat4 mvpMatrix = OrthoMatrix(0, virtualW, virtualH, 0, -9999, 9999);
			glUniformMatrix4fv(mvpMatrixLoc_, 1, GL_FALSE, mvpMatrix.data());
		}
		if (!lastKey || lastKey->blendMode != key.blendMode) {
			if (showStats_) {
				ImGui::Text("New blend mode %s", s_blendModeString.at((r_blendMode_e)key.blendMode));
			}
			switch (key.blendMode) {
			case RB_ALPHA:
				glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
				break;
			case RB_PRE_ALPHA:
				glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
				break;
			case RB_ADDITIVE:
				glBlendFunc(GL_ONE, GL_ONE);
				break;
			}
		}
		{
			for (size_t i = 0, numTex = texLocs_.size(); i < numTex; ++i) {
				glUniform1i(texLocs_[i], (GLint)i);
				glActiveTexture((GLenum)(GL_TEXTURE0 + i));
				if (i < textures.size()) {
					auto tex = textures[i];
					tex->Bind();
					if (showStats_) {
						ImGui::Text("New tex %llX (% s) % d", tex->dataGL ? (uintptr_t)tex->dataGL->texId : (uintptr_t)tex->dataDX->srv.p, tex->fileName.c_str(), tex->status.load());
					}
					if (!usedIncompleteTextures && tex->status != r_tex_c::Status::DONE) {
						usedIncompleteTextures = true;
					}
				}
				else {
					glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
				}
			}
			glActiveTexture(GL_TEXTURE0);
		}

		batch.Execute(vbo_, 0);

		lastDispatchKey_ = key;
		batch_.batch.vertices.clear();
		batch_.textures.clear();

		glUseProgram(0);

		batchIndex += 1;
	}

	r_layer_c* layer_{};
	r_renderer_c* renderer_{};
	GLuint prog_{};
	std::vector<GLint> texLocs_;
	GLint mvpMatrixLoc_{};

	size_t batchTextureCap_{};
	GLuint vbo_{};

	struct TexturedBatch {
		explicit TexturedBatch(GLuint prog) : batch(prog) {
			textures.reserve(128);
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

	bool usedIncompleteTextures = false;
};

bool r_layer_c::Render()
{
	if (!renderer->stateGL)
		return false;

	int const optLevel = renderer->r_layerOptimize->intVal;
	bool const shuffle = renderer->r_layerShuffle->intVal == 1;

	std::unique_ptr<RenderStrategy> strat(new AdjacentMergeStrategy(this, renderer, renderer->stateGL->tintedTextureProgram));

	if (glPushGroupMarkerEXT)
	{
		std::ostringstream oss;
		oss << "Layer " << id.layer << ", sub-layer " << id.subLayer;
		glPushGroupMarkerEXT(0, oss.str().c_str());
	}

	if (strat) {
		bool showStats{};
		if (renderer->debugLayers) {
			if (ImGui::Begin("Layers", &renderer->debugLayers)) {
				std::string heading = fmt::format("Layer {}:{}", id.layer, id.subLayer);
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

	if (glPopGroupMarkerEXT) {
		glPopGroupMarkerEXT();
	}

	return strat->UsedIncompleteTextures();
}

void r_layer_c::Discard()
{
	cmdCursor = 0;
	numCmd = 0;
	referencedTextures.clear();
}

// =====================
// r_IRenderer Interface
// =====================

InterfacePtr<r_IRenderer> r_IRenderer::GetHandle(sys_IMain* sysHnd)
{
	return std::make_unique<r_renderer_c>(sysHnd);
}

r_renderer_c::r_renderer_c(sys_IMain* sysHnd)
	: conCmdHandler_c(sysHnd->con.get()), sys(sysHnd)
{
	r_compress = sys->con->Cvar_Add(u8"r_compress", CV_ARCHIVE, u8"0");
	r_screenshotFormat = sys->con->Cvar_Add(u8"r_screenshotFormat", CV_ARCHIVE, u8"jpg");
	r_layerDebug = sys->con->Cvar_Add(u8"r_layerDebug", CV_ARCHIVE, u8"0");
	r_layerOptimize = sys->con->Cvar_Add(u8"r_layerOptimize", CV_ARCHIVE | CV_CLAMP, u8"1", 0, 1);
	r_layerShuffle = sys->con->Cvar_Add(u8"r_layerShuffle", CV_ARCHIVE | CV_CLAMP, u8"0", 0, 1);
	r_elideFrames = sys->con->Cvar_Add(u8"r_elideFrames", CV_ARCHIVE | CV_CLAMP, u8"1", 0, 1);
	r_drawCull = sys->con->Cvar_Add(u8"r_drawCull", CV_ARCHIVE | CV_CLAMP, u8"1", 0, 1);

	Cmd_Add(u8"screenshot", 0, u8"[<format>]", this, &r_renderer_c::C_Screenshot);
}

// =============
// Init/Shutdown
// =============

void r_renderer_c::Init(r_featureFlag_e features)
{
	sys->con->PrintFunc(u8"Render Init");

	apiDpiAware = !!(features & F_DPI_AWARE);

	timer_c timer;
	timer.Start();

	if (sys->video->vid.api == sys_vidApi_e::ANGLE) {
		api = stateGL = std::make_shared<r_stateGL_s>(this);
	}
	else {
		api = stateDX = std::make_shared<r_stateDX_s>(this);
	}

	// Initialise texture manager
	texMan = r_ITexManager::GetHandle(this);
	imguiCtx = ImGui::CreateContext();
	ImGui::SetCurrentContext(imguiCtx);

	api->Init();

	// Initialise layer array
	layerList[{0, 0}] = std::make_shared<r_layer_c>(this, 0, 0);

	takeScreenshot = R_SSNONE;

	// Load render resources
	sys->con->Print(u8"Loading resources...\n");

	whiteImage = RegisterShader(u8"@white", 0);
	blackImage = RegisterShader(u8"@black", 0);

	fonts[F_FIXED] = new r_font_c(this, u8"Bitstream Vera Sans Mono");
	fonts[F_VAR] = new r_font_c(this, u8"Liberation Sans");
	fonts[F_VAR_BOLD] = new r_font_c(this, u8"Liberation Sans Bold");
	fonts[F_FONTIN_SC] = new r_font_c(this, u8"Fontin SmallCaps");
	fonts[F_FONTIN_SC_ITALIC] = new r_font_c(this, u8"Fontin SmallCaps Italic");
	fonts[F_FONTIN] = new r_font_c(this, u8"Fontin");
	fonts[F_FONTIN_ITALIC] = new r_font_c(this, u8"Fontin Italic");

	sys->con->Print(fmt::format(u8"Renderer initialised in {} msec.\n", timer.Get()));
}

void r_renderer_c::Shutdown()
{
	sys->con->PrintFunc(u8"Render Shutdown");

	sys->con->Print(u8"Unloading resources...\n");

	delete whiteImage;
	delete blackImage;

	for (int f = 0; f < F_NUMFONTS; f++) {
		delete fonts[f];
	}

	shaderList.clear();

	curLayer.reset();
	layerList.clear();

	// Shutdown texture manager
	texMan.reset();

	api->Shutdown();
	ImGui::DestroyContext(imguiCtx);
	api.reset();
	stateGL.reset();
	stateDX.reset();

	sys->con->Print(u8"Renderer shutdown complete.\n");
}

// =================
// Render Management
// =================

void r_renderer_c::PumpShaders()
{
	texMan->ProcessPendingTextureUploads();
}

void r_renderer_c::BeginFrame()
{
	api->ImGuiBeginFrame();
	ImGui::NewFrame();
	api->BeginFrame();

	assert(layerList.size());
	curLayer = layerList.begin()->second;

	SetViewport();
	SetBlendMode(RB_ALPHA);
	DrawColor();

	beginFrameToc = std::chrono::steady_clock::now();
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

	auto layerSort = std::views::transform(layerList, [](auto& kv) { return kv.second.get(); }) | std::ranges::to<std::vector>();
	if (r_layerDebug->intVal) {
		size_t totalCmd = 0;
		for (const auto& [layerIdx, layer] : layerSort | std::views::enumerate) {
			totalCmd += layer->numCmd;
			char str[1024];
			sprintf(str, "%zu (%4d,%4d) [%2d]", layer->numCmd, layer->id.layer, layer->id.subLayer, (int)layerIdx);
			float w = (float)DrawStringWidth(16, F_FIXED, str);
			DrawColor(0x7F000000);
			DrawImage(NULL, { (float)VirtualScreenWidth() - w, VirtualScreenHeight() - (layerIdx + 2) * 16.0f }, { w, 16 });
			DrawStringFormat(0, VirtualScreenHeight() - (layerIdx + 2) * 16.0f, F_RIGHT, 16, colorWhite, F_FIXED, str);
		}
		char str[1024];
		sprintf(str, "%zu", totalCmd);
		float w = (float)DrawStringWidth(16, F_FIXED, str);
		DrawColor(0xAF000000);
		DrawImage(NULL, { (float)VirtualScreenWidth() - w, VirtualScreenHeight() - 16.0f }, { w, 16 });
		DrawStringFormat(0, VirtualScreenHeight() - 16.0f, F_RIGHT, 16, colorWhite, F_FIXED, str);
	}

	std::optional<r_layerId_s> layerBreak;
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

			size_t totalHistoricalFootprint{}, totalDenseFootprint{};
			for (auto& layer : layerSort) {
				size_t byteAcc{};
				size_t const numCmd = layer->numCmd;
				totalHistoricalFootprint += numCmd * sizeof(r_layerCmdQuad_s); // legacy footprint with uniform union commands
				totalDenseFootprint += layer->cmdCursor;
			}

			ImGui::Text("Total historical footprint: %sB", BinaryUnitPrefix(totalHistoricalFootprint).c_str());
			ImGui::Text("Total dense footprint: %sB", BinaryUnitPrefix(totalDenseFootprint).c_str());

			if (ImGui::BeginTable("Layer stats", 6, ImGuiTableFlags_Borders | ImGuiTableFlags_SizingFixedFit)) {
				ImGui::TableSetupColumn("Index");
				ImGui::TableSetupColumn("Layer");
				ImGui::TableSetupColumn("Sublayer");
				ImGui::TableSetupColumn("Command count");
				ImGui::TableSetupColumn("Dense");
				ImGui::TableSetupColumn("Debug");
				ImGui::TableHeadersRow();
				for (const auto& [layerIdx, layer] : layerSort | std::views::enumerate) {
					ImGui::PushID(layer->id.layer);
					ImGui::PushID(layer->id.subLayer);

					ImGui::TableNextRow();
					ImGui::TableNextColumn();
					ImGui::Text("%d", layerIdx);
					ImGui::TableNextColumn();
					ImGui::Text("%d", layer->id.layer);
					ImGui::TableNextColumn();
					ImGui::Text("%d", layer->id.subLayer);
					ImGui::TableNextColumn();
					ImGui::Text("%d", layer->numCmd);
					ImGui::TableNextColumn();
					ImGui::Text("%sB", BinaryUnitPrefix(layer->cmdCursor).c_str());
					ImGui::TableNextColumn();
					if (ImGui::Button("Debug")) {
						layerBreak = { layer->id.layer, layer->id.subLayer };
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
		lastFrameHash = 0;
	}

	auto tic = std::chrono::high_resolution_clock::now();

	uint64_t commandDigest = 0;
	if (elideFrames) {
		std::shared_ptr<XXH3_state_t> hashState(XXH3_createState(), XXH3_freeState);
		XXH3_64bits_reset(hashState.get());

		for (auto& layer : layerSort) {
			uint64_t subHash = XXH3_64bits(layer->cmdStorage.data(), (int)layer->cmdCursor);
			XXH3_64bits_update(hashState.get(), &subHash, sizeof(subHash));
		}

		commandDigest = XXH3_64bits_digest(hashState.get());
	}

	++totalFrames;
	const bool elideDraw = lastFrameHash != 0 && lastFrameHash == commandDigest;
	if (!elideDraw)
	{
		api->PrepareDrawTarget();
		for (auto& layer : layerSort) {
			if (layerBreak && *layerBreak == layer->id) {
#ifdef _WIN32
				DebugBreak();
#endif
			}
			inhibitElision = layer->Render() || inhibitElision;
		}
		presentRtt = 1 - presentRtt;
		++drawnFrames;
	}

	// If we explicitly inhibited elision due to things like incomplete textures, make sure that the next frame is drawn.
	lastFrameHash = inhibitElision ? 0 : commandDigest;

	for (auto& layer : layerSort) {
		layer->Discard();
	}
	layerSort.clear();

	api->DrawPresentTarget();

	if (showHash) {
		if (ImGui::Begin("Hash")) {
			char* b64{};
			size_t b64Len{};
			Base64UrlEncode((char const*)&lastFrameHash, sizeof(lastFrameHash), &b64, &b64Len);
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
	api->ImGuiEndFrame();
	api->EndFrame();

	// Swap output buffers
	if (stateGL)
		stateGL->openGL->Swap();

	// Take screenshot
	switch (takeScreenshot) {
	case R_SSTGA:
	{
		targa_c i(sys->con.get());
		DoScreenshot(&i, IMGTYPE_RGB, u8"tga");
	}
	break;
	case R_SSJPEG:
	{
		jpeg_c i(sys->con.get());
		DoScreenshot(&i, IMGTYPE_RGB, u8"jpg");
	}
	break;
	case R_SSPNG:
	{
		png_c i(sys->con.get());
		DoScreenshot(&i, IMGTYPE_RGB, u8"png");
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
	if (!sh || sh->tex->GetStatus() != r_tex_c::Status::DONE)
		return {};
	return (int)sh->tex->stackLayers;
}

void r_renderer_c::PurgeShaders()
{
	// Delete released shaders
	shaderList.erase(std::remove_if(shaderList.begin(), shaderList.end(), [](const std::weak_ptr<r_shader_c>& entry) { return entry.expired(); }), shaderList.end());
}

r_shaderHnd_c* r_renderer_c::RegisterShader(std::u8string_view shname, int flags)
{
	if (shname.empty()) {
		return NULL;
	}

	std::u8string name(shname);
	PERFORMANCEAPI_INSTRUMENT_FUNCTION_DATA((const char*)name.c_str());
	const auto nameHash = StringHash(name);
	int newId = -1;
	auto found = std::find_if(shaderList.begin(), shaderList.end(), [&name, &nameHash, &flags](const auto& entry) {
		if (std::shared_ptr<r_shader_c> sp = entry.lock())
			return sp->nameHash == nameHash && sp->name == name && sp->tex->flags == flags;
		return false;
	});
	std::shared_ptr<r_shader_c> sp;
	if (found != shaderList.end()) {
		// Shader already exists, return a new handle for it
		sp = found->lock();
	}
	else {
		sp = std::make_shared<r_shader_c>(this, shname, flags);
		shaderList.push_back(sp);
	}
	return new r_shaderHnd_c(std::move(sp));
}

r_shaderHnd_c* r_renderer_c::RegisterShaderFromImage(std::unique_ptr<image_c> img, int flags)
{
	const auto shname = fmt::format(u8"data:%d", shaderList.size());
	std::shared_ptr sp = std::make_shared<r_shader_c>(this, shname, flags, std::move(img));
	shaderList.push_back(sp);
	return new r_shaderHnd_c(std::move(sp));
}

void r_renderer_c::GetShaderImageSize(r_shaderHnd_c* hnd, int& width, int& height)
{
	if (hnd && hnd->sh)
	{
		PERFORMANCEAPI_INSTRUMENT_FUNCTION_DATA(hnd->sh->name.size() ? (const char*)hnd->sh->name.c_str() : "<nameless>");
		auto& tex = *hnd->sh->tex;
		tex.WaitOnStatusAtLeast(r_tex_c::SIZE_KNOWN);
		width = tex.fileWidth;
		height = tex.fileHeight;
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

void r_renderer_c::SetClearColor(const col4_t col)
{
	glClearColor(col[0], col[1], col[2], col[3]);
}

void r_renderer_c::SetDrawLayer(int layer, int subLayer)
{
	r_layerId_s id{layer, subLayer};
	if (layer == curLayer->id.layer && subLayer == curLayer->id.subLayer) {
		return;
	}
	auto it = layerList.find(id);
	if (it == layerList.end()) {
		it = layerList.emplace(id, std::make_shared<r_layer_c>(this, layer, subLayer)).first;
	}
	curLayer = it->second;
	curLayer->SetViewport(&curViewport);
	curLayer->SetBlendMode(curBlendMode);
}

void r_renderer_c::SetDrawSubLayer(int subLayer)
{
	SetDrawLayer(curLayer->id.layer, subLayer);
}

int r_renderer_c::GetDrawLayer()
{
	return curLayer->id.subLayer;
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

void r_renderer_c::DrawString(float x, float y, int align, int height, const col4_t col, int font, std::string_view str)
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
	const auto fmtName = args.argc >= 2 ? std::u8string_view(args.argv[1]) : std::u8string_view(r_screenshotFormat->strVal);
	takeScreenshot = R_SSNONE;
	if (fmtName == u8"tga") {
		takeScreenshot = R_SSTGA;
	}
	else if (fmtName == u8"jpg" || fmtName == u8"jpeg") {
		takeScreenshot = R_SSJPEG;
	}
	else if (fmtName == u8"png") {
		takeScreenshot = R_SSPNG;
	}
	else {
		conHnd->Warning(fmt::format(u8"Unknown screenshot format '{}', valid formats: jpg, tga, png", fmtName));
	}
}

void r_renderer_c::DoScreenshot(image_c* i, int type, std::u8string_view ext)
{
	if (type != IMGTYPE_RGB) {
		return;
	}

	if (!stateGL)
		return;

	auto& rt = stateGL->rttMain[GetPresentRenderTarget()];
	int const xs = rt.width;
	int const ys = rt.height;

	// Pixel reading only supports RGBA and an implementation-specific format.
	// Use RGBA for convenience as that's close enough to what we want to save in the end.
	int const readSize = xs * ys * 4;
	int const writeSize = xs * ys * 3;
	std::vector<byte> sbuf(readSize);

	// Read the front buffer
	GLint oldFb{};
	GLenum oglErr = glGetError();
	GLenum implColorReadFormat{}, implColorReadType{};
	glGetIntegerv(GL_FRAMEBUFFER_BINDING, &oldFb);
	glBindFramebuffer(GL_FRAMEBUFFER, rt.framebuffer);
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glReadPixels(0, 0, xs, ys, GL_RGBA, GL_UNSIGNED_BYTE, sbuf.data());
	oglErr = glGetError();
	glBindFramebuffer(GL_FRAMEBUFFER, oldFb);

	// Flip and convert the image to RGB
	int const readSpan = xs * 4;
	int	const writeSpan = xs * 3;
	std::vector<byte> ss(writeSize);
	byte* p1 = sbuf.data();
	byte* p2 = ss.data() + writeSize - writeSpan;
	for (int y = 0; y < ys; ++y, p2 -= writeSpan * 2) {
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
	const auto ssPath = std::filesystem::u8path(fmt::format(CFG_DATAPATH "Screenshots/{:%m%d%y_%H%M%S}.{}",
		*std::localtime(&curTime), ext));

	// Make folder if it doesn't exist
	std::error_code ec;
	std::filesystem::create_directories(ssPath.parent_path(), ec);
	if (ec) {
		sys->con->Print(u8"Couldn't create screenshot folder!\n");
		return;
	}

	if (i->Save(ssPath)) {
		sys->con->Print(u8"Couldn't write screenshot!\n");
		return;
	}
	sys->con->Print(fmt::format(u8"Wrote screenshot to {}\n", ssPath.generic_u8string()));
}

size_t r_renderer_c::GetDrawRenderTarget()
{
	return 1 - presentRtt;
}

size_t r_renderer_c::GetPresentRenderTarget()
{
	return presentRtt;
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
