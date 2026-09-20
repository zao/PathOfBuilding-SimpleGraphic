// SimpleGraphic Engine
// (c) David Gowor, 2014
//
// Module: Render Main
//

//#define NOOPTIMISE
#ifdef NOOPTIMISE
#pragma optimize("", off)
#endif

#define GLAD_GLES2_IMPLEMENTATION
#define IMGUI_DEFINE_MATH_OPERATORS
#include "r_local.h"

#include "common/base64.h"

#include <algorithm>
#include <array>
#include <filesystem>
#include <flat_map>
#include <flat_set>
#include <fmt/chrono.h>
#include <future>
#include <map>
#include <numeric>
#include <random>
#include <ranges>
#include <sstream>
#include <vector>

#include <cmrc/cmrc.hpp>
#include <imgui_impl_glfw.h>
#include <imgui_impl_opengl3.h>
#include <imgui_stdlib.h>
#include <magic_enum/magic_enum.hpp>

CMRC_DECLARE(SimpleGraphic);

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

// =================
// Layer queue class
// =================

#pragma pack(push, r_layerCmd, 1)
struct r_layerCmd_s {
	enum Command : uint8_t {
		VIEWPORT,
		BIND,
		COLOR,
		QUAD,
		SPRITE,
	} cmd;
};

struct r_layerCmdViewport_s {
	r_layerCmd_s::Command cmd;
	r_viewport_s viewport;
};

struct r_layerCmdBind_s {
	r_layerCmd_s::Command cmd;
	r_tex_c* tex;
	uint16_t stackLayer;
	glm::vec2 subLo;
	glm::vec2 subHi;
};

struct r_layerCmdColor_s {
	r_layerCmd_s::Command cmd;
	glm::vec4 col;
};

struct r_layerCmdQuad_s {
	r_layerCmd_s::Command cmd;
	struct {
		std::array<glm::vec2, 4> uv;
		std::array<glm::vec2, 4> xy;
	} quad;
};

struct r_layerCmdSprite_s {
	r_layerCmd_s::Command cmd;
	glm::vec2 pos;
	glm::vec2 size;
	glm::ivec2 coordLo;
	glm::ivec2 coordHi;
	r_effects::tag_e effectTag;
	uint16_t effectSize;
};

namespace r_effects {
	struct CircleMaskData_s {};

	struct ArcBandMaskData_s {
		uint16_t bandWidth;
		uint16_t startDegrees;
		uint16_t stopDegrees;
	};

	struct TiledData_s {};

	std::span<const std::byte> GetEffectStorage(const r_layerCmdSprite_s& sprite) {
		return std::span((const std::byte*)(&sprite + 1), sprite.effectSize);
	}

	std::span<std::byte> GetWritableEffectStorage(r_layerCmdSprite_s& sprite) {
		return std::span((std::byte*)(&sprite + 1), sprite.effectSize);
	}
}

#pragma pack(pop, r_layerCmd)

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
	case Tag::BIND: return sizeof(r_layerCmdBind_s);
	case Tag::COLOR: return sizeof(r_layerCmdColor_s);
	case Tag::QUAD: return sizeof(r_layerCmdQuad_s);
	case Tag::SPRITE: return sizeof(r_layerCmdSprite_s) + extraSize;
	default:
		abort();
	}
}

static size_t CommandExtraSize(const r_layerCmd_s* cmd) {
	using Tag = r_layerCmd_s::Command;
	switch (cmd->cmd) {
	case Tag::VIEWPORT: [[fallthrough]];
	case Tag::BIND: [[fallthrough]];
	case Tag::COLOR: [[fallthrough]];
	case Tag::QUAD: return 0;
	case Tag::SPRITE: return ((const r_layerCmdSprite_s*)cmd)->effectSize;
	default:
		abort();
	}
}

r_layer_c::CmdHandle r_layer_c::GetFirstCommand() const
{
	CmdHandle ret{};
	ret.offset = 0;
	if (cmdCursor > 0) {
		ret.cmd = (r_layerCmd_s*)cmdStorage.data();
	}
	return ret;
}

bool r_layer_c::GetNextCommand(r_layer_c::CmdHandle& handle) const
{
	if (handle.cmd == nullptr) {
		return false;
	}
	auto extraSize = CommandExtraSize(handle.cmd);
	handle.offset += (uint32_t)CommandSize(handle.cmd->cmd, extraSize);
	if (handle.offset >= cmdCursor) {
		handle.cmd = nullptr;
		return false;
	}
	handle.cmd = (r_layerCmd_s*)(cmdStorage.data() + handle.offset);
	return true;
}

std::generator<r_layer_c::CmdHandle> r_layer_c::GetCommands() const
{
	for (CmdHandle cmdH = GetFirstCommand(); cmdH.cmd != nullptr; GetNextCommand(cmdH)) {
		co_yield cmdH;
	}
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
		cmd->viewport = *viewport;
	}
}

void r_layer_c::Bind(r_tex_c* tex, int stackLayer)
{
	if (auto* cmd = (r_layerCmdBind_s*)NewCommand(CommandSize(r_layerCmd_s::BIND))) {
		cmd->cmd = r_layerCmd_s::BIND;
		cmd->tex = tex;
		cmd->stackLayer = stackLayer;
		cmd->subLo = {};
		cmd->subHi = {};
	}
}

void r_layer_c::Color(col4_t col)
{
	if (auto* cmd = (r_layerCmdColor_s*)NewCommand(CommandSize(r_layerCmd_s::COLOR))) {
		cmd->cmd = r_layerCmd_s::COLOR;
		cmd->col = glm::make_vec4(col);
	}
}

void r_layer_c::Quad(glm::vec2 st0, glm::vec2 xy0, glm::vec2 st1, glm::vec2 xy1, glm::vec2 st2, glm::vec2 xy2, glm::vec2 st3, glm::vec2 xy3)
{
	if (auto* cmd = (r_layerCmdQuad_s*)NewCommand(CommandSize(r_layerCmd_s::QUAD))) {
		cmd->cmd = r_layerCmd_s::QUAD;
		cmd->quad.uv[0] = st0; cmd->quad.uv[1] = st1; cmd->quad.uv[2] = st2; cmd->quad.uv[3] = st3;
		cmd->quad.xy[0] = xy0; cmd->quad.xy[1] = xy1; cmd->quad.xy[2] = xy2; cmd->quad.xy[3] = xy3;
	}
}

void r_layer_c::Sprite(glm::vec2 pos, glm::vec2 size, glm::ivec2 pixelLo, glm::ivec2 pixelHi, r_effects::tag_e effectTag, std::shared_ptr<void> effectData)
{
	const auto effectSize = 0;
	if (auto* cmd = (r_layerCmdSprite_s*)NewCommand(CommandSize(r_layerCmd_s::SPRITE, effectSize))) {
		cmd->cmd = r_layerCmd_s::SPRITE;
		cmd->pos = pos;
		cmd->size = size;
		cmd->coordLo = pixelLo;
		cmd->coordHi = pixelHi;
		cmd->effectTag = effectTag;
		cmd->effectSize = effectSize;
		if (effectSize) {
			auto storage = r_effects::GetWritableEffectStorage(*cmd);
			std::ranges::fill(storage, (std::byte)0);
		}
	}
}

struct EffectInfo
{
	r_effects::tag_e tag;
	std::shared_ptr<OpenGLProgram> program;
	std::vector<OpenGLVertexAttribute> vertexAttrs;
	std::vector<GLint> texLocs;
	std::optional<GLint> uScreenSizeLoc;
	uint16_t vertexStride{};
};

struct LatchingLayerState
{
	r_tex_c* tex{};
	int stackLayer{};
	glm::vec4 color{};
	r_viewport_s viewport{};
	std::optional<r_effects::tag_e> effectTag;
	const EffectInfo* effect{};
	bool showStats{};
};

struct BaseVertex
{
	glm::vec2 xy;
	glm::vec2 uv;
	glm::vec4 tint;
	float texId, stackLayer; // vec2
};

struct NoneVertex
{
	BaseVertex common;
};

struct CircleVertex
{
	BaseVertex common;
	glm::vec2 localPos;
};

struct ArcBandVertex
{
	BaseVertex common;
	glm::vec2 localPos;
	glm::vec2 startDir; glm::vec2 stopDir;
	float innerRadius, angleSpan;
};

struct TiledVertex
{
	BaseVertex common;
	glm::vec2 localTexcoord;
};

struct Effects
{
	EffectInfo noneEffect;
	EffectInfo circleEffect;
	EffectInfo arcBandEffect;
	EffectInfo tiledEffect;
};

struct LayerProcessor
{
	LayerProcessor(r_renderer_c* renderer, int layerId, int subLayerId, bool showStats)
		: renderer(renderer)
		, effects(renderer->effects)
		, screenSize(renderer->VirtualScreenWidth(), renderer->VirtualScreenHeight())
	{
		batchGroups.reserve(1u << 16);
		latched.showStats = showStats;
	}

	struct Batch
	{
		r_viewport_s viewport;
		uint32_t indexByteStart{};
		uint32_t indexCount{};
		uint32_t vertexByteStart{};
		uint16_t vertexCount{};

		std::shared_ptr<void> effectData;
		std::vector<r_tex_c*> texSlots;
		std::flat_map<r_tex_c*, uint8_t> texSeen;
	};

	std::shared_ptr<const Effects> effects;

	r_renderer_c* renderer{};
	LatchingLayerState latched;
	glm::vec2 screenSize{};
	bool usedIncompleteTextures = false;

	struct GeometryStorage
	{
		std::vector<std::byte> cpuVertices;
		std::vector<uint16_t> cpuIndices;
		OpenGLBuffer gpuVertices{GL_ARRAY_BUFFER};
		OpenGLBuffer gpuIndices{GL_ELEMENT_ARRAY_BUFFER};
	};
	GeometryStorage geometry;

	// A batch group represents a consecutive sequence of applications of some particular effect.
	struct BatchGroup
	{
		const EffectInfo* effect{};
		std::shared_ptr<void> effectData;
		size_t batchCount{};
	};

	std::vector<BatchGroup> batchGroups;
	std::vector<Batch> batches;

	void EndLayer()
	{
		if (latched.showStats) {
			ImGui::Text("%d batches across %d groups", batches.size(), batchGroups.size());
		}

		// HACK(zao): buffer binds intentionally scoped beyond group dispatch
		auto indexBind = geometry.gpuIndices.ScopedBind();
		auto vertexBind = geometry.gpuVertices.ScopedBind();
		geometry.gpuIndices.BufferData(std::span(geometry.cpuIndices), GL_STREAM_DRAW);
		geometry.gpuVertices.BufferData(std::span(geometry.cpuVertices), GL_STREAM_DRAW);

		std::span<Batch> batchTail(batches);
		for (auto& group : batchGroups) {
			DispatchGroup(group, batchTail.subspan(0, group.batchCount));
			batchTail = batchTail.subspan(group.batchCount);
		}
	}

	template <typename Range>
	void ProcessCommands(Range&& cmds) {
		for (auto cmd : cmds) {
			OnCommand(cmd.cmd);
		}
	}

	void OnCommand(const r_layerCmd_s* cmd)
	{
		switch (cmd->cmd) {
		case r_layerCmd_s::BIND: OnCommand(*(const r_layerCmdBind_s*)cmd); break;
		case r_layerCmd_s::COLOR: OnCommand(*(const r_layerCmdColor_s*)cmd); break;
		case r_layerCmd_s::QUAD: OnCommand(*(const r_layerCmdQuad_s*)cmd); break;
		case r_layerCmd_s::SPRITE: OnCommand(*(const r_layerCmdSprite_s*)cmd); break;
		case r_layerCmd_s::VIEWPORT: OnCommand(*(const r_layerCmdViewport_s*)cmd); break;
		default:
			abort();
		}
	}

	void OnCommand(const r_layerCmdBind_s& cmd)
	{
		latched.tex = cmd.tex;
		latched.stackLayer = cmd.stackLayer;
	}

	void OnCommand(const r_layerCmdColor_s& cmd)
	{
		latched.color = cmd.col;
	}

	const EffectInfo* SelectEffect(r_effects::tag_e tag) const noexcept
	{
		switch (tag) {
		case r_effects::tag_e::None: return &effects->noneEffect;
		case r_effects::tag_e::CircleMask: return &effects->circleEffect;
		case r_effects::tag_e::ArcBandMask: return &effects->arcBandEffect;
		case r_effects::tag_e::Tiled: return &effects->tiledEffect;
		default:
			abort();
		}
	}

	bool TryCutBatch()
	{
		auto& lastGroup = batchGroups.back();
		if (lastGroup.batchCount > 0) {
			auto& lastBatch = batches.back();
			if (lastBatch.viewport == latched.viewport &&
				lastBatch.vertexCount < (1u << 16) - 4 &&
				(lastBatch.texSeen.size() < latched.effect->texLocs.size() || lastBatch.texSeen.contains(latched.tex)))
			{
				return false;
			}
		}

		batches.emplace_back(Batch{
			.viewport = latched.viewport,
			.indexByteStart = (uint32_t)std::span(geometry.cpuIndices).size_bytes(),
			.vertexByteStart = (uint32_t)std::span(geometry.cpuVertices).size_bytes(),
			.texSlots{latched.effect->texLocs.size()},
		});
		++lastGroup.batchCount;
		return true;
	}

	uint8_t RecordTexture(r_tex_c* tex) {
		auto& batch = batches.back();
		const auto [texSlot, freshTex] = batch.texSeen.try_emplace(tex, (uint8_t)batch.texSeen.size());
		if (freshTex) {
			batch.texSlots[texSlot->second] = tex;
			if (!usedIncompleteTextures && tex->status.load(std::memory_order_relaxed) < r_tex_c::Status::DONE) {
				usedIncompleteTextures = true;
			}
		}
		return texSlot->second;
	}

	void OnCommand(const r_layerCmdQuad_s& cmd)
	{
		const auto noneTag = r_effects::tag_e::None;

		// TODO(zao): cull against viewport here if desired

		if (latched.effectTag != noneTag) {
			latched.effectTag = noneTag;
			latched.effect = SelectEffect(noneTag);
			batchGroups.emplace_back(latched.effect, nullptr);
		}
		TryCutBatch();
		auto texSlot = RecordTexture(latched.tex);
		auto& batch = batches.back();

		const auto vp = latched.viewport;
		const auto tint = latched.color;
		const auto stackLayer = latched.stackLayer;

		{
			static_assert(std::tuple_size_v<decltype(cmd.quad.xy)> == 4 && std::tuple_size_v<decltype(cmd.quad.uv)> == 4);
			const auto& quad = cmd.quad;
			std::array<NoneVertex, 4> vertices;
			for (int i = 0; i < 4; ++i) {
				vertices[i] = NoneVertex{
					.common{
						.xy = quad.xy[i] + glm::vec2(vp.lo),
						.uv = quad.uv[i],
						.tint = tint,
						.texId = (float)texSlot,
						.stackLayer = (float)stackLayer,
					},
				};
			}
			geometry.cpuVertices.append_range(std::as_bytes(std::span(vertices)));

			const auto base = batch.vertexCount;
			const std::array<uint16_t, 6> indices{base + 0u, base + 1u, base + 2u, base + 0u, base + 2u, base + 3u};
			geometry.cpuIndices.append_range(indices);

			batch.vertexCount += 4;
			batch.indexCount += 6;
		}
	}

	void OnCommand(const r_layerCmdSprite_s& cmd)
	{
		// TODO(zao): cull against viewport here if desired

		auto effectStorage = r_effects::GetEffectStorage(cmd);
		if (latched.effectTag != cmd.effectTag) {
			latched.effectTag = cmd.effectTag;
			latched.effect = SelectEffect(cmd.effectTag);
			batchGroups.emplace_back(latched.effect, nullptr);
		}
		TryCutBatch();
		auto texSlot = RecordTexture(latched.tex);
		auto& batch = batches.back();

		// TODO(zao): generate geometry and advance that tracking state
		//curBatch.effect->ProcessSprite(cmd, curBatch.effectData, latched);

		const auto vp = latched.viewport;
		const auto tint = latched.color;
		const auto stackLayer = latched.stackLayer;
		const glm::vec2 texSize(
			std::max(1u, latched.tex->fileWidth.load(std::memory_order_relaxed)),
			std::max(1u, latched.tex->fileHeight.load(std::memory_order_relaxed)));

		BaseVertex commonVertex{ .tint = tint, .texId = (float)texSlot, .stackLayer = (float)stackLayer, };
		const glm::vec2 posLo = glm::vec2(vp.lo) + cmd.pos;
		const glm::vec2 posHi = glm::vec2(vp.lo) + cmd.pos + cmd.size;
		const glm::vec2 tcLo = glm::vec2(cmd.coordLo) / texSize;
		const glm::vec2 tcHi = glm::vec2(cmd.coordHi) / texSize;
		auto GenerateBasicVertices = [&](auto& vertices) {
			vertices.fill({.common = commonVertex});

			vertices[0].common.xy = posLo;
			vertices[1].common.xy = {posHi.x, posLo.y};
			vertices[2].common.xy = posHi;
			vertices[3].common.xy = {posLo.x, posHi.y};

			vertices[0].common.uv = tcLo;
			vertices[1].common.uv = {tcHi.x, tcLo.y};
			vertices[2].common.uv = tcHi;
			vertices[3].common.uv = {tcLo.x, tcHi.y};
		};

		switch (cmd.effectTag) {
		case r_effects::tag_e::None: {
			std::array<NoneVertex, 4> vertices{};
			GenerateBasicVertices(vertices);

			geometry.cpuVertices.append_range(std::as_bytes(std::span(vertices)));
		} break;
		case r_effects::tag_e::CircleMask: {
			std::array<CircleVertex, 4> vertices{};
			GenerateBasicVertices(vertices);

			vertices[0].localPos = {0, 0};
			vertices[1].localPos = {1, 0};
			vertices[2].localPos = {1, 1};
			vertices[3].localPos = {0, 1};

			geometry.cpuVertices.append_range(std::as_bytes(std::span(vertices)));
		} break;
		case r_effects::tag_e::ArcBandMask: {
			std::array<CircleVertex, 4> vertices{};
			GenerateBasicVertices(vertices);

			vertices[0].localPos = {0, 0};
			vertices[1].localPos = {1, 0};
			vertices[2].localPos = {1, 1};
			vertices[3].localPos = {0, 1};

			geometry.cpuVertices.append_range(std::as_bytes(std::span(vertices)));
		} break;
		case r_effects::tag_e::Tiled: {
		} break;
		}

		// Expect all effects to generate a triangle pair for now.
		const auto base = batch.vertexCount;
		const std::array<uint16_t, 6> indices{base + 0u, base + 1u, base + 2u, base + 0u, base + 2u, base + 3u};
		geometry.cpuIndices.append_range(indices);

		batch.vertexCount += 4;
		batch.indexCount += 6;
	}

	void OnCommand(const r_layerCmdViewport_s& cmd)
	{
		latched.viewport = cmd.viewport;
	}

	void DispatchGroup(BatchGroup& group, std::span<Batch> batches)
	{
		auto fx = group.effect;
		const auto* groupData = group.effectData.get(); // TODO(zao): remove, unused?
		if (latched.showStats) {
			std::string tagName{magic_enum::enum_name(fx->tag)};
			ImGui::Text("Group of %d batches with %s effect", batches.size(), tagName.c_str());
		}

		glEnable(GL_SCISSOR_TEST);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		auto progUse = fx->program->ScopedUse();
		{
			glViewport(0, 0, screenSize.x, screenSize.y);
			glm::vec2 uScreenSize(screenSize);
			glUniform2fv(*fx->uScreenSizeLoc, 1, glm::value_ptr(uScreenSize));
		}

		auto ConfigureVertexAttribute = [&](uintptr_t vertexByteStart, const OpenGLVertexAttribute& attr) -> bool {
			const uintptr_t offset = vertexByteStart + attr.offset;
			glVertexAttribPointer(attr.location, attr.components, attr.type, attr.normalize, attr.stride, (const void*)offset);
			glEnableVertexAttribArray(attr.location);
			return true;
		};

		auto DeconfigureVertexAttribute = [](const OpenGLVertexAttribute& attr) -> bool {
			glDisableVertexAttribArray(attr.location);
			return true;
		};

		std::optional<r_viewport_s> lastViewport;
		for (auto& batch : batches) {
			if (latched.showStats) {
				ImGui::Text("Batch of %d indices", batch.indexCount);
			}

			for (const auto& attr : fx->vertexAttrs) {
				ConfigureVertexAttribute(batch.vertexByteStart, attr);
			}

			// Clip to viewport
			if (!lastViewport || lastViewport != batch.viewport)
			{
				auto vp = batch.viewport;
				if (latched.showStats) {
					ImGui::Text("New viewport %d,%d+%dx%d", vp.lo.x, vp.lo.y, vp.extent.x, vp.extent.y);
				}
				lastViewport = vp;
				const GLint scissorY = screenSize.y - vp.extent.y - vp.lo.y;
				glScissor(vp.lo.x, scissorY, vp.extent.x, vp.extent.y);
			}

			// Set up textures
			{
				const size_t slotsUsed = batch.texSeen.size();
				for (size_t i = 0, numTex = fx->texLocs.size(); i < numTex; ++i) {
					glUniform1i(fx->texLocs[i], (GLint)i);
					glActiveTexture((GLenum)(GL_TEXTURE0 + i));
					if (i < slotsUsed) {
						auto tex = batch.texSlots[i];
						tex->Bind();
						if (latched.showStats) {
							ImGui::Text("New tex %d (%s) %s", tex->texId, tex->fileName.c_str(), magic_enum::enum_name(tex->status.load()).data());
						}
					}
					else {
						glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
					}
				}
				glActiveTexture(GL_TEXTURE0);
			}

			glDrawElements(GL_TRIANGLES, batch.indexCount, GL_UNSIGNED_SHORT, (const void*)(uintptr_t)batch.indexByteStart);
		}

		for (const auto& attr : fx->vertexAttrs) {
			DeconfigureVertexAttribute(attr);
		}

		glDisable(GL_SCISSOR_TEST);
	}
};

bool r_layer_c::Render()
{
	const int optLevel = renderer->r_layerOptimize->intVal;
	const bool shuffle = renderer->r_layerShuffle->intVal == 1;
	bool usedIncompleteTextures = false;

	if (renderer->glPushGroupMarkerEXT)
	{
		std::ostringstream oss;
		oss << "Layer " << layer << ", sub-layer " << subLayer;
		renderer->glPushGroupMarkerEXT(0, oss.str().c_str());
	}

	bool showStats{};
	if (renderer->debugLayers) {
		if (ImGui::Begin("Layers", &renderer->debugLayers)) {
			std::string heading = fmt::format("Layer {}:{}", layer, subLayer);
			showStats = ImGui::CollapsingHeader(heading.c_str(), ImGuiTreeNodeFlags_DefaultOpen);
		}
	}

	LayerProcessor layerProc(renderer, layer, subLayer, showStats);

	layerProc.ProcessCommands(GetCommands());

	layerProc.EndLayer();
	if (!usedIncompleteTextures) {
		usedIncompleteTextures = layerProc.usedIncompleteTextures;
	}

	if (renderer->debugLayers) {
		ImGui::End();
	}

	if (renderer->glPopGroupMarkerEXT) {
		renderer->glPopGroupMarkerEXT();
	}

	return usedIncompleteTextures;
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

// =============
// Init/Shutdown
// =============

void r_renderer_c::Init(r_featureFlag_e features)
{
	sys->con->PrintFunc("Render Init");

	const auto resources = cmrc::SimpleGraphic::get_filesystem();
	frameHashState.reset(XXH3_createState(), XXH3_freeState);

	apiDpiAware = !!(features & F_DPI_AWARE);

	timer_c timer;
	timer.Start();

	// Initialise OpenGL
	openGL = sys_IOpenGL::GetHandle(sys);
	sys_glSet_s set;
	set.bColor = 32;
	set.bDepth = 24;
	set.bStencil = 0;
	set.vsync = true;
	if (openGL->Init(&set)) {
		sys->Error("OpenGL initialisation failed");
	}

	// Get strings
	st_vendor = (const char*)glGetString(GL_VENDOR);
	st_renderer = (const char*)glGetString(GL_RENDERER);
	st_ver = (const char*)glGetString(GL_VERSION);
	st_ext = (const char*)glGetString(GL_EXTENSIONS);

	glGetIntegerv(GL_MAX_TEXTURE_SIZE, (int*)&texMaxDim);
	sys->con->Printf("GL_MAX_TEXTURE_SIZE: %d\n", texMaxDim);

	// Set default state
	glClearColor(0.0, 0.0, 0.0, 1.0);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);

	// Load extensions
	sys->con->Printf("Loading OpenGL extensions...\n");

	if (strstr(st_ext, "GL_EXT_texture_compression_s3tc")) {
		sys->con->Printf("using GL_EXT_texture_compression_s3tc\n");
		glCompressedTexImage2D = (PFNGLCOMPRESSEDTEXIMAGE2DPROC)openGL->GetProc("glCompressedTexImage2D");
	}
	else {
		sys->con->Printf("GL_EXT_texture_compression_s3tc not supported\n");
		glCompressedTexImage2D = NULL;
	}

	if (strstr(st_ext, "GL_EXT_texture_compression_bptc")) {
		sys->con->Printf("using GL_EXT_texture_compression_bptc\n");
		texBC7 = true;
	}
	else {
		sys->con->Printf("GL_EXT_texture_compression_bptc not supported\n");
		texBC7 = false;
	}

	if (strstr(st_ext, "GL_EXT_debug_marker")) {
		sys->con->Printf("using GL_EXT_debug_marker\n");
		glInsertEventMarkerEXT = (PFNGLINSERTEVENTMARKEREXTPROC)openGL->GetProc("glInsertEventMarkerEXT");
		glPushGroupMarkerEXT = (PFNGLPUSHGROUPMARKEREXTPROC)openGL->GetProc("glPushGroupMarkerEXT");
		glPopGroupMarkerEXT = (PFNGLPOPGROUPMARKEREXTPROC)openGL->GetProc("glPopGroupMarkerEXT");
	}
	else {
		sys->con->Printf("GL_EXT_debug_marker not supported\n");
		glInsertEventMarkerEXT = NULL;
		glPushGroupMarkerEXT = NULL;
		glPopGroupMarkerEXT = NULL;
	}

	texNonPOT = true;

	// Initialise texture manager
	texMan = r_ITexManager::GetHandle(this);

	// Initialise shader array
	numShader = 0;
	memset(shaderList, 0, sizeof(shaderList));

	GLint texUnitCount{};
	glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, &texUnitCount);
	maxTextureImageUnits = (dword)texUnitCount;

	// Initialise vertex programs
	InitEffects();
	//effects[r_effects::tag_e::None] = std::make_shared<NoneRenderEffect>(this);
	//effects[r_effects::tag_e::CircleMask] = std::make_shared<CircleMaskRenderEffect>(this);
	//auto noneEffect = effects[r_effects::tag_e::None];
	//effects[r_effects::tag_e::ArcBandMask] = noneEffect;
	//effects[r_effects::tag_e::Tiled] = noneEffect;

	//effects[r_effects::tag_e::ArcBandMask] = std::make_shared<NoneRenderEffect>(this);
	//effects[r_effects::tag_e::Tiled] = std::make_shared<NoneRenderEffect>(this);

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

	// Set up DPI-scaling render target
	for (int i = 0; i < 2; ++i) {
		auto& rtt = rttMain[i];
		if (i > 0) {
			rtt = rttMain[0]; // Reuse shared parts like dimensions and program/locations.
		}
		glGenFramebuffers(1, &rtt.framebuffer);
		glGenTextures(1, &rtt.colorTexture);
		
		if (i == 0) {
			auto compileShader = [](std::string_view src, GLenum type) -> OpenGLShader {
				OpenGLShader shader(type);
				shader.ShaderSource(src);
				shader.CompileShader();
				return shader;
			};

			auto vs = compileShader(ResourceStringView(resources.open("assets/gles/display_render_target.vert")), GL_VERTEX_SHADER);
			if (!vs.GetCompileSuccess()) {
				auto log = vs.GetInfoLog();
				sys->con->Printf("Scaling VS compile failure: %s\n", log.c_str());
			}
			auto fs = compileShader(ResourceStringView(resources.open("assets/gles/display_render_target.frag")), GL_FRAGMENT_SHADER);
			if (!fs.GetCompileSuccess()) {
				auto log = fs.GetInfoLog();
				sys->con->Printf("Scaling FS compile failure: %s\n", log.c_str());
			}

			rtt.blitProg = std::make_shared<OpenGLProgram>();
			const auto& prog = rtt.blitProg;
			prog->AttachShader(vs);
			prog->AttachShader(fs);
			prog->LinkProgram();
			if (!prog->GetLinkSuccess()) {
				auto log = prog->GetInfoLog();
				sys->con->Printf("Scaling program link failure: %s\n", log.c_str());
			}

			rtt.blitAttribLocPos = prog->GetAttribLocation("a_position").value_or(-1);
			rtt.blitAttribLocTC = prog->GetAttribLocation("a_texcoord").value_or(-1);
			rtt.blitSampleLocColour = prog->GetUniformLocation("s_tex").value_or(-1);
		}
	}

	// Load render resources
	sys->con->Printf("Loading resources...\n");

	whiteImage = RegisterShader("@white", 0);
	blackImage = RegisterShader("@black", 0);

	imguiCtx = ImGui::CreateContext();
	ImGui::SetCurrentContext(imguiCtx);

	ImGui_ImplGlfw_InitForOpenGL((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplOpenGL3_Init("#version 100");

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

	if (imguiCtx) {
		ImGui_ImplOpenGL3_Shutdown();
		ImGui_ImplGlfw_Shutdown();
		ImGui::DestroyContext(imguiCtx);
	}

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

	for (int i = 0; i < 2; ++i) {
		auto& rtt = rttMain[i];
		glDeleteTextures(1, &rtt.colorTexture);
		glDeleteFramebuffers(1, &rtt.framebuffer);
		rtt = {};
	}

	// Shutdown texture manager
	r_ITexManager::FreeHandle(texMan);

	// Shutdown OpenGL
	openGL->Shutdown();
	sys_IOpenGL::FreeHandle(openGL);

	sys->con->Printf("Renderer shutdown complete.\n");
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
	ImGui_ImplOpenGL3_NewFrame();
	ImGui_ImplGlfw_NewFrame();
	ImGui::NewFrame();
	{
		auto& vid = sys->video->vid;
		int wNew = VirtualScreenWidth();
		int hNew = VirtualScreenHeight();
		bool const wantIntegerScaling = fmodf(vid.dpiScale, 1.0f) < 0.0005f;
		for (int i = 0; i < 2; ++i) {
			auto& rtt = rttMain[i];
			if (rtt.width != wNew || rtt.height != hNew) {
				GLint prevTex2D, prevFB;
				glGetIntegerv(GL_TEXTURE_BINDING_2D, &prevTex2D);
				glGetIntegerv(GL_FRAMEBUFFER_BINDING, &prevFB);
				glBindTexture(GL_TEXTURE_2D, rtt.colorTexture);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, wNew, hNew, 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
				GLint const filterMode = wantIntegerScaling ? GL_NEAREST : GL_LINEAR;
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filterMode);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filterMode);

				rtt.width = wNew;
				rtt.height = hNew;

				glBindFramebuffer(GL_FRAMEBUFFER, rtt.framebuffer);
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, rtt.colorTexture, 0);

				glCheckFramebufferStatus(GL_FRAMEBUFFER);

				glBindFramebuffer(GL_FRAMEBUFFER, prevFB);
				glBindTexture(GL_TEXTURE_2D, prevTex2D);
			}
		}
	}

	curLayer = layerList[0];

	SetViewport();
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

	//effectFrameData.clear();
	//for (const auto& [tag, effect] : effects) {
	//	if (!effectFrameData.contains(effect)) {
	//		effectFrameData[effect] = effect->MakeFrameData();
	//	}
	//}

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

			size_t totalDenseFootprint{};
			for (int l = 0; l < numLayer; ++l) {
				size_t byteAcc{};
				auto layer = layerSort[l];
				size_t const numCmd = layer->numCmd;
				totalDenseFootprint += layer->cmdCursor;
			}

			ImGui::Text("Total dense footprint: %sB", BinaryUnitPrefix(totalDenseFootprint).c_str());

			size_t totalCmd{};
			if (ImGui::BeginTable("Layer stats", 6, ImGuiTableFlags_Borders | ImGuiTableFlags_SizingFixedFit)) {
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
		lastFrameHash = 0;
	}

	auto tic = std::chrono::high_resolution_clock::now();

	uint64_t commandDigest = 0;
	if (elideFrames) {
		auto* frameHasher = frameHashState.get();
		XXH3_64bits_reset(frameHasher);

		for (auto lIdx = 0; lIdx < numLayer; ++lIdx) {
			auto layer = layerSort[lIdx];
			uint64_t subHash = XXH3_64bits(layer->cmdStorage.data(), (int)layer->cmdCursor);
			XXH3_64bits_update(frameHasher, &subHash, sizeof(subHash));
		}

		commandDigest = XXH3_64bits_digest(frameHasher);
	}

	++totalFrames;
	const bool elideDraw = lastFrameHash != 0 && lastFrameHash == commandDigest;
	if (!elideDraw)
	{
		glBindFramebuffer(GL_FRAMEBUFFER, GetDrawRenderTarget().framebuffer);
		glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
		for (int l = 0; l < numLayer; l++) {
			auto& layer = layerSort[l];
			if (layerBreak && layerBreak->first == layer->layer && layerBreak->second == layer->subLayer) {
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

	for (int l = 0; l < numLayer; ++l) {
		layerSort[l]->Discard();
	}
	delete[] layerSort;

	{
		auto& rtt = GetPresentRenderTarget();
		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
		glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

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

		glViewport(0, 0, sys->video->vid.fbSize[0], sys->video->vid.fbSize[1]);
		const auto progUse = rtt.blitProg->ScopedUse();
		glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, std::data(blitTriPos));
		glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, std::data(blitTriUV));
		glEnableVertexAttribArray(0);
		glEnableVertexAttribArray(1);
		glBindTexture(GL_TEXTURE_2D, rtt.colorTexture);
		glUniform1i(rtt.blitSampleLocColour, 0);
		glDrawArrays(GL_TRIANGLES, 0, 3);
		glBindTexture(GL_TEXTURE_2D, 0);
	}

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
	ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

	// Swap output buffers
	openGL->Swap();

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

r_shaderHnd_c* r_renderer_c::DuplicateShader(r_shaderHnd_c* hnd)
{
	if (!hnd || !hnd->sh)
		return nullptr;
	return new r_shaderHnd_c(hnd->sh);
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
		if (auto sz = hnd->knownSize) {
			width = sz->x;
			height = sz->y;
			return;
		}
			
		while (hnd->sh->tex->status < r_tex_c::SIZE_KNOWN) {
			Sleep(1);
		}
		const auto sz = glm::ivec2{hnd->sh->tex->fileWidth.load(), hnd->sh->tex->fileHeight.load()};
		width = sz.x;
		height = sz.y;
		hnd->knownSize = sz;
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
	curViewport.lo = glm::ivec2(x, y);
	curViewport.extent = glm::ivec2(width, height);
	curLayer->SetViewport(&curViewport);
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

void r_renderer_c::DrawImage(r_shaderHnd_c* hnd, glm::vec2 pos, glm::vec2 extent, glm::vec2 uv1, glm::vec2 uv2, int stackLayer)
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
		stackLayer);
}

void r_renderer_c::DrawSprite(const r_sprite_s& sprite, glm::vec2 pos, glm::vec2 extent, r_effects::tag_e effectTag, std::shared_ptr<void> effectData)
{
	if (const auto* hnd = sprite.hnd) {
		const auto stackLayer = clamp(sprite.stackLayer, 0, (int)hnd->sh->tex->stackLayers - 1);
		curLayer->Bind(hnd->sh->tex, sprite.stackLayer);
	}
	else {
		curLayer->Bind(hnd->sh->tex, 0);
	}
	curLayer->Color(drawColor);
	curLayer->Sprite(pos, extent, sprite.texelRectLo, sprite.texelRectHi, effectTag, nullptr);
}

void r_renderer_c::DrawImageQuad(r_shaderHnd_c* hnd, glm::vec2 p0, glm::vec2 p1, glm::vec2 p2, glm::vec2 p3, glm::vec2 uv0, glm::vec2 uv1, glm::vec2 uv2, glm::vec2 uv3, int stackLayer)
{
	if (hnd) {
		stackLayer = clamp(stackLayer, 0, (int)hnd->sh->tex->stackLayers - 1);
		curLayer->Bind(hnd->sh->tex, stackLayer);
	}
	else {
		curLayer->Bind(whiteImage->sh->tex);
	}
	curLayer->Color(drawColor);
	curLayer->Quad(
		{uv0.s, uv0.t}, {p0.x, p0.y},
		{uv1.s, uv1.t}, {p1.x, p1.y},
		{uv2.s, uv2.t}, {p2.x, p2.y},
		{uv3.s, uv3.t}, {p3.x, p3.y});
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
	const auto ssPathU8 = ssPath.generic_u8string();
	sys->con->Print(fmt::format("Wrote screenshot to {}\n", std::string_view((const char*)ssPathU8.data(), ssPathU8.size())).c_str());
}

r_renderer_c::RenderTarget& r_renderer_c::GetDrawRenderTarget()
{
	return rttMain[1 - presentRtt];
}

r_renderer_c::RenderTarget& r_renderer_c::GetPresentRenderTarget()
{
	return rttMain[presentRtt];
}

template <typename... T>
std::shared_ptr<OpenGLProgram> CompileTemplatedShaders(r_renderer_c* renderer, std::string_view pathBase, T&&... formatArgs)
{
	const auto resources = cmrc::SimpleGraphic::get_filesystem();
	GLint success = GL_FALSE;
	auto prog = std::make_shared<OpenGLProgram>();

	auto vs = OpenGLShader(GL_VERTEX_SHADER);
	vs.ShaderSource(resources.open(fmt::format("{}.vert", pathBase)));
	vs.CompileShader();
	if (!vs.GetCompileSuccess()) {
		std::string log = vs.GetInfoLog();
		renderer->sys->Error("Failed to compile vertex shader for %s:\n%s", std::string(pathBase).c_str(), log.c_str());
	}
	auto fs = OpenGLShader(GL_FRAGMENT_SHADER);
	const auto tintFragmentTemplate = ResourceStringView(resources.open(fmt::format("{}.frag", pathBase)));
	try {
		std::string fragSource = fmt::vformat(tintFragmentTemplate, fmt::make_format_args(formatArgs...));
		fs.ShaderSource(fragSource);
	}
	catch (std::exception& e) {
		renderer->sys->Error("Failed to format fragment shader for %s:\n%s", std::string(pathBase).c_str(), e.what());
	}
	fs.CompileShader();
	if (!fs.GetCompileSuccess()) {
		std::string log = fs.GetInfoLog();
		renderer->sys->Error("Failed to compile fragment shader for %s:\n%s", std::string(pathBase).c_str(), log.c_str());
	}

	prog->AttachShader(vs);
	prog->AttachShader(fs);
	prog->LinkProgram();
	if (!prog->GetLinkSuccess()) {
		std::string log = prog->GetInfoLog();
		renderer->sys->Error("Failed to link program for %s:\n%s", std::string(pathBase).c_str(), log.c_str());
	}

	return prog;
}

void r_renderer_c::InitEffects()
{
	effects = std::make_shared<Effects>();

	auto maxTexSlots = maxTextureImageUnits;
	std::string textureSwitch;
	{
		fmt::memory_buffer buf;
		for (size_t i = 0; i < maxTexSlots; ++i) {
			if (i == 0) {
				fmt::format_to(fmt::appender(buf), "if (v_texId.x < {}.5) ", i);
			}
			else if (i == maxTexSlots - 1) {
				fmt::format_to(fmt::appender(buf), "else ");
			}
			else {
				fmt::format_to(fmt::appender(buf), "else if (v_texId.x < {}.5)", i);
			}
			fmt::format_to(fmt::appender(buf), R"( {{
	color = texture(s_tex[{}], vec3(v_texcoord, v_texId.y));
}}
)", i, i);
		}
		textureSwitch = to_string(buf);
	}

	std::string textureSwitchTiled;
	{
		fmt::memory_buffer buf;
		for (size_t i = 0; i < maxTexSlots; ++i) {
			if (i == 0) {
				fmt::format_to(fmt::appender(buf), "if (v_texId.x < {}.5) ", i);
			}
			else if (i == maxTexSlots - 1) {
				fmt::format_to(fmt::appender(buf), "else ");
			}
			else {
				fmt::format_to(fmt::appender(buf), "else if (v_texId.x < {}.5)", i);
			}
			fmt::format_to(fmt::appender(buf), R"( {{
	color = vec3(1., 1., 1.); //texture(s_tex[{}], vec3(v_texcoord, v_texId.y));
}}
)", i, i);
		}
	}

	effects->noneEffect.tag = r_effects::tag_e::None;
	effects->noneEffect.program = CompileTemplatedShaders(this, "assets/gles/tinted_texture",
		fmt::arg("SG_TEXTURE_COUNT", maxTexSlots),
		fmt::arg("SG_TEXTURE_SWITCH", textureSwitch));
	effects->noneEffect.vertexStride = sizeof(NoneVertex);

	effects->circleEffect.tag = r_effects::tag_e::CircleMask;
	effects->circleEffect.program = CompileTemplatedShaders(this, "assets/gles/tinted_texture_circle",
		fmt::arg("SG_TEXTURE_COUNT", maxTexSlots),
		fmt::arg("SG_TEXTURE_SWITCH", textureSwitch));
	effects->circleEffect.vertexStride = sizeof(CircleVertex);

	effects->arcBandEffect.tag = r_effects::tag_e::ArcBandMask;
	effects->arcBandEffect.program = CompileTemplatedShaders(this, "assets/gles/tinted_texture_arc_band",
		fmt::arg("SG_TEXTURE_COUNT", maxTexSlots),
		fmt::arg("SG_TEXTURE_SWITCH", textureSwitch));
	effects->arcBandEffect.vertexStride = sizeof(ArcBandVertex);

	effects->tiledEffect.tag = r_effects::tag_e::Tiled;
	effects->tiledEffect.program = CompileTemplatedShaders(this, "assets/gles/tinted_texture_tiled",
		fmt::arg("SG_TEXTURE_COUNT", maxTexSlots),
		fmt::arg("SG_TEXTURE_SWITCH", textureSwitchTiled));
	effects->tiledEffect.vertexStride = sizeof(TiledVertex);

	auto TryDefineVertexAttribute = [](EffectInfo& fx, std::string name, OpenGLVertexAttribute params) {
		if (auto loc = fx.program->GetAttribLocation(name)) {
			params.location = (GLuint)*loc;
			params.stride = fx.vertexStride;
			fx.vertexAttrs.emplace_back(params);
		}
	};

	for (EffectInfo* fx : {&effects->noneEffect, &effects->circleEffect, &effects->arcBandEffect, &effects->tiledEffect}) {
		// Vertex attributes
		TryDefineVertexAttribute(*fx, "a_vertex"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(BaseVertex, xy), 2));
		TryDefineVertexAttribute(*fx, "a_texcoord"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(BaseVertex, uv), 2));
		TryDefineVertexAttribute(*fx, "a_tint"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(BaseVertex, tint), 4));
		TryDefineVertexAttribute(*fx, "a_texId"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(BaseVertex, texId), 2));
		
		// Uniforms
		fx->uScreenSizeLoc = fx->program->GetUniformLocation("u_screenSize");

		// Textures
		fx->texLocs.reserve(maxTexSlots);
		for (size_t i = 0;; ++i) {
			auto loc = fx->program->GetUniformLocation(fmt::format("s_tex[{}]", i));
			if (!loc) {
				break;
			}
			fx->texLocs.push_back(*loc);
		}
	}

	TryDefineVertexAttribute(effects->circleEffect, "a_uvLocal"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(CircleVertex, localPos), 2));

	TryDefineVertexAttribute(effects->arcBandEffect, "a_localCoord"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(ArcBandVertex, localPos), 2));
	TryDefineVertexAttribute(effects->arcBandEffect, "a_startDir_stopDir"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(ArcBandVertex, startDir), 4));
	TryDefineVertexAttribute(effects->arcBandEffect, "a_innerRadius_angleSpan"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(ArcBandVertex, innerRadius), 2));

	TryDefineVertexAttribute(effects->tiledEffect, "a_uvLocal"s, OpenGLVertexAttribute({}, GL_FLOAT, 0, offsetof(TiledVertex, localTexcoord), 2));
	// TODO(zao): more attributes as the shader evolves
}

#ifdef NOOPTIMISE
#pragma optimize("", on)
#undef NOOPTIMISE
#endif
