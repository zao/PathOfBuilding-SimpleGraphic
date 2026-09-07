#include "r_local.h"

#include <cmrc/cmrc.hpp>
#include <dawn/native/DawnNative.h>
#include <GLFW/glfw3.h>
#include <GLFW/glfw3native.h>
#include <glm/gtx/compatibility.hpp>
#include <imgui_impl_glfw.h>
#include <imgui_impl_wgpu.h>
#include <webgpu/webgpu.h>

#include <algorithm>
#include <expected>
#include <ranges>

template <>
struct magic_enum::customize::enum_range<gli::format> {
	static constexpr int min = 0;
	static constexpr int max = gli::FORMAT_LAST;
};

CMRC_DECLARE(SimpleGraphic);

struct WGRenderStrategy : r_IRenderStrategy
{
	explicit WGRenderStrategy(r_stateWG_s* api, r_layerId_s id);
	~WGRenderStrategy();
	void ProcessCommand(r_layerCmd_s* cmd) override;
	void Flush() override;
	bool UsedIncompleteTextures() const override { return incompleteTextureUsed; }

	void ProcessCommandInternal(const r_layerCmdBind_s& cmd);
	void ProcessCommandInternal(const r_layerCmdColor_s& cmd);
	void ProcessCommandInternal(const r_layerCmdQuad_s& cmd);
	void ProcessCommandInternal(const r_layerCmdViewport_s& cmd);

	void NewBatch();

	struct r_stateWG_s* api{};
	r_layerId_s layerId{};

	glm::vec4 colorLatch{1.0, 1.0, 1.0, 1.0};
	r_tex_c* texLatch{};
	r_viewport_s viewportLatch{};

	struct Vertex
	{
		glm::vec2 pos;
		glm::vec2 uv;
		glm::u8vec4 color;
		glm::i32vec2 texId; // texId, texLayer
	};
	using Index = uint32_t;

	struct FrameData
	{
		glm::uvec2 screenSize{};
	};

	struct Batch
	{
		size_t idxStart{};
		size_t idxCount{};
		wgpu::Sampler sampler{};
		wgpu::TextureView tex{};
		glm::ivec2 scissorFrom{};
		glm::ivec2 scissorTo{};
	};

	Batch curBatch{};
	std::vector<Batch> readyBatches;

	FrameData cpuFrameData;
	wgpu::Buffer gpuFrameData;

	std::vector<Vertex> cpuVertices;
	wgpu::Buffer gpuVertices;
	
	std::vector<Index> cpuIndices;
	wgpu::Buffer gpuIndices;

	bool cullGeometry{};
	bool incompleteTextureUsed = false;
};

struct r_stateWG_s : public r_api_c {
	friend struct WGRenderStrategy;

	explicit r_stateWG_s(class r_renderer_c* renderer);
	r_stateWG_s& operator = (const r_stateWG_s&) = delete;

	void Init() override;
	void Shutdown() override;
	void ImGuiBeginFrame() override;
	void ImGuiEndFrame() override;

	void BeginFrame() override;
	void EndFrame() override;
	void PrepareDrawTarget() override;
	void DrawPresentTarget() override;

	std::shared_ptr<r_IRenderStrategy> GetRenderStrategy(const r_layer_c& layer) override;
	std::shared_ptr<void> UploadTextureData(r_tex_c*) override;

private:
	void InitInstance();
	void InitSurface();
	void InitAdapter();
	void InitDevice();
	void InitRenderTargets();
	void InitShaders();

	// Utility
	void PrintLimits(const wgpu::Limits& limits, std::u8string_view subject);

	dawn::native::Instance dawnInstance;
	dawn::native::Adapter dawnAdapter;

	wgpu::Instance instance{};
	wgpu::Adapter adapter{};
	wgpu::Device device{};
	wgpu::Queue queue{};

	struct WindowSurface
	{
		wgpu::Surface surface{};
		wgpu::SurfaceCapabilities surfCaps{};
		wgpu::TextureFormat format{};
		glm::ivec2 lastFbSize{};
	};
	WindowSurface windowSurface;

	struct FrameState
	{
		wgpu::SurfaceTexture surfaceTexture{};
		wgpu::TextureView targetView{};
		wgpu::CommandEncoder encoder{};
		std::optional<wgpu::RenderPassEncoder> drawPassEncoder;
	};
	std::optional<FrameState> frameState;

	struct RenderCommon
	{
		bool isLowSpec = false;
		struct PipelineBundle
		{
			wgpu::RenderPipeline pipeline;
			wgpu::BindGroupLayout frameBindGroupLayout;
			wgpu::BindGroupLayout batchBindGroupLayout;
		};
		PipelineBundle tintedTextureBundle;
		
		wgpu::Sampler clampSampler, wrapSampler;
	};
	RenderCommon renderCommon;

	struct RenderTargetCommon
	{
		glm::ivec2 lastRtSize{};
		wgpu::ComputeState scaleVert{};
		wgpu::ComputeState scaleFrag{};

		wgpu::ComputeState stretchVs{}, stretchFs{};
		wgpu::RenderPipeline stretchPipeline{};
		wgpu::Sampler linearSampler{};

		wgpu::BindGroupLayout bindGroupLayout{};
	};
	RenderTargetCommon renderTargetCommon;

	struct RenderTargetState
	{
		wgpu::Texture renderTexture;
		wgpu::TextureView targetView;
		wgpu::BindGroup bindGroup;
	};
	std::array<RenderTargetState, 2> renderTargets;

	void AdvanceFrameState();
	std::expected<wgpu::ShaderModule, std::u8string> CreateShaderModuleWGSL(std::string_view source);
};

struct TexDataWG
{
	wgpu::TextureFormat format;
	wgpu::Texture tex;
	wgpu::TextureView view;
};

static std::u8string_view AsU8StringView(WGPUStringView src)
{
	return std::u8string_view((const char8_t*)src.data, src.length);
}

r_stateWG_s::r_stateWG_s(r_renderer_c* renderer)
	: r_api_c(renderer)
{
	InitInstance();
	InitSurface();
	InitAdapter();
	InitDevice();
	InitRenderTargets();
	InitShaders();
}

void r_stateWG_s::InitInstance()
{
	instance = wgpu::Instance(dawnInstance.Get());
}

void r_stateWG_s::InitSurface()
{
	HMODULE hmod{};
	GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, (LPCWSTR)&GetWineHostVersion, &hmod);

	wgpu::SurfaceSourceWindowsHWND fromHwnd;
	fromHwnd.hinstance = (HINSTANCE)hmod;
	fromHwnd.hwnd = glfwGetWin32Window((GLFWwindow*)sys->video->GetWindowHandle());

	wgpu::SurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = &fromHwnd;

	windowSurface.surface = instance.CreateSurface(&surfaceDesc);
}

void r_stateWG_s::InitAdapter()
{
	// TODO(zao): determine hardware capabilities and set fallbacks accordingly
	renderCommon.isLowSpec = true;
	wgpu::RequestAdapterOptions reqOpts{};
#ifdef WIN32
	reqOpts.backendType = wgpu::BackendType::D3D11;
#endif
	reqOpts.compatibleSurface = windowSurface.surface;
	const auto adapters = dawnInstance.EnumerateAdapters(&reqOpts);
	std::optional<dawn::native::Adapter> discreteAdapter, integratedAdapter;
	for (const auto& a : adapters) {
		wgpu::Adapter adapter(a.Get());
		wgpu::AdapterInfo info{};
		if (WGPUStatus_Success == adapter.GetInfo(&info)) {
			if (info.adapterType == wgpu::AdapterType::DiscreteGPU && !discreteAdapter) {
				discreteAdapter = a;
			}
			else if (info.adapterType == wgpu::AdapterType::IntegratedGPU && !integratedAdapter) {
				integratedAdapter = a;
			}

			fmt::basic_memory_buffer<char8_t> buf;
			fmt::format_to(fmt::basic_appender(buf), u8"Adapter {}\n", fmt::ptr(adapter.Get()));
			fmt::format_to(fmt::basic_appender(buf), u8"  - vendor: {}\n", AsU8StringView(info.vendor));
			fmt::format_to(fmt::basic_appender(buf), u8"  - architecture: {}\n", AsU8StringView(info.architecture));
			fmt::format_to(fmt::basic_appender(buf), u8"  - device: {}\n", AsU8StringView(info.device));
			fmt::format_to(fmt::basic_appender(buf), u8"  - description: {}\n", AsU8StringView(info.description));
			fmt::format_to(fmt::basic_appender(buf), u8"  - backendType: {}\n", AsU8StringView(magic_enum::enum_name(info.backendType)));
			fmt::format_to(fmt::basic_appender(buf), u8"  - adapterType: {}\n", AsU8StringView(magic_enum::enum_name(info.adapterType)));
			fmt::format_to(fmt::basic_appender(buf), u8"  - vendorID: {:#010x}\n", info.vendorID);
			fmt::format_to(fmt::basic_appender(buf), u8"  - deviceID: {:#010x}\n", info.deviceID);
			fmt::format_to(fmt::basic_appender(buf), u8"  - subgroupMinSize: {}\n", info.subgroupMinSize);
			fmt::format_to(fmt::basic_appender(buf), u8"  - subgroupMaxSize: {}\n", info.subgroupMaxSize);
			sys->con->Print(std::u8string_view(buf.data(), buf.size()));
		}
	}

	dawnAdapter = discreteAdapter.or_else([&] { return integratedAdapter; }).value();
	adapter = wgpu::Adapter(dawnAdapter.Get());

	wgpu::Limits limits{};
	if (WGPUStatus_Success == wgpu::Adapter(adapter.Get()).GetLimits(&limits)) {
		PrintLimits(limits, u8"Adapter"sv);
	}
}

void r_stateWG_s::InitDevice()
{
	wgpu::Limits adapterLimits{};
	wgpu::Adapter(adapter.Get()).GetLimits(&adapterLimits);

	const auto requiredFeatures = std::array{
		wgpu::FeatureName::TextureCompressionBC,
	};

	// TODO(zao): figure the required limits we have
	wgpu::Limits requiredLimits{
		.maxTextureArrayLayers = std::max(adapterLimits.maxTextureArrayLayers, 1024u), // TODO(zao): fall back to fewer, maybe do a virtual split of overly large pre-stacks?
		.minUniformBufferOffsetAlignment = adapterLimits.minUniformBufferOffsetAlignment,
		.minStorageBufferOffsetAlignment = adapterLimits.minStorageBufferOffsetAlignment,
	};

	wgpu::DeviceDescriptor descriptor{};
	descriptor.requiredFeatureCount = requiredFeatures.size();
	descriptor.requiredFeatures = requiredFeatures.data();
	descriptor.requiredLimits = &requiredLimits;
	descriptor.SetDeviceLostCallback(wgpu::CallbackMode::AllowSpontaneous, [this](const wgpu::Device& device, wgpu::DeviceLostReason reason, wgpu::StringView message) {
		if (reason != wgpu::DeviceLostReason::CallbackCancelled) {
			sys->con->Warning(fmt::format(u8"[WGPU] Device loss: {}", AsU8StringView(message)));
		}
	});
	descriptor.SetUncapturedErrorCallback([](const wgpu::Device& device, wgpu::ErrorType type, wgpu::StringView message, r_stateWG_s* self) {
		self->sys->con->Warning(fmt::format(u8"[WGPU] Uncaptured error: {}", AsU8StringView(message)));
	}, this);

	device = adapter.CreateDevice(&descriptor);

	wgpu::Limits deviceLimits{};
	if (WGPUStatus_Success == device.GetLimits(&deviceLimits)) {
		PrintLimits(deviceLimits, u8"Device"sv);
	}
	texMaxDim = deviceLimits.maxTextureDimension2D;
	texBC7 = device.HasFeature(wgpu::FeatureName::TextureCompressionBC);

	queue = device.GetQueue();

	if (WGPUStatus_Success != windowSurface.surface.GetCapabilities(adapter, &windowSurface.surfCaps)) {
		sys->con->Warning(u8"Could not obtain present surface caps.");
	}
}

void r_stateWG_s::InitRenderTargets()
{
	// Render target setup
	windowSurface.format = windowSurface.surfCaps.formats[0];
	const auto stretchWgsl = cmrc::SimpleGraphic::get_filesystem().open("assets/webgpu/display_render_target.wgsl");
	if (const auto stretchModule = CreateShaderModuleWGSL(std::string_view(stretchWgsl)); stretchModule.has_value()) {
		wgpu::RenderPipelineDescriptor pipelineDesc{};
		pipelineDesc.vertex = wgpu::VertexState{
			.module = *stretchModule,
			.entryPoint = "vsMain"sv,
			.constantCount = 0,
			.constants = nullptr,
			.bufferCount = 0,
			.buffers = nullptr,
		};
		pipelineDesc.primitive = wgpu::PrimitiveState{
			.topology = wgpu::PrimitiveTopology::TriangleList,
			.stripIndexFormat = wgpu::IndexFormat::Undefined,
			.frontFace = wgpu::FrontFace::CCW,
			.cullMode = wgpu::CullMode::None,
		};
		wgpu::FragmentState frag{
			.module = *stretchModule,
			.entryPoint = "fsMain"sv,
			.constantCount = 0,
			.constants = nullptr,
		};
		pipelineDesc.fragment = &frag;
		pipelineDesc.depthStencil = nullptr;
		wgpu::BlendState blendState{
			.color{
				.operation = wgpu::BlendOperation::Add,
				.srcFactor = wgpu::BlendFactor::One,
				.dstFactor = wgpu::BlendFactor::Zero,
			},
			.alpha{
				.operation = wgpu::BlendOperation::Add,
				.srcFactor = wgpu::BlendFactor::One,
				.dstFactor = wgpu::BlendFactor::Zero,
			},
		};
		wgpu::ColorTargetState colorTarget{
			.format = windowSurface.format,
			.blend = &blendState,
			.writeMask = wgpu::ColorWriteMask::All,
		};
		frag.targetCount = 1;
		frag.targets = &colorTarget;
		pipelineDesc.multisample = wgpu::MultisampleState{
			.count = 1,
			.mask = ~0u,
			.alphaToCoverageEnabled = false,
		};

		std::array<wgpu::BindGroupLayoutEntry, 2> bindLayoutEntries{
			// @group(0) @binding(0) var s: sampler;
			wgpu::BindGroupLayoutEntry{
				.binding = 0,
				.visibility = wgpu::ShaderStage::Fragment,
				.sampler = {
					.type = wgpu::SamplerBindingType::Filtering,
				},
			},
			// @group(0) @binding(1) var t : texture_2d<f32>;
			wgpu::BindGroupLayoutEntry{
				.binding = 1,
				.visibility = wgpu::ShaderStage::Fragment,
				.texture = {
					.sampleType = wgpu::TextureSampleType::Float,
					.viewDimension = wgpu::TextureViewDimension::e2D,
				},
			},
		};

		wgpu::BindGroupLayoutDescriptor bindLayoutDesc{
			.label = "RT Stretch",
			.entryCount = bindLayoutEntries.size(),
			.entries = bindLayoutEntries.data(),
		};

		renderTargetCommon.bindGroupLayout = device.CreateBindGroupLayout(&bindLayoutDesc);

		wgpu::PipelineLayoutDescriptor pipelineLayoutDesc{
			.bindGroupLayoutCount = 1,
			.bindGroupLayouts = &renderTargetCommon.bindGroupLayout,
		};
		pipelineDesc.layout = device.CreatePipelineLayout(&pipelineLayoutDesc);

		renderTargetCommon.stretchPipeline = device.CreateRenderPipeline(&pipelineDesc);

		wgpu::SamplerDescriptor samplerDesc{
			.addressModeU = wgpu::AddressMode::ClampToEdge,
			.addressModeV = wgpu::AddressMode::ClampToEdge,
			.magFilter = wgpu::FilterMode::Linear,
			.minFilter = wgpu::FilterMode::Linear,
			.mipmapFilter = wgpu::MipmapFilterMode::Nearest,
		};
		renderTargetCommon.linearSampler = device.CreateSampler(&samplerDesc);
	}
}

void r_stateWG_s::InitShaders()
{
	// Tinted textures, low-spec fallback
	const auto wgsl = cmrc::SimpleGraphic::get_filesystem().open("assets/webgpu/tinted_texture.wgsl");
	if (const auto shaderModule = CreateShaderModuleWGSL(std::string_view(wgsl)); shaderModule.has_value()) {
		// TODO(zao): set up input assembler
		std::array attributes = {
			wgpu::VertexAttribute{
				.format = wgpu::VertexFormat::Float32x2,
				.offset = offsetof(WGRenderStrategy::Vertex, pos),
				.shaderLocation = 0,
			},
			wgpu::VertexAttribute{
				.format = wgpu::VertexFormat::Float32x2,
				.offset = offsetof(WGRenderStrategy::Vertex, uv),
				.shaderLocation = 1,
			},
			wgpu::VertexAttribute{
				.format = wgpu::VertexFormat::Unorm8x4BGRA,
				.offset = offsetof(WGRenderStrategy::Vertex, color),
				.shaderLocation = 2,
			},
			wgpu::VertexAttribute{
				.format = wgpu::VertexFormat::Sint32x2,
				.offset = offsetof(WGRenderStrategy::Vertex, texId),
				.shaderLocation = 3,
			},
		};
		wgpu::VertexBufferLayout vbLayout{
			.stepMode = wgpu::VertexStepMode::Vertex,
			.arrayStride = sizeof(WGRenderStrategy::Vertex),
			.attributeCount = attributes.size(),
			.attributes = attributes.data(),
		};

		wgpu::BlendState blendState{
			.color{
				.operation = wgpu::BlendOperation::Add,
				.srcFactor = wgpu::BlendFactor::SrcAlpha,
				.dstFactor = wgpu::BlendFactor::OneMinusSrcAlpha,
			},
			.alpha{
				.operation = wgpu::BlendOperation::Add,
				.srcFactor = wgpu::BlendFactor::One,
				.dstFactor = wgpu::BlendFactor::Zero,
			},
		};

		wgpu::ColorTargetState colorTarget{
			.format = windowSurface.format,
			.blend = &blendState,
			.writeMask = wgpu::ColorWriteMask::All,
		};

		wgpu::FragmentState frag{
			.module = *shaderModule,
			.entryPoint = "fsMain"sv,
			.constantCount = 0,
			.constants = nullptr,
			.targetCount = 1,
			.targets = &colorTarget,
		};

		wgpu::RenderPipelineDescriptor pipelineDesc{
			.label = "tinted_texture"sv,
			.vertex{
				.module = *shaderModule,
				.entryPoint = "vsMain"sv,
				.bufferCount = 1,
				.buffers = &vbLayout,
			},
			.primitive{
				.topology = wgpu::PrimitiveTopology::TriangleList,
				.frontFace = wgpu::FrontFace::CCW,
				.cullMode = wgpu::CullMode::None,
			},
			.fragment = &frag,
		};
		
		const auto frameBindLayoutEntries = std::array{
			// @group(0) @binding(0) var<uniform> frameData: FrameUniforms;
			wgpu::BindGroupLayoutEntry{
				.binding = 0,
				.visibility = wgpu::ShaderStage::Vertex,
				.buffer{
					.type = wgpu::BufferBindingType::Uniform,
					.hasDynamicOffset = false,
					.minBindingSize = sizeof(WGRenderStrategy::FrameData),
				},
			},
		};

		const auto batchBindLayoutEntries = std::array{
			// @group(1) @binding(0) var texSampler : sampler;
			wgpu::BindGroupLayoutEntry{
				.binding = 0,
				.visibility = wgpu::ShaderStage::Fragment,
				.sampler{
					.type = wgpu::SamplerBindingType::Filtering,
				},
			},
			// @group(1) @binding(1) var colorTex : texture_2d_array<f32>;
			wgpu::BindGroupLayoutEntry{
				.binding = 1,
				.visibility = wgpu::ShaderStage::Fragment,
				.texture{
					.sampleType = wgpu::TextureSampleType::Float,
					.viewDimension = wgpu::TextureViewDimension::e2DArray,
					.multisampled = false,
				},
			},
		};

		wgpu::BindGroupLayoutDescriptor frameBindLayoutDesc{
			.label = "tinted_texture(frame)",
			.entryCount = frameBindLayoutEntries.size(),
			.entries = frameBindLayoutEntries.data(),
		};
		renderCommon.tintedTextureBundle.frameBindGroupLayout = device.CreateBindGroupLayout(&frameBindLayoutDesc);

		wgpu::BindGroupLayoutDescriptor batchBindLayoutDesc{
			.label = "tinted_texture(batch)",
			.entryCount = batchBindLayoutEntries.size(),
			.entries = batchBindLayoutEntries.data(),
		};
		renderCommon.tintedTextureBundle.batchBindGroupLayout = device.CreateBindGroupLayout(&batchBindLayoutDesc);

		const std::array bindGroupLayouts = std::array{
			renderCommon.tintedTextureBundle.frameBindGroupLayout,
			renderCommon.tintedTextureBundle.batchBindGroupLayout
		};
		wgpu::PipelineLayoutDescriptor pipelineLayoutDesc{
			.bindGroupLayoutCount = bindGroupLayouts.size(),
			.bindGroupLayouts = bindGroupLayouts.data(),
		};
		pipelineDesc.layout = device.CreatePipelineLayout(&pipelineLayoutDesc);

		renderCommon.tintedTextureBundle.pipeline = device.CreateRenderPipeline(&pipelineDesc);

		// TODO(zao): share more of these across renderer
		wgpu::SamplerDescriptor samplerDesc{
			.addressModeU = wgpu::AddressMode::ClampToEdge,
			.addressModeV = wgpu::AddressMode::ClampToEdge,
			.magFilter = wgpu::FilterMode::Linear,
			.minFilter = wgpu::FilterMode::Linear,
			.mipmapFilter = wgpu::MipmapFilterMode::Linear,
		};
		renderCommon.clampSampler = device.CreateSampler(&samplerDesc);

		samplerDesc.addressModeU = samplerDesc.addressModeV = wgpu::AddressMode::Repeat;
		renderCommon.wrapSampler = device.CreateSampler(&samplerDesc);
	}
}

void r_stateWG_s::PrintLimits(const wgpu::Limits& limits, std::u8string_view subject)
{
	fmt::basic_memory_buffer<char8_t> buf;
#define PRINT_LIMIT(name) fmt::format_to(fmt::basic_appender(buf), u8"  - {}: {}\n", u8"" #name ""sv, limits.name)
	fmt::format_to(fmt::basic_appender(buf), u8"{} limits:\n", subject);
	PRINT_LIMIT(maxTextureDimension1D);
	PRINT_LIMIT(maxTextureDimension2D);
	PRINT_LIMIT(maxTextureDimension3D);
	PRINT_LIMIT(maxTextureArrayLayers);
	PRINT_LIMIT(maxBindGroups);
	PRINT_LIMIT(maxBindGroupsPlusVertexBuffers);
	PRINT_LIMIT(maxBindingsPerBindGroup);
	PRINT_LIMIT(maxDynamicUniformBuffersPerPipelineLayout);
	PRINT_LIMIT(maxDynamicStorageBuffersPerPipelineLayout);
	PRINT_LIMIT(maxSampledTexturesPerShaderStage);
	PRINT_LIMIT(maxSamplersPerShaderStage);
	PRINT_LIMIT(maxStorageBuffersPerShaderStage);
	PRINT_LIMIT(maxStorageTexturesPerShaderStage);
	PRINT_LIMIT(maxUniformBuffersPerShaderStage);
	PRINT_LIMIT(maxUniformBufferBindingSize);
	PRINT_LIMIT(maxStorageBufferBindingSize);
	PRINT_LIMIT(minUniformBufferOffsetAlignment);
	PRINT_LIMIT(minStorageBufferOffsetAlignment);
	PRINT_LIMIT(maxVertexBuffers);
	PRINT_LIMIT(maxBufferSize);
	PRINT_LIMIT(maxVertexAttributes);
	PRINT_LIMIT(maxVertexBufferArrayStride);
	PRINT_LIMIT(maxInterStageShaderVariables);
	PRINT_LIMIT(maxColorAttachments);
	PRINT_LIMIT(maxColorAttachmentBytesPerSample);
	PRINT_LIMIT(maxComputeWorkgroupStorageSize);
	PRINT_LIMIT(maxComputeInvocationsPerWorkgroup);
	PRINT_LIMIT(maxComputeWorkgroupSizeX);
	PRINT_LIMIT(maxComputeWorkgroupSizeY);
	PRINT_LIMIT(maxComputeWorkgroupSizeZ);
	PRINT_LIMIT(maxComputeWorkgroupsPerDimension);
	PRINT_LIMIT(maxImmediateSize);
#undef PRINT_LIMIT
	sys->con->Print(std::u8string_view(buf.data(), buf.size()));
}

void r_stateWG_s::Init()
{
	ImGui_ImplGlfw_InitForOther((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplWGPU_InitInfo initInfo{};
	initInfo.Device = device.Get();
	initInfo.RenderTargetFormat = (WGPUTextureFormat)windowSurface.surfCaps.formats[0];
	ImGui_ImplWGPU_Init(&initInfo);
}

void r_stateWG_s::Shutdown()
{
	ImGui_ImplWGPU_Shutdown();
	ImGui_ImplGlfw_Shutdown();
}

void r_stateWG_s::ImGuiBeginFrame()
{
	ImGui_ImplWGPU_NewFrame();
	ImGui_ImplGlfw_NewFrame();
}

void r_stateWG_s::ImGuiEndFrame()
{
	wgpu::RenderPassColorAttachment colorAttachment{};
	colorAttachment.view = frameState->targetView;
	colorAttachment.loadOp = wgpu::LoadOp::Load;
	colorAttachment.storeOp = wgpu::StoreOp::Store;

	wgpu::RenderPassDescriptor renderPassDesc{};
	renderPassDesc.colorAttachmentCount = 1;
	renderPassDesc.colorAttachments = &colorAttachment;
	renderPassDesc.label = "Dear ImGui";

	wgpu::RenderPassEncoder enc = frameState->encoder.BeginRenderPass(&renderPassDesc);
	ImGui_ImplWGPU_RenderDrawData(ImGui::GetDrawData(), enc.Get());
	enc.End();
}

void r_stateWG_s::BeginFrame()
{
	const auto fbSize = sys->video->vid.fbSize;
	if (fbSize != windowSurface.lastFbSize) {
		windowSurface.surface.Unconfigure();

		wgpu::SurfaceConfiguration surfConfig{};
		surfConfig.width = fbSize.x;
		surfConfig.height = fbSize.y;
		assert(windowSurface.surfCaps.formatCount > 0);
		surfConfig.format = windowSurface.surfCaps.formats[0];
		surfConfig.usage = wgpu::TextureUsage::RenderAttachment;
		surfConfig.device = device;
		assert(windowSurface.surfCaps.presentModeCount > 0);
		surfConfig.presentMode = windowSurface.surfCaps.presentModes[0];

		windowSurface.surface.Configure(&surfConfig);
		windowSurface.lastFbSize = fbSize;
	}

	glm::ivec2 rtSize{renderer->VirtualScreenWidth(), renderer->VirtualScreenHeight()};
	if (renderTargetCommon.lastRtSize != rtSize) {
		renderTargetCommon.lastRtSize = rtSize;
		renderTargets = {};
		std::array<wgpu::BindGroupEntry, 2> bindEntries{};
		{
			auto& entry = bindEntries[0];
			entry.binding = 0;
			entry.sampler = renderTargetCommon.linearSampler;
		}
		{
			auto& entry = bindEntries[1];
			entry.binding = 1;
		}
		wgpu::BindGroupDescriptor bindDesc{};
		bindDesc.layout = renderTargetCommon.bindGroupLayout;
		bindDesc.entryCount = bindEntries.size();
		bindDesc.entries = bindEntries.data();

		for (const auto& [rtIdx, rt] : renderTargets | std::views::enumerate) {
			const auto rtLabel = fmt::format("RenderTarget#{}", rtIdx);
			wgpu::TextureDescriptor texDesc{};
			texDesc.label = std::string_view(rtLabel);
			texDesc.usage = wgpu::TextureUsage::RenderAttachment | wgpu::TextureUsage::TextureBinding;
			texDesc.dimension = wgpu::TextureDimension::e2D;
			texDesc.size = wgpu::Extent3D{(uint32_t)rtSize.x, (uint32_t)rtSize.y};
			texDesc.format = windowSurface.format;
			rt.renderTexture = device.CreateTexture(&texDesc);

			wgpu::TextureViewDescriptor viewDesc{};
			rt.targetView = rt.renderTexture.CreateView(&viewDesc);

			bindEntries[1].textureView = rt.targetView;
			rt.bindGroup = device.CreateBindGroup(&bindDesc);
		}
	}

	AdvanceFrameState();
	device.Tick();
}

void r_stateWG_s::EndFrame()
{
	wgpu::CommandBuffer command = frameState->encoder.Finish();
	queue.Submit(1, &command);
	windowSurface.surface.Present();
	device.Tick();
	frameState.reset();
}

void r_stateWG_s::PrepareDrawTarget()
{
	auto& rtt = renderTargets[renderer->GetDrawRenderTarget()];
	wgpu::RenderPassColorAttachment colorAttachment{};
	colorAttachment.view = rtt.targetView;
	colorAttachment.loadOp = wgpu::LoadOp::Clear;
	colorAttachment.storeOp = wgpu::StoreOp::Store;
	auto clear = renderer->clearColor;
	colorAttachment.clearValue = wgpu::Color{.r = clear.r, .g = clear.g, .b = clear.b, .a = clear.a};
	wgpu::RenderPassDescriptor renderPassDesc{};
	renderPassDesc.colorAttachmentCount = 1;
	renderPassDesc.colorAttachments = &colorAttachment;
	renderPassDesc.label = "PrepareDrawTarget";

	frameState->drawPassEncoder = frameState->encoder.BeginRenderPass(&renderPassDesc);
}

void r_stateWG_s::DrawPresentTarget()
{
	auto& rtCommon = renderTargetCommon;
	auto& rtt = renderTargets[renderer->GetPresentRenderTarget()];

	if (frameState->drawPassEncoder) {
		frameState->drawPassEncoder->End();
	}

	wgpu::RenderPassDescriptor renderPassDesc{};
	wgpu::RenderPassColorAttachment colorAttachment{};
	colorAttachment.view = frameState->targetView;
	colorAttachment.loadOp = wgpu::LoadOp::Clear;
	colorAttachment.storeOp = wgpu::StoreOp::Store;
	auto clear = renderer->clearColor;
	colorAttachment.clearValue = wgpu::Color{.r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0};

	wgpu::BindGroup drawBindGroup;
	{
		const auto bindEntries = std::array{
			wgpu::BindGroupEntry{
				.binding = 0,
				.sampler = renderTargetCommon.linearSampler,
			},
			wgpu::BindGroupEntry{
				.binding = 1,
				.textureView = rtt.targetView,
			},
		};
		wgpu::BindGroupDescriptor bindDesc{};
		bindDesc.layout = renderTargetCommon.bindGroupLayout;
		bindDesc.entryCount = bindEntries.size();
		bindDesc.entries = bindEntries.data();

		drawBindGroup = device.CreateBindGroup(&bindDesc);
	}

	renderPassDesc.colorAttachmentCount = 1;
	renderPassDesc.colorAttachments = &colorAttachment;
	renderPassDesc.label = "DrawPresentTarget";

	wgpu::RenderPassEncoder enc = frameState->encoder.BeginRenderPass(&renderPassDesc);
	enc.SetPipeline(rtCommon.stretchPipeline);
	enc.SetBindGroup(0, drawBindGroup);
	enc.Draw(3, 1, 0, 0);
	enc.End();
}

std::shared_ptr<r_IRenderStrategy> r_stateWG_s::GetRenderStrategy(const r_layer_c& layer)
{
	return std::make_shared<WGRenderStrategy>(this, layer.id);
}

static wgpu::TextureFormat TextureFormatWgpuFromGli(gli::format format)
{
	switch (format)
	{
	case gli::FORMAT_RGBA8_UNORM_PACK8:
		return wgpu::TextureFormat::RGBA8Unorm;
	case gli::FORMAT_RGBA8_SRGB_PACK8:
		return wgpu::TextureFormat::RGBA8UnormSrgb;
	case gli::FORMAT_RGBA_DXT1_UNORM_BLOCK8:
		return wgpu::TextureFormat::BC1RGBAUnorm;
	case gli::FORMAT_RGBA_DXT1_SRGB_BLOCK8:
		return wgpu::TextureFormat::BC1RGBAUnormSrgb;
	case gli::FORMAT_RGBA_DXT3_UNORM_BLOCK16:
		return wgpu::TextureFormat::BC2RGBAUnorm;
	case gli::FORMAT_RGBA_DXT3_SRGB_BLOCK16:
		return wgpu::TextureFormat::BC2RGBAUnormSrgb;
	case gli::FORMAT_RGBA_DXT5_UNORM_BLOCK16:
		return wgpu::TextureFormat::BC3RGBAUnorm;
	case gli::FORMAT_RGBA_DXT5_SRGB_BLOCK16:
		return wgpu::TextureFormat::BC3RGBAUnormSrgb;
	case gli::FORMAT_RGBA_BP_UNORM_BLOCK16:
		return wgpu::TextureFormat::BC7RGBAUnorm;
	case gli::FORMAT_RGBA_BP_SRGB_BLOCK16:
		return wgpu::TextureFormat::BC7RGBAUnormSrgb;
	default:
		return wgpu::TextureFormat::Undefined;
	}
}

std::shared_ptr<void> r_stateWG_s::UploadTextureData(r_tex_c* tex)
{
	auto texData = std::make_shared<TexDataWG>();
	auto& img = *tex->img;
	wgpu::TextureFormat mappedFormat = TextureFormatWgpuFromGli(img.tex.format());
	if (mappedFormat == wgpu::TextureFormat::Undefined) {
		sys->con->Warning(fmt::format(u8"Unsupported texture format in texture upload: {}", AsU8StringView(magic_enum::enum_name(img.tex.format()))));
		return {};
	}

	const auto format = img.tex.format();
	const auto extent = img.tex.extent();
	wgpu::TextureDescriptor texDesc{
		.label = AsStringView(tex->fileName),
		.usage = wgpu::TextureUsage::CopyDst | wgpu::TextureUsage::TextureBinding,
		.dimension = wgpu::TextureDimension::e2D,
		.size = wgpu::Extent3D(extent.x, extent.y, img.tex.layers()),
		.format = mappedFormat,
		.mipLevelCount = (uint32_t)img.tex.levels(),
	};
	texData->tex = device.CreateTexture(&texDesc);
	wgpu::TextureViewDescriptor viewDesc{
		.dimension = wgpu::TextureViewDimension::e2DArray,
	};
	texData->view = texData->tex.CreateView(&viewDesc);

	const auto blockSize = gli::block_size(img.tex.format());
	const auto blockExtents = gli::block_extent(img.tex.format());

	wgpu::TexelCopyTextureInfo dest{
		.texture = texData->tex,
		.aspect = wgpu::TextureAspect::All,
	};

	for (size_t layerIdx = 0; layerIdx < img.tex.layers(); ++layerIdx) {
		for (size_t mipIdx = 0; mipIdx < img.tex.levels(); ++mipIdx) {
			dest.mipLevel = mipIdx;
			dest.origin.z = layerIdx;
				
			wgpu::TexelCopyBufferLayout dataLayout{};
			wgpu::Extent3D writeSize{
				.depthOrArrayLayers = 1
			};

			auto mipExtent = img.tex.extent(mipIdx);
			if (gli::is_compressed(format)) {
				auto PhysicalBlocks = [](auto val, auto block) { return (val + block - 1) / block; };
				const glm::ivec2 numBlocks{
					PhysicalBlocks(mipExtent.x, blockExtents.x),
					PhysicalBlocks(mipExtent.y, blockExtents.y),
				};
				const auto physicalExtent = numBlocks * glm::ivec2(blockExtents);
				dataLayout.bytesPerRow = numBlocks.x * blockSize;
				dataLayout.rowsPerImage = numBlocks.y;
				writeSize.width = physicalExtent.x;
				writeSize.height = physicalExtent.y;
			}
			else {
				dataLayout.bytesPerRow = mipExtent.x * blockSize;
				dataLayout.rowsPerImage = mipExtent.y;
				writeSize.width = mipExtent.x;
				writeSize.height = mipExtent.y;
			}

			queue.WriteTexture(&dest, img.tex.data(layerIdx, 0, mipIdx), img.tex.size(mipIdx), &dataLayout, &writeSize);
		}
	}
	return texData;
}

void r_stateWG_s::AdvanceFrameState()
{
	frameState.emplace();
	windowSurface.surface.GetCurrentTexture(&frameState->surfaceTexture);

	wgpu::TextureViewDescriptor viewDesc{};
	viewDesc.format = frameState->surfaceTexture.texture.GetFormat();
	viewDesc.dimension = wgpu::TextureViewDimension::e2D;
	viewDesc.baseMipLevel = 0;
	viewDesc.mipLevelCount = 1;
	viewDesc.baseArrayLayer = 0;
	viewDesc.arrayLayerCount = 1;
	viewDesc.aspect = wgpu::TextureAspect::All;
	frameState->targetView = frameState->surfaceTexture.texture.CreateView(&viewDesc);

	frameState->encoder = device.CreateCommandEncoder();
}

std::expected<wgpu::ShaderModule, std::u8string> r_stateWG_s::CreateShaderModuleWGSL(std::string_view source)
{
	wgpu::ShaderSourceWGSL wgslDesc{};
	wgslDesc.code = source;

	wgpu::ShaderModuleDescriptor desc{.nextInChain = &wgslDesc};

	device.PushErrorScope(wgpu::ErrorFilter::Validation);
	wgpu::ShaderModule module = device.CreateShaderModule(&desc);
	bool validationError = false;
	std::optional<std::u8string> validationMessage;
	device.PopErrorScope(wgpu::CallbackMode::AllowSpontaneous, [this, &validationError, &validationMessage](wgpu::PopErrorScopeStatus status, wgpu::ErrorType type, wgpu::StringView message) {
		if (type == wgpu::ErrorType::Validation) {
			validationError = true;
			validationMessage = std::u8string(AsU8StringView(message));
			sys->con->Warning(fmt::format(u8"WGSL validation failed: {}", AsU8StringView(message)));
		}
	});
	if (module && !validationError)
		return module;
	else
		return std::unexpected(std::move(*validationMessage));
}

std::shared_ptr<r_api_c> MakeWebGPURendererAPI(r_renderer_c* renderer)
{
	return std::make_shared<r_stateWG_s>(renderer);
}

WGRenderStrategy::WGRenderStrategy(r_stateWG_s* api, r_layerId_s layerId)
	: api(api)
	, layerId(layerId)
	, cullGeometry(!!api->renderer->r_drawCull->intVal)
{
	cpuVertices.reserve(90 * 1024);
	cpuIndices.reserve(120 * 1024);
	cpuFrameData.screenSize = api->renderTargetCommon.lastRtSize;
}

WGRenderStrategy::~WGRenderStrategy()
{
}

void WGRenderStrategy::ProcessCommandInternal(const r_layerCmdBind_s& cmd) {
	if (cmd.tex->GetStatus() == r_tex_c::Status::DONE)
		texLatch = cmd.tex;
	else
		texLatch = nullptr;
}

void WGRenderStrategy::ProcessCommandInternal(const r_layerCmdColor_s& cmd) {
	colorLatch = glm::saturate(glm::make_vec4(cmd.col));
}

void WGRenderStrategy::ProcessCommandInternal(const r_layerCmdQuad_s& cmd) {
	// Avoid using incomplete textures
	if (!texLatch) {
		incompleteTextureUsed = true;
		return;
	}

	// Pre-cull against viewport
	const auto& q = cmd.quad;
	if (cullGeometry) {
		auto [minX, maxX] = std::ranges::minmax(q.x);
		auto [minY, maxY] = std::ranges::minmax(q.y);
		const bool intersects = minX < viewportLatch.extent.x && maxX > 0.0f && minY < viewportLatch.extent.y && maxY > 0.0f;
		if (!intersects) {
			return;
		}
	}

	const auto apiTex = std::static_pointer_cast<TexDataWG>(texLatch->apiData);

	const auto scissorTo = viewportLatch.lo + viewportLatch.extent;
	if (curBatch.idxCount >= 0) {
		if (curBatch.tex.Get() != apiTex->view.Get() || curBatch.scissorFrom != viewportLatch.lo || curBatch.scissorTo != scissorTo) {
			// Cut new batch if latched state is incompatible with existing batch geometry
			NewBatch();
		}
	}
	
	auto& newSampler = texLatch->flags & TF_CLAMP ? api->renderCommon.clampSampler : api->renderCommon.wrapSampler;
	if (curBatch.sampler.Get() != newSampler.Get()) {
		curBatch.sampler = newSampler;
	}
	if (curBatch.tex.Get() != apiTex->view.Get()) {
		curBatch.tex = apiTex->view;
	}
	curBatch.scissorFrom = viewportLatch.lo;
	curBatch.scissorTo = scissorTo;

	const auto viewportOffset = glm::vec2(viewportLatch.lo);
	const auto b = (uint32_t)cpuVertices.size();
	for (int i = 0; i < 4; ++i) {
		glm::vec2 pos{q.x[i], q.y[i]};
		auto color = glm::u8vec4(colorLatch * 255.0f);
		std::swap(color[0], color[2]); // vertex format is BGRA8
		cpuVertices.push_back(Vertex{
			.pos = pos + viewportOffset,
			.uv = {q.s[i], q.t[i]},
			.color = color,
			.texId = {0, (int32_t)q.stackLayer},
		});
	}
	const auto indices = {b, b + 1, b + 2, b, b + 2, b + 3};
	cpuIndices.insert(cpuIndices.end(), begin(indices), end(indices));
	curBatch.idxCount += 6;
}

void WGRenderStrategy::ProcessCommandInternal(const r_layerCmdViewport_s& cmd) {
	if (viewportLatch == cmd.viewport)
		return;

	viewportLatch = cmd.viewport;
	NewBatch();
}

void WGRenderStrategy::NewBatch()
{
	if (curBatch.idxCount > 0) {
		readyBatches.push_back(curBatch);
		curBatch = Batch{
			.idxStart = cpuIndices.size(),
		};
	}
}

void WGRenderStrategy::ProcessCommand(r_layerCmd_s* layerCmd)
{
	switch (layerCmd->cmd)
	{
	case r_layerCmd_s::BIND:
		ProcessCommandInternal((const r_layerCmdBind_s&)*layerCmd);
		break;
	case r_layerCmd_s::COLOR:
		ProcessCommandInternal((const r_layerCmdColor_s&)*layerCmd);
		break;
	case r_layerCmd_s::QUAD:
		ProcessCommandInternal((const r_layerCmdQuad_s&)*layerCmd);
		break;
	case r_layerCmd_s::VIEWPORT:
		ProcessCommandInternal((const r_layerCmdViewport_s&)*layerCmd);
		break;
	}
}

void WGRenderStrategy::Flush()
{
	NewBatch();
	if (readyBatches.empty())
		return;

	wgpu::BufferDescriptor vbDesc{
		.usage = wgpu::BufferUsage::CopyDst | wgpu::BufferUsage::Vertex,
		.size = sizeof(Vertex) * cpuVertices.size(),
	};
	gpuVertices = api->device.CreateBuffer(&vbDesc);
	api->queue.WriteBuffer(gpuVertices, 0, cpuVertices.data(), std::span(cpuVertices).size_bytes());

	wgpu::BufferDescriptor ibDesc{
		.usage = wgpu::BufferUsage::CopyDst | wgpu::BufferUsage::Index,
		.size = sizeof(Index) * cpuIndices.size(),
	};
	gpuIndices = api->device.CreateBuffer(&ibDesc);
	api->queue.WriteBuffer(gpuIndices, 0, cpuIndices.data(), std::span(cpuIndices).size_bytes());

	wgpu::BufferDescriptor frameDataDesc{
		.usage = wgpu::BufferUsage::CopyDst | wgpu::BufferUsage::Uniform,
		.size = sizeof(FrameData),
	};
	gpuFrameData = api->device.CreateBuffer(&frameDataDesc);
	api->queue.WriteBuffer(gpuFrameData, 0, &cpuFrameData, sizeof(FrameData));

	std::map<std::pair<WGPUSampler, WGPUTextureView>, wgpu::BindGroup> bindGroupCache;
	auto& enc = api->frameState->drawPassEncoder.value();
	enc.SetIndexBuffer(gpuIndices, sizeof(Index) == 4 ? wgpu::IndexFormat::Uint32 : wgpu::IndexFormat::Uint16);
	enc.SetVertexBuffer(0, gpuVertices);
	// Frame common
	enc.SetPipeline(api->renderCommon.tintedTextureBundle.pipeline);
	{
		const auto bgEntries = std::array{
			wgpu::BindGroupEntry{
				.binding = 0,
				.buffer = gpuFrameData,
			},
		};
		wgpu::BindGroupDescriptor bgDesc{
			.layout = api->renderCommon.tintedTextureBundle.frameBindGroupLayout,
			.entryCount = bgEntries.size(),
			.entries = bgEntries.data(),
		};
		auto frameBindGroup = api->device.CreateBindGroup(&bgDesc);
		enc.SetBindGroup(0, frameBindGroup);
	}

	for (const auto& batch : readyBatches) {
		if (batch.idxCount == 0)
			continue;

		const auto scissorExtent = batch.scissorTo - batch.scissorFrom;
		enc.SetScissorRect(batch.scissorFrom.x, batch.scissorFrom.y, scissorExtent.x, scissorExtent.y);
		auto bgKey = std::pair{batch.sampler.Get(), batch.tex.Get()};
		auto& bindGroup = bindGroupCache[bgKey];
		if (!bindGroup) {
			const auto bgEntries = std::array{
				wgpu::BindGroupEntry{
					.binding = 0,
					.sampler = batch.sampler,
				},
				wgpu::BindGroupEntry{
					.binding = 1,
					.textureView = batch.tex,
				},
			};
			wgpu::BindGroupDescriptor bgDesc{
				.layout = api->renderCommon.tintedTextureBundle.batchBindGroupLayout,
				.entryCount = bgEntries.size(),
				.entries = bgEntries.data(),
			};
			bindGroup = api->device.CreateBindGroup(&bgDesc);
		}
		enc.SetBindGroup(1, bindGroup);
		enc.DrawIndexed(batch.idxCount, 1, batch.idxStart);
	}
}

