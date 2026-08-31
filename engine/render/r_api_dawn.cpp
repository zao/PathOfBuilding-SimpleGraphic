#include "r_local.h"

#include <dawn/native/DawnNative.h>
#include <webgpu/webgpu.h>

#include <GLFW/glfw3.h>
#include <GLFW/glfw3native.h>
#include <imgui_impl_glfw.h>
#include <imgui_impl_wgpu.h>

#include <expected>
#include <ranges>

namespace
{
	static constexpr std::string_view scaleShaderWgsl = R"(
struct VertexInput {
	@builtin(vertex_index) vIdx: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@group(0) @binding(0) var s: sampler;
@group(0) @binding(1) var t: texture_2d<f32>;

@vertex
fn vsMain(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.position = vec4<f32>(
		select(-1.0, 3.0, in.vIdx == 1),
		select(-1.0, 3.0, in.vIdx == 2), 0.0, 1.0);
    out.uv = vec2<f32>(
		select(0.0, 2.0, in.vIdx == 1),
		select(0.0, 2.0, in.vIdx == 2));
    return out;
}

@fragment
fn fsMain(in: VertexOutput) -> @location(0) vec4<f32> {
    let color = textureSample(t, s, in.uv).rgb;
    return vec4<f32>(color, 1.0);
}
)"sv;
}

struct r_stateWG_s : public r_api_c {
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

private:
	void InitInstance();
	void InitSurface();
	void InitAdapter();
	void InitDevice();
	void InitRenderTargets();

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
	};
	std::optional<FrameState> frameState;

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
	const auto requiredFeatures = std::array{
		wgpu::FeatureName::TextureCompressionBC,
	};

	wgpu::DeviceDescriptor descriptor{};
	descriptor.requiredFeatureCount = requiredFeatures.size();
	descriptor.requiredFeatures = requiredFeatures.data();
	wgpu::Limits deviceLimits{};
	descriptor.requiredLimits = &deviceLimits; // TODO(zao): figure the required limits we have
	descriptor.SetDeviceLostCallback(wgpu::CallbackMode::AllowProcessEvents, [this](const wgpu::Device& device, wgpu::DeviceLostReason reason, wgpu::StringView message) {
		if (reason != wgpu::DeviceLostReason::CallbackCancelled) {
			sys->con->Warning(fmt::format(u8"[WGPU] Device loss: {}", AsU8StringView(message)));
		}
	});
	descriptor.SetUncapturedErrorCallback([](const wgpu::Device& device, wgpu::ErrorType type, wgpu::StringView message, r_stateWG_s* self) {
		self->sys->con->Warning(fmt::format(u8"[WGPU] Uncaptured error: {}", AsU8StringView(message)));
	}, this);

	device = adapter.CreateDevice(&descriptor);

	deviceLimits = {};
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
	if (const auto stretchModule = CreateShaderModuleWGSL(scaleShaderWgsl); stretchModule.has_value()) {
		wgpu::RenderPipelineDescriptor pipelineDesc{};
		{
			auto& vertex = pipelineDesc.vertex;
			vertex.bufferCount = 0;
			vertex.buffers = nullptr;
			vertex.module = *stretchModule;
			vertex.entryPoint = "vsMain"sv;
			vertex.constantCount = 0;
			vertex.constants = nullptr;
		}
		{
			auto& primitive = pipelineDesc.primitive;
			primitive.topology = wgpu::PrimitiveTopology::TriangleList;
			primitive.stripIndexFormat = wgpu::IndexFormat::Undefined;
			primitive.frontFace = wgpu::FrontFace::CCW;
			primitive.cullMode = wgpu::CullMode::None;
		}
		wgpu::FragmentState frag;
		{
			frag.module = *stretchModule;
			frag.entryPoint = "fsMain"sv;
			frag.constantCount = 0;
			frag.constants = nullptr;

			pipelineDesc.fragment = &frag;
		}
		pipelineDesc.depthStencil = nullptr;
		wgpu::BlendState blendState;
		{
			blendState.color.srcFactor = wgpu::BlendFactor::One;
			blendState.color.dstFactor = wgpu::BlendFactor::Zero;
			blendState.color.operation = wgpu::BlendOperation::Add;
			blendState.alpha.srcFactor = wgpu::BlendFactor::One;
			blendState.alpha.dstFactor = wgpu::BlendFactor::Zero;
			blendState.alpha.operation = wgpu::BlendOperation::Add;
		}
		wgpu::ColorTargetState colorTarget;
		colorTarget.format = windowSurface.format;
		colorTarget.blend = &blendState;
		colorTarget.writeMask = wgpu::ColorWriteMask::All;
		frag.targetCount = 1;
		frag.targets = &colorTarget;
		{
			auto& multisample = pipelineDesc.multisample;
			multisample.count = 1;
			multisample.mask = ~0u;
			multisample.alphaToCoverageEnabled = false;
		}

		std::array<wgpu::BindGroupLayoutEntry, 2> bindLayoutEntries{};
		// @group(0) @binding(0) var s: sampler;
		{
			auto& entry = bindLayoutEntries[0];
			entry.binding = 0;
			entry.visibility = wgpu::ShaderStage::Fragment;
			entry.sampler = {.type = wgpu::SamplerBindingType::Filtering};
		}
		// @group(0) @binding(1) var t : texture_2d<f32>;
		{
			auto& entry = bindLayoutEntries[1];
			entry.binding = 1;
			entry.visibility = wgpu::ShaderStage::Fragment;
			entry.texture = {.sampleType = wgpu::TextureSampleType::Float, .viewDimension = wgpu::TextureViewDimension::e2D};
		}

		wgpu::BindGroupLayoutDescriptor bindLayoutDesc{};
		bindLayoutDesc.label = "RT Stretch";
		bindLayoutDesc.entryCount = bindLayoutEntries.size();
		bindLayoutDesc.entries = bindLayoutEntries.data();

		renderTargetCommon.bindGroupLayout = device.CreateBindGroupLayout(&bindLayoutDesc);
		
		wgpu::PipelineLayoutDescriptor pipelineLayoutDesc{};
		pipelineLayoutDesc.bindGroupLayoutCount = 1;
		pipelineLayoutDesc.bindGroupLayouts = &renderTargetCommon.bindGroupLayout;
		pipelineDesc.layout = device.CreatePipelineLayout(&pipelineLayoutDesc);

		renderTargetCommon.stretchPipeline = device.CreateRenderPipeline(&pipelineDesc);

		wgpu::SamplerDescriptor samplerDesc{};
		samplerDesc.addressModeU = samplerDesc.addressModeV = wgpu::AddressMode::ClampToEdge;
		samplerDesc.magFilter = wgpu::FilterMode::Linear;
		samplerDesc.minFilter = wgpu::FilterMode::Linear;
		samplerDesc.mipmapFilter = wgpu::MipmapFilterMode::Nearest;
		renderTargetCommon.linearSampler = device.CreateSampler(&samplerDesc);
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
		assert(surfCaps.formatCount > 0);
		surfConfig.format = windowSurface.surfCaps.formats[0];
		surfConfig.usage = wgpu::TextureUsage::RenderAttachment;
		surfConfig.device = device;
		assert(surfCaps.presentModeCount > 0);
		surfConfig.presentMode = windowSurface.surfCaps.presentModes[0];

		windowSurface.surface.Configure(&surfConfig);
		windowSurface.lastFbSize = fbSize;
	}

	glm::ivec2 rtSize{renderer->VirtualScreenWidth(), renderer->VirtualScreenHeight()};
	if (rtSize != renderTargetCommon.lastRtSize) {
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

	wgpu::RenderPassEncoder enc = frameState->encoder.BeginRenderPass(&renderPassDesc);
	enc.End();
}

void r_stateWG_s::DrawPresentTarget()
{
	auto& rtCommon = renderTargetCommon;
	auto& rtt = renderTargets[renderer->GetPresentRenderTarget()];
	wgpu::RenderPassColorAttachment colorAttachment{};
	colorAttachment.view = frameState->targetView;
	colorAttachment.loadOp = wgpu::LoadOp::Clear;
	colorAttachment.storeOp = wgpu::StoreOp::Store;
	auto clear = renderer->clearColor;
	colorAttachment.clearValue = wgpu::Color{.r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0};
	wgpu::RenderPassDescriptor renderPassDesc{};
	renderPassDesc.colorAttachmentCount = 1;
	renderPassDesc.colorAttachments = &colorAttachment;
	renderPassDesc.label = "DrawPresentTarget";

	wgpu::RenderPassEncoder enc = frameState->encoder.BeginRenderPass(&renderPassDesc);
	enc.SetPipeline(rtCommon.stretchPipeline);
	enc.SetBindGroup(0, rtt.bindGroup);
	enc.Draw(3, 1, 0, 0);
	enc.End();
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
