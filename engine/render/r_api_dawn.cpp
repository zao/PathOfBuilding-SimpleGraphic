#include "r_local.h"

#include <dawn/native/DawnNative.h>
#include <webgpu/webgpu.h>

#include <GLFW/glfw3.h>
#include <GLFW/glfw3native.h>
#include <imgui_impl_glfw.h>
#include <imgui_impl_wgpu.h>

struct TexDataWG
{
	CComPtr<ID3D11ShaderResourceView> srv;
};

struct r_stateWG_s::Impl
{
	dawn::native::Instance dawnInstance;
	dawn::native::Adapter dawnAdapter;

	wgpu::Instance instance;
	wgpu::Adapter adapter;
	wgpu::Device device{};
	wgpu::Queue queue{};
	wgpu::Surface surface{};
	wgpu::SurfaceCapabilities surfCaps{};

	struct FrameState
	{
		wgpu::SurfaceTexture surfaceTexture{};
		wgpu::TextureView textureView{};
	};
	std::optional<FrameState> frameState;

	~Impl();

	void AdvanceFrameState();
};

static std::u8string_view AsU8StringView(WGPUStringView src)
{
	return std::u8string_view((const char8_t*)src.data, src.length);
}

r_stateWG_s::r_stateWG_s(r_renderer_c* renderer)
	: r_api_c(renderer)
{
	impl = std::make_shared<Impl>();
	impl->instance = wgpu::Instance(impl->dawnInstance.Get());

	HMODULE hmod{};
	GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, (LPCWSTR)&GetWineHostVersion, &hmod);

	wgpu::SurfaceSourceWindowsHWND fromHwnd;
	fromHwnd.hinstance = (HINSTANCE)hmod;
	fromHwnd.hwnd = glfwGetWin32Window((GLFWwindow*)sys->video->GetWindowHandle());

	wgpu::SurfaceDescriptor surfaceDesc;
	surfaceDesc.nextInChain = &fromHwnd;

	impl->surface = impl->instance.CreateSurface(&surfaceDesc);

	wgpu::RequestAdapterOptions reqOpts{};
#ifdef WIN32
	reqOpts.backendType = wgpu::BackendType::D3D11;
#endif
	reqOpts.compatibleSurface = impl->surface;
	const auto adapters = impl->dawnInstance.EnumerateAdapters(&reqOpts);
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

	impl->dawnAdapter = discreteAdapter.or_else([&] { return integratedAdapter; }).value();
	impl->adapter = wgpu::Adapter(impl->dawnAdapter.Get());

	wgpu::Limits limits{};
	if (WGPUStatus_Success == wgpu::Adapter(impl->adapter.Get()).GetLimits(&limits)) {
		fmt::basic_memory_buffer<char8_t> buf;
		fmt::format_to(fmt::basic_appender(buf),
				u8R"(Adapter limits:
  - maxTextureDimension1D: {}
  - maxTextureDimension2D: {}
  - maxTextureDimension3D: {}
  - maxTextureArrayLayers: {}
)",
			limits.maxTextureDimension1D, limits.maxTextureDimension2D,
			limits.maxTextureDimension3D, limits.maxTextureArrayLayers);
		sys->con->Print(std::u8string_view(buf.data(), buf.size()));
	}

	const auto requiredFeatures = std::array{
		wgpu::FeatureName::TextureCompressionBC,
	};

	wgpu::DeviceDescriptor descriptor{};
	descriptor.requiredFeatureCount = requiredFeatures.size();
	descriptor.requiredFeatures = requiredFeatures.data();
	descriptor.SetDeviceLostCallback(wgpu::CallbackMode::AllowProcessEvents, [this](const wgpu::Device& device, wgpu::DeviceLostReason reason, wgpu::StringView message) {
		sys->con->Warning(fmt::format(u8"[WGPU] Device loss: {}", AsU8StringView(message)));
	});
	descriptor.SetUncapturedErrorCallback([](const wgpu::Device& device, wgpu::ErrorType type, wgpu::StringView message, r_stateWG_s* self) {
		self->sys->con->Warning(fmt::format(u8"[WGPU] Uncaptured error: {}", AsU8StringView(message)));
	}, this);

	impl->device = impl->adapter.CreateDevice(&descriptor);

	limits = {};
	if (WGPUStatus_Success == impl->device.GetLimits(&limits)) {
		fmt::basic_memory_buffer<char8_t> buf;
		fmt::format_to(fmt::basic_appender(buf),
			u8R"(Device limits:
  - maxTextureDimension1D: {}
  - maxTextureDimension2D: {}
  - maxTextureDimension3D: {}
  - maxTextureArrayLayers: {}
)",
			limits.maxTextureDimension1D, limits.maxTextureDimension2D,
			limits.maxTextureDimension3D, limits.maxTextureArrayLayers);
		sys->con->Print(std::u8string_view(buf.data(), buf.size()));
	}
	texMaxDim = limits.maxTextureDimension2D;
	texBC7 = impl->device.HasFeature(wgpu::FeatureName::TextureCompressionBC);

	impl->queue = impl->device.GetQueue();

	if (WGPUStatus_Success != impl->surface.GetCapabilities(impl->adapter, &impl->surfCaps)) {
		sys->con->Warning(u8"Could not obtain present surface caps.");
	}

	wgpu::SurfaceConfiguration surfConfig{};
	surfConfig.width = sys->video->vid.fbSize.x;
	surfConfig.height = sys->video->vid.fbSize.y;
	assert(surfCaps.formatCount > 0);
	surfConfig.format = impl->surfCaps.formats[0];
	surfConfig.usage = wgpu::TextureUsage::RenderAttachment;
	surfConfig.device = impl->device;
	assert(surfCaps.presentModeCount > 0);
	surfConfig.presentMode = impl->surfCaps.presentModes[0];

	impl->surface.Configure(&surfConfig);
}

void r_stateWG_s::Init()
{
	ImGui_ImplGlfw_InitForOther((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplWGPU_InitInfo initInfo{};
	initInfo.Device = impl->device.Get();
	initInfo.RenderTargetFormat = (WGPUTextureFormat)impl->surfCaps.formats[0];
	ImGui_ImplWGPU_Init(&initInfo);
}

void r_stateWG_s::Shutdown()
{
	ImGui_ImplWGPU_Shutdown();
	ImGui_ImplGlfw_Shutdown();

	impl.reset();
}

void r_stateWG_s::ImGuiBeginFrame()
{
	ImGui_ImplWGPU_NewFrame();
	ImGui_ImplGlfw_NewFrame();
}

void r_stateWG_s::ImGuiEndFrame()
{
	WGPURenderPassEncoder enc{};
	//ImGui_ImplWGPU_RenderDrawData(ImGui::GetDrawData(), {}); // TODO(zao): implement
}

void r_stateWG_s::BeginFrame()
{
	impl->AdvanceFrameState();
	impl->device.Tick();
}

void r_stateWG_s::EndFrame()
{
	impl->surface.Present();
	impl->device.Tick();
	impl->frameState.reset();
}

r_stateWG_s::Impl::~Impl()
{
}

void r_stateWG_s::Impl::AdvanceFrameState()
{
	frameState.emplace();

	surface.GetCurrentTexture(&frameState->surfaceTexture);

	wgpu::TextureViewDescriptor viewDesc{};
	viewDesc.format = frameState->surfaceTexture.texture.GetFormat();
	viewDesc.dimension = wgpu::TextureViewDimension::e2D;
	viewDesc.baseMipLevel = 0;
	viewDesc.mipLevelCount = 1;
	viewDesc.baseArrayLayer = 0;
	viewDesc.arrayLayerCount = 1;
	viewDesc.aspect = wgpu::TextureAspect::All;
	frameState->textureView = frameState->surfaceTexture.texture.CreateView(&viewDesc);
}
