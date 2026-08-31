#include "r_local.h"

#include <imgui_impl_glfw.h>
#include <imgui_impl_dx11.h>

#include <atlcomcli.h>
#include <d3d11.h>

struct TexDataDX
{
	CComPtr<ID3D11ShaderResourceView> srv;
};

struct r_stateDX_s : public r_api_c {
	explicit r_stateDX_s(class r_renderer_c* renderer);

	void Init() override;
	void Shutdown() override;
	void ImGuiBeginFrame() override;
	void ImGuiEndFrame() override;

	//void BeginFrame() override;
	//void EndFrame() override;

	struct Impl;
	std::shared_ptr<Impl> impl;
};

struct r_stateDX_s::Impl
{
	CComPtr<ID3D11Device> dev;
	CComPtr<ID3D11DeviceContext> ctx;
	D3D_FEATURE_LEVEL featureLevel{};
};

r_stateDX_s::r_stateDX_s(r_renderer_c* renderer)
	: r_api_c(renderer)
{
	impl = std::make_shared<Impl>();
	HRESULT hr = D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, D3D11_CREATE_DEVICE_BGRA_SUPPORT, nullptr, 0, D3D11_SDK_VERSION, &impl->dev, &impl->featureLevel, &impl->ctx);

	texMaxDim = 16384; // DX11.1
	texBC7 = true; //DX11.1
}

void r_stateDX_s::Init()
{
	ImGui_ImplGlfw_InitForOther((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplDX11_Init(impl->dev.p, impl->ctx.p);
}

void r_stateDX_s::Shutdown()
{
	ImGui_ImplDX11_Shutdown();
	ImGui_ImplGlfw_Shutdown();
}

void r_stateDX_s::ImGuiBeginFrame()
{
	ImGui_ImplDX11_NewFrame();
	ImGui_ImplGlfw_NewFrame();
}

void r_stateDX_s::ImGuiEndFrame()
{
	ImGui_ImplDX11_RenderDrawData(ImGui::GetDrawData());
}

std::shared_ptr<r_api_c> MakeDirectXRendererAPI(r_renderer_c* renderer)
{
	return std::make_shared<r_stateDX_s>(renderer);
}
