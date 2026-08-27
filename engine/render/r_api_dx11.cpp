#include "r_local.h"

#include <imgui_impl_glfw.h>
#include <imgui_impl_dx11.h>

r_stateDX_s::r_stateDX_s(r_renderer_c* renderer)
	: r_api_c(renderer)
{
	auto* sys = renderer->sys;

	HRESULT hr = D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, D3D11_CREATE_DEVICE_BGRA_SUPPORT, nullptr, 0, D3D11_SDK_VERSION, &dev, &featureLevel, &ctx);

	texMaxDim = 16384; // DX11.1
	texBC7 = true; //DX11.1
}

void r_stateDX_s::Init()
{
	ImGui_ImplGlfw_InitForOther((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplDX11_Init(dev.p, ctx.p);
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
