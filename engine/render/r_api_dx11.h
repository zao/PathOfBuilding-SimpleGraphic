#pragma once
#include "r_api.h"

#include <atlcomcli.h>
#include <d3d11.h>

struct r_stateDX_s : public r_api_c {
	explicit r_stateDX_s(class r_renderer_c* renderer);

	void Init() override;
	void Shutdown() override;
	void ImGuiBeginFrame() override;
	void ImGuiEndFrame() override;

	//void BeginFrame() override;
	//void EndFrame() override;

	CComPtr<ID3D11Device> dev;
	CComPtr<ID3D11DeviceContext> ctx;
	D3D_FEATURE_LEVEL featureLevel{};
};
