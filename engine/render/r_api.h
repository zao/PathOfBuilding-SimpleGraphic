#pragma once

#include <memory>

struct r_api_c {
	explicit r_api_c(r_renderer_c* renderer);
	virtual ~r_api_c() = default;

	virtual void Init() = 0;
	virtual void Shutdown() = 0;

	virtual void ImGuiBeginFrame() {}
	virtual void ImGuiEndFrame() {}

	virtual void BeginFrame() {}
	virtual void EndFrame() {}

	virtual void PrepareDrawTarget() {}
	virtual void DrawPresentTarget() {}

	BorrowedInterfacePtr<r_renderer_c> renderer = nullptr;
	BorrowedInterfacePtr<sys_IMain> sys = nullptr;

	bool texNonPOT = true;	// Non power-of-2 textures supported?
	dword texMaxDim = 0;	// Maximum texture dimension
	bool texBC7 = true;		// BC7 textures supported?
};
