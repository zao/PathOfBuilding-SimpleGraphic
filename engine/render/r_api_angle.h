#pragma once

#include "r_api.h"

struct r_stateGL_s : public r_api_c {
	explicit r_stateGL_s(class r_renderer_c* renderer);

	void Init() override;
	void Shutdown() override;
	void ImGuiBeginFrame() override;
	void ImGuiEndFrame() override;

	void BeginFrame() override;
	void EndFrame() override;
	void PrepareDrawTarget() override;
	void DrawPresentTarget() override;

	std::shared_ptr<r_IRenderStrategy> GetRenderStrategy(const r_layer_c& layer) override;

	std::shared_ptr<void> ScopedDebugMarker(std::u8string_view label) override;
	bool DoScreenshot(image_c& outImg, int type) override;
	std::shared_ptr<void> UploadTextureData(r_tex_c*) override;

	struct Impl;
	std::shared_ptr<Impl> impl;
};
