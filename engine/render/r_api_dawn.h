#pragma once
#include "r_api.h"

struct r_stateWG_s : public r_api_c {
	explicit r_stateWG_s(class r_renderer_c* renderer);

	void Init() override;
	void Shutdown() override;
	void ImGuiBeginFrame() override;
	void ImGuiEndFrame() override;

	void BeginFrame() override;
	void EndFrame() override;
	void PrepareDrawTarget() override;

	struct Impl;
	std::shared_ptr<Impl> impl;
};
