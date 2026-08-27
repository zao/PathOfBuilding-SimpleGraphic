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

	InterfacePtr<sys_IOpenGL> openGL = nullptr;

	std::u8string st_vendor;	// Vendor string
	std::u8string st_renderer;	// Renderer string
	std::u8string st_ver;		// Version string
	std::u8string st_ext;		// Extension string

	int tintedTextureProgram = 0;

	struct RenderTarget {
		int		width = -1, height = -1;
		GLuint	framebuffer = 0;
		GLuint	colorTexture = 0;

		GLuint	blitProg = 0;
		GLuint	blitAttribLocPos = 0;
		GLuint	blitAttribLocTC = 0;
		GLuint  blitSampleLocColour = 0;
	};

	RenderTarget rttMain[2];
};
