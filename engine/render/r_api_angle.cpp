#include "r_local.h"

#include <imgui_impl_glfw.h>
#include <imgui_impl_opengl3.h>

static bool GetShaderCompileSuccess(GLuint id)
{
	GLint success{};
	glGetShaderiv(id, GL_COMPILE_STATUS, &success);
	return success == GL_TRUE;
}

static std::u8string GetShaderInfoLog(GLuint id)
{
	GLint len{};
	glGetShaderiv(id, GL_INFO_LOG_LENGTH, &len);
	std::vector<char8_t> msg(len);
	glGetShaderInfoLog(id, (GLsizei)msg.size(), &len, (char*)msg.data());
	return std::u8string(msg.data(), msg.data() + len);
}

static bool GetProgramLinkSuccess(GLuint id)
{
	GLint success{};
	glGetProgramiv(id, GL_LINK_STATUS, &success);
	return success == GL_TRUE;
}

static std::u8string GetProgramInfoLog(GLuint id)
{
	GLint len{};
	glGetProgramiv(id, GL_INFO_LOG_LENGTH, &len);
	std::vector<char8_t> msg(len);
	glGetProgramInfoLog(id, (GLsizei)msg.size(), &len, (char*)msg.data());
	return std::u8string(msg.data(), msg.data() + len);
}

constexpr char const* s_tintedTextureVertexSource = R"(#version 300 es

uniform mat4 mvp_matrix;

in vec2 a_vertex;
in vec2 a_texcoord;
in vec4 a_tint;
in vec4 a_viewport;
in vec3 a_texId;

out vec2 v_screenPos;
out vec2 v_texcoord;
out vec4 v_tint;
out vec4 v_viewport;
out vec3 v_texId;

void main(void)
{
	v_texcoord = a_texcoord;
	v_tint = a_tint;
	v_texId = a_texId;
	vec2 vp0 = a_viewport.xy + vec2(0.0, a_viewport.w);
	vec2 vp1 = a_viewport.xy + vec2(a_viewport.z, 0.0);
	v_viewport = vec4(
		(mvp_matrix * vec4(vp0, 0.0, 1.0)).xy,
		(mvp_matrix * vec4(vp1, 0.0, 1.0)).xy);
	vec4 pos = mvp_matrix * vec4(a_vertex + a_viewport.xy, 0.0, 1.0);
	v_screenPos = pos.xy;
	gl_Position = pos;
}
)";

constexpr char const* s_tintedTextureFragmentTemplate = R"(#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];
uniform vec4 i_tint;

in vec2 v_screenPos;
in vec2 v_texcoord;
in vec4 v_tint;
in vec4 v_viewport; // x0, y0, x1, y1
in vec3 v_texId;

out vec4 f_fragColor;

void main(void)
{{
	float x = v_screenPos[0], y = v_screenPos[1];
	if (x < v_viewport[0] ||
	    y < v_viewport[1] ||
	    x >= v_viewport[2] ||
	    y >= v_viewport[3]) {{
		discard;
	}}
	vec4 color;
	{SG_TEXTURE_SWITCH}
	f_fragColor = color * v_tint;
}}
)";

std::string const s_scaleVsSource = R"(#version 300 es
in vec4 a_position;
in vec2 a_texcoord;

out vec2 v_texcoord;

void main(void) {
	gl_Position = a_position;
	v_texcoord = a_texcoord;
}
)";

std::string const s_scaleFsSource = R"(#version 300 es
precision mediump float;

uniform highp sampler2D s_tex;

in vec2 v_texcoord;

out vec4 f_fragColor;

void main(void) {
	vec3 color = texture(s_tex, v_texcoord).rgb;
	f_fragColor = vec4(color, 1.0);
}
)";

r_stateGL_s::r_stateGL_s(r_renderer_c* renderer)
	: r_api_c(renderer)
{
	// Initialise OpenGL
	openGL = sys_IOpenGL::GetHandle(sys);
	sys_glSet_s set;
	set.bColor = 32;
	set.bDepth = 24;
	set.bStencil = 0;
	set.vsync = true;
	if (openGL->Init(&set)) {
		sys->Error(u8"OpenGL initialisation failed");
	}

	// Get strings
	st_vendor = (const char8_t*)glGetString(GL_VENDOR);
	st_renderer = (const char8_t*)glGetString(GL_RENDERER);
	st_ver = (const char8_t*)glGetString(GL_VERSION);
	st_ext = (const char8_t*)glGetString(GL_EXTENSIONS);

	glGetIntegerv(GL_MAX_TEXTURE_SIZE, (int*)&texMaxDim);
	sys->con->Print(fmt::format(u8"GL_MAX_TEXTURE_SIZE: {}\n", texMaxDim));

	// Set default state
	glClearColor(0.0, 0.0, 0.0, 1.0);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);

	// Load extensions
	sys->con->Print(u8"Loading OpenGL extensions...\n");

	if (st_ext.contains(u8"GL_EXT_texture_compression_s3tc"sv)) {
		sys->con->Print(u8"using GL_EXT_texture_compression_s3tc\n");
	}
	else {
		sys->con->Print(u8"GL_EXT_texture_compression_s3tc not supported\n");
	}

	if (st_ext.contains(u8"GL_EXT_texture_compression_bptc"sv)) {
		sys->con->Print(u8"using GL_EXT_texture_compression_bptc\n");
		texBC7 = true;
	}
	else {
		sys->con->Print(u8"GL_EXT_texture_compression_bptc not supported\n");
		texBC7 = false;
	}

	if (st_ext.contains(u8"GL_EXT_debug_marker"sv)) {
		sys->con->Print(u8"using GL_EXT_debug_marker\n");
	}
	else {
		sys->con->Print(u8"GL_EXT_debug_marker not supported\n");
	}
}

void r_stateGL_s::Init()
{
	ImGui_ImplGlfw_InitForOpenGL((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplOpenGL3_Init("#version 100");

	GLint maxTextureImageUnits{};
	glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, &maxTextureImageUnits);

	// Initialise vertex programs
	{
		GLint success = GL_FALSE;
		GLuint prog = glCreateProgram();
		GLuint vs = glCreateShader(GL_VERTEX_SHADER);
		glShaderSource(vs, 1, &s_tintedTextureVertexSource, nullptr);
		glCompileShader(vs);
		if (!GetShaderCompileSuccess(vs)) {
			const auto log = GetShaderInfoLog(vs);
			sys->Error(u8"Failed to compile vertex shader:\n%s", log.c_str());
		}
		GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
		std::string textureSwitch;
		{
			fmt::memory_buffer buf;
			for (size_t i = 0; i < maxTextureImageUnits; ++i) {
				if (i == 0) {
					fmt::format_to(fmt::appender(buf), "if (v_texId.x < {}.5) ", i);
				}
				else if (i == maxTextureImageUnits - 1) {
					fmt::format_to(fmt::appender(buf), "else ");
				}
				else {
					fmt::format_to(fmt::appender(buf), "else if (v_texId.x < {}.5)", i);
				}
				fmt::format_to(fmt::appender(buf), R"( {{
	color = texture(s_tex[{}], vec3(v_texcoord, v_texId.y));
	if (v_texId.z > -0.5)
		color *= texture(s_tex[{}], vec3(v_texcoord, v_texId.z));
}}
)", i, i);
			}
			textureSwitch = to_string(buf);
		}
		std::string fragSource = fmt::format(s_tintedTextureFragmentTemplate,
			fmt::arg("SG_TEXTURE_COUNT", maxTextureImageUnits),
			fmt::arg("SG_TEXTURE_SWITCH", textureSwitch));
		char const* fragSourcePtr = fragSource.c_str();
		glShaderSource(fs, 1, &fragSourcePtr, nullptr);
		glCompileShader(fs);
		if (!GetShaderCompileSuccess(fs)) {
			const auto log = GetShaderInfoLog(fs);
			sys->Error(u8"Failed to compile fragment shader:\n%s", log.c_str());
		}

		glAttachShader(prog, vs);
		glAttachShader(prog, fs);
		glLinkProgram(prog);
		if (!GetProgramLinkSuccess(prog)) {
			const auto log = GetProgramInfoLog(prog);
			sys->Error(u8"Failed to link program:\n%s", log.c_str());
		}
		glDeleteShader(vs);
		glDeleteShader(fs);
		tintedTextureProgram = prog;
	}

	// Set up DPI-scaling render target
	for (int i = 0; i < 2; ++i) {
		auto& rtt = rttMain[i];
		if (i > 0) {
			rtt = rttMain[0]; // Reuse shared parts like dimensions and program/locations.
		}
		glGenFramebuffers(1, &rtt.framebuffer);
		glGenTextures(1, &rtt.colorTexture);

		if (i == 0) {
			auto compileShader = [](std::string_view src, GLenum type) -> GLuint {
				GLuint id = glCreateShader(type);
				auto sourcePtr = src.data();
				glShaderSource(id, 1, &sourcePtr, nullptr);
				glCompileShader(id);
				return id;
				};

			auto vsId = compileShader(s_scaleVsSource, GL_VERTEX_SHADER);
			if (!GetShaderCompileSuccess(vsId)) {
				const auto log = GetShaderInfoLog(vsId);
				sys->con->Print(fmt::format(u8"Scaling VS compile failure: {}\n", log));
			}
			auto fsId = compileShader(s_scaleFsSource, GL_FRAGMENT_SHADER);
			if (!GetShaderCompileSuccess(fsId)) {
				const auto log = GetShaderInfoLog(fsId);
				sys->con->Print(fmt::format(u8"Scaling FS compile failure: {}\n", log));
			}

			GLuint prog = rtt.blitProg = glCreateProgram();
			glAttachShader(prog, vsId);
			glAttachShader(prog, fsId);
			glLinkProgram(prog);
			if (!GetProgramLinkSuccess(prog)) {
				const auto log = GetProgramInfoLog(prog);
				sys->con->Print(fmt::format(u8"Scaling program link failure: {}\n", log));
			}

			GLint linked = GL_FALSE;
			glGetProgramiv(prog, GL_LINK_STATUS, &linked);

			glDeleteShader(vsId);
			glDeleteShader(fsId);

			rtt.blitAttribLocPos = glGetAttribLocation(prog, "a_position");
			rtt.blitAttribLocTC = glGetAttribLocation(prog, "a_texcoord");
			rtt.blitSampleLocColour = glGetUniformLocation(prog, "s_tex");
		}
	}
}

void r_stateGL_s::Shutdown()
{
	for (int i = 0; i < 2; ++i) {
		auto& rtt = rttMain[i];
		glDeleteTextures(1, &rtt.colorTexture);
		glDeleteFramebuffers(1, &rtt.framebuffer);
	}
	glDeleteProgram(rttMain[0].blitProg);

	ImGui_ImplOpenGL3_Shutdown();
	ImGui_ImplGlfw_Shutdown();
}

void r_stateGL_s::ImGuiBeginFrame()
{
	ImGui_ImplOpenGL3_NewFrame();
	ImGui_ImplGlfw_NewFrame();
}

void r_stateGL_s::ImGuiEndFrame()
{
	ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
}

void r_stateGL_s::BeginFrame()
{
	// TODO(zao): Move all of this to a function that deals with resolution-dependent resources?

	auto& vid = sys->video->vid;
	int wNew = renderer->VirtualScreenWidth();
	int hNew = renderer->VirtualScreenHeight();
	bool const wantIntegerScaling = fmodf(vid.dpiScale, 1.0f) < 0.0005f;
	for (int i = 0; i < 2; ++i) {
		auto& rtt = rttMain[i];
		if (rtt.width != wNew || rtt.height != hNew) {
			GLint prevTex2D, prevFB;
			glGetIntegerv(GL_TEXTURE_BINDING_2D, &prevTex2D);
			glGetIntegerv(GL_FRAMEBUFFER_BINDING, &prevFB);
			glBindTexture(GL_TEXTURE_2D, rtt.colorTexture);
			glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, wNew, hNew, 0, GL_RGB, GL_UNSIGNED_BYTE, nullptr);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
			GLint const filterMode = wantIntegerScaling ? GL_NEAREST : GL_LINEAR;
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filterMode);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filterMode);

			rtt.width = wNew;
			rtt.height = hNew;

			glBindFramebuffer(GL_FRAMEBUFFER, rtt.framebuffer);
			glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, rtt.colorTexture, 0);

			glCheckFramebufferStatus(GL_FRAMEBUFFER);

			glBindFramebuffer(GL_FRAMEBUFFER, prevFB);
			glBindTexture(GL_TEXTURE_2D, prevTex2D);
		}
	}
}

void r_stateGL_s::EndFrame()
{
}

void r_stateGL_s::PrepareDrawTarget()
{
	glBindFramebuffer(GL_FRAMEBUFFER, rttMain[renderer->GetDrawRenderTarget()].framebuffer);
	glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
}

void r_stateGL_s::DrawPresentTarget()
{
	auto& rtt = rttMain[renderer->GetPresentRenderTarget()];
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	float blitTriPos[] = {
		-1.0f, -1.0f, //
		3.0f, -1.0f, //
		-1.0f, 3.0f, //
	};
	float blitTriUV[] = {
		0.0f, 0.0f, //
		2.0f, 0.0f, //
		0.0f, 2.0f, //
	};

	glViewport(0, 0, sys->video->vid.fbSize[0], sys->video->vid.fbSize[1]);
	glUseProgram(rtt.blitProg);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, std::data(blitTriPos));
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 0, std::data(blitTriUV));
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glBindTexture(GL_TEXTURE_2D, rtt.colorTexture);
	glUniform1i(rtt.blitSampleLocColour, 0);
	glDrawArrays(GL_TRIANGLES, 0, 3);
	glBindTexture(GL_TEXTURE_2D, 0);
	glUseProgram(0);
}
