#include "r_local.h"

#define GLAD_GLES2_IMPLEMENTATION
#include <glad/gles2.h>
#include <glad/egl.h>
#include <GLFW/glfw3.h>

#include <cmrc/cmrc.hpp>
#include <imgui_impl_glfw.h>
#include <imgui_impl_opengl3.h>

#include <gli/gl.hpp>

CMRC_DECLARE(SimpleGraphic);

struct TexDataGL
{
	dword texId{};	// GLint
	dword target{};	// GLenum

	~TexDataGL()
	{
		if (texId)
			glDeleteTextures(1, &texId);
	}

	TexDataGL& operator = (const TexDataGL&) = delete;
};

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

struct r_stateGL_s::Impl
{
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

r_stateGL_s::r_stateGL_s(r_renderer_c* renderer)
	: r_api_c(renderer)
{
	impl = std::make_shared<Impl>();
	// Initialise OpenGL
	impl->openGL = sys_IOpenGL::GetHandle(sys);
	sys_glSet_s set;
	set.bColor = 32;
	set.bDepth = 24;
	set.bStencil = 0;
	set.vsync = true;
	if (impl->openGL->Init(&set)) {
		sys->Error(u8"OpenGL initialisation failed");
	}

	// Set default state
	glClearColor(0.0, 0.0, 0.0, 1.0);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);

	// Clear early to avoid flash
	glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
	impl->openGL->Swap();

	// Get strings
	impl->st_vendor = (const char8_t*)glGetString(GL_VENDOR);
	impl->st_renderer = (const char8_t*)glGetString(GL_RENDERER);
	impl->st_ver = (const char8_t*)glGetString(GL_VERSION);
	impl->st_ext = (const char8_t*)glGetString(GL_EXTENSIONS);

	glGetIntegerv(GL_MAX_TEXTURE_SIZE, (int*)&texMaxDim);
	sys->con->Print(fmt::format(u8"GL_MAX_TEXTURE_SIZE: {}\n", texMaxDim));

	// Load extensions
	sys->con->Print(u8"Loading OpenGL extensions...\n");

	if (impl->st_ext.contains(u8"GL_EXT_texture_compression_s3tc"sv)) {
		sys->con->Print(u8"using GL_EXT_texture_compression_s3tc\n");
	}
	else {
		sys->con->Print(u8"GL_EXT_texture_compression_s3tc not supported\n");
	}

	if (impl->st_ext.contains(u8"GL_EXT_texture_compression_bptc"sv)) {
		sys->con->Print(u8"using GL_EXT_texture_compression_bptc\n");
		texBC7 = true;
	}
	else {
		sys->con->Print(u8"GL_EXT_texture_compression_bptc not supported\n");
		texBC7 = false;
	}

	if (impl->st_ext.contains(u8"GL_EXT_debug_marker"sv)) {
		sys->con->Print(u8"using GL_EXT_debug_marker\n");
	}
	else {
		sys->con->Print(u8"GL_EXT_debug_marker not supported\n");
	}
}

static std::tuple<const GLchar*, GLint> ResourceViewOpenGL(cmrc::file file)
{
	return {(const GLchar*)file.begin(), (GLint)file.size()};
}

void r_stateGL_s::Init()
{
	const auto resources = cmrc::SimpleGraphic::get_filesystem();

	ImGui_ImplGlfw_InitForOpenGL((GLFWwindow*)sys->video->GetWindowHandle(), true);
	ImGui_ImplOpenGL3_Init("#version 100");

	GLint maxTextureImageUnits{};
	glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, &maxTextureImageUnits);

	// Initialise vertex programs
	{
		GLint success = GL_FALSE;
		GLuint prog = glCreateProgram();
		GLuint vs = glCreateShader(GL_VERTEX_SHADER);
		auto [tintVertText, tintVertLen] = ResourceViewOpenGL(resources.open("assets/gles/tinted_texture.vert"));
		glShaderSource(vs, 1, &tintVertText, &tintVertLen);
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
}}
)", i, i);
			}
			textureSwitch = to_string(buf);
		}
		const auto fragTemplate = std::string_view(resources.open("assets/gles/tinted_texture.frag"));
		try {
			std::string fragSource = fmt::format(fmt::runtime(fragTemplate),
				fmt::arg("SG_TEXTURE_COUNT", maxTextureImageUnits),
				fmt::arg("SG_TEXTURE_SWITCH", textureSwitch));
			char const* fragSourcePtr = fragSource.c_str();
			glShaderSource(fs, 1, &fragSourcePtr, nullptr);
		}
		catch (std::exception& e) {
			sys->Error(u8"Failed to format fragment shader:\n%s", e.what());
		}
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
		impl->tintedTextureProgram = prog;
	}

	// Set up DPI-scaling render target
	for (int i = 0; i < 2; ++i) {
		auto& rtt = impl->rttMain[i];
		if (i > 0) {
			rtt = impl->rttMain[0]; // Reuse shared parts like dimensions and program/locations.
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

			auto vsId = compileShader(std::string_view(resources.open("assets/gles/display_render_target.vert")), GL_VERTEX_SHADER);
			if (!GetShaderCompileSuccess(vsId)) {
				const auto log = GetShaderInfoLog(vsId);
				sys->con->Print(fmt::format(u8"Scaling VS compile failure: {}\n", log));
			}
			auto fsId = compileShader(std::string_view(resources.open("assets/gles/display_render_target.frag")), GL_FRAGMENT_SHADER);
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
		auto& rtt = impl->rttMain[i];
		glDeleteTextures(1, &rtt.colorTexture);
		glDeleteFramebuffers(1, &rtt.framebuffer);
	}
	glDeleteProgram(impl->rttMain[0].blitProg);

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
		auto& rtt = impl->rttMain[i];
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
	// Swap output buffers
	impl->openGL->Swap();
}

void r_stateGL_s::PrepareDrawTarget()
{
	glBindFramebuffer(GL_FRAMEBUFFER, impl->rttMain[renderer->GetDrawRenderTarget()].framebuffer);
	const auto clear = renderer->clearColor;
	glClearColor(clear.r, clear.g, clear.b, clear.a);
	glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
}

void r_stateGL_s::DrawPresentTarget()
{
	auto& rtt = impl->rttMain[renderer->GetPresentRenderTarget()];
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

std::shared_ptr<void> r_stateGL_s::ScopedDebugMarker(std::u8string_view label)
{
	std::shared_ptr<void> ret;
	if (glPushGroupMarkerEXT && glPopGroupMarkerEXT)
	{
		glPushGroupMarkerEXT(label.size(), (const GLchar*)label.data());
		ret = std::shared_ptr<void>(nullptr, [](void*) {
			glPopGroupMarkerEXT();
		});
	}
	return ret;
}

bool r_stateGL_s::DoScreenshot(image_c& outImg, int type)
{
	if (type != IMGTYPE_RGB) {
		return false;
	}

	auto& rt = impl->rttMain[renderer->GetPresentRenderTarget()];
	int const xs = rt.width;
	int const ys = rt.height;

	// Pixel reading only supports RGBA and an implementation-specific format.
	// Use RGBA for convenience as that's close enough to what we want to save in the end.
	int const readSize = xs * ys * 4;
	int const writeSize = xs * ys * 3;
	std::vector<byte> sbuf(readSize);

	// Read the front buffer
	GLint oldFb{};
	GLenum oglErr = glGetError();
	GLenum implColorReadFormat{}, implColorReadType{};
	glGetIntegerv(GL_FRAMEBUFFER_BINDING, &oldFb);
	glBindFramebuffer(GL_FRAMEBUFFER, rt.framebuffer);
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glReadPixels(0, 0, xs, ys, GL_RGBA, GL_UNSIGNED_BYTE, sbuf.data());
	oglErr = glGetError();
	glBindFramebuffer(GL_FRAMEBUFFER, oldFb);

	// Flip and convert the image to RGB
	int const readSpan = xs * 4;
	int	const writeSpan = xs * 3;
	std::vector<byte> ss(writeSize);
	byte* p1 = sbuf.data();
	byte* p2 = ss.data() + writeSize - writeSpan;
	for (int y = 0; y < ys; ++y, p2 -= writeSpan * 2) {
		for (int x = 0; x < xs; ++x) {
			*p2++ = *p1++; // R
			*p2++ = *p1++; // G
			*p2++ = *p1++; // B
			p1++; // A
		}
	}
	sbuf.clear();

	// Set image info
	outImg.CopyRaw(IMGTYPE_RGB, xs, ys, ss.data());
	return true;
}

std::shared_ptr<void> r_stateGL_s::UploadTextureData(r_tex_c* src)
{
	auto data = std::make_shared<TexDataGL>();

	static gli::gl gl(gli::gl::PROFILE_ES30);

	auto& target = data->target;
	auto& texId = data->texId;

	const auto& tex = src->img->tex;
	target = gl.translate(tex.target());
	const auto format = gl.translate(tex.format(), tex.swizzles());

	// Find and bind texture name
	glGenTextures(1, &texId);
	glBindTexture(target, texId);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glTexParameteri(target, GL_TEXTURE_BASE_LEVEL, 0);
	glTexParameteri(target, GL_TEXTURE_MAX_LEVEL, (GLint)tex.levels());
	glTexParameteri(target, GL_TEXTURE_SWIZZLE_R, format.Swizzles.r);
	glTexParameteri(target, GL_TEXTURE_SWIZZLE_G, format.Swizzles.g);
	glTexParameteri(target, GL_TEXTURE_SWIZZLE_B, format.Swizzles.b);
	glTexParameteri(target, GL_TEXTURE_SWIZZLE_A, format.Swizzles.a);

	const int miplevels = (int)tex.levels();

	// Set filters
	if (miplevels == 1) {
		glTexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	}
	else {
		glTexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	}
	if (src->flags & TF_NEAREST) {
		glTexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	}
	else {
		glTexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	}

	constexpr float anisotropyCap = 16.0f;
	static const float maxAnisotropy = [] {
		float ret{};
		glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &ret);
		return ret;
		}();
	glTexParameterf(target, GL_TEXTURE_MAX_ANISOTROPY, (std::min)(maxAnisotropy, anisotropyCap));

	// Set repeating
	if (src->flags & TF_CLAMP) {
		glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	}
	else {
		glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_REPEAT);
	}

	const int layers = (int)tex.layers();
	const auto extent = tex.extent();
	const bool isTextureArray = target == GL_TEXTURE_2D_ARRAY;

	if (isTextureArray)
		glTexStorage3D(target, miplevels, format.Internal, extent.x, extent.y, layers);
	else
		glTexStorage2D(target, miplevels, format.Internal, extent.x, extent.y);

	for (int layer = 0; layer < layers; ++layer) {
		for (int miplevel = 0; miplevel < miplevels; ++miplevel) {

			const auto extent = tex.extent(miplevel);

			const int up_w = extent.x;
			const int up_h = extent.y;

			// Upload the mipmap
			const auto* data = tex.data(layer, 0, miplevel);
			if (is_compressed(tex.format()))
				if (isTextureArray)
					glCompressedTexSubImage3D(target, miplevel, 0, 0, layer, extent.x, extent.y, 1, format.Internal, (GLsizei)tex.size(miplevel), data);
				else
					glCompressedTexSubImage2D(target, miplevel, 0, 0, extent.x, extent.y, format.Internal, (GLsizei)tex.size(miplevel), data);
			else
				if (isTextureArray)
					glTexSubImage3D(target, miplevel, 0, 0, layer, extent.x, extent.y, 1, format.External, format.Type, data);
				else
					glTexSubImage2D(target, miplevel, 0, 0, extent.x, extent.y, format.External, format.Type, data);
		}
	}
	return data;
}

// =================
// Batched Rendering
// =================

struct Vertex {
	glm::vec2 pos;
	glm::vec2 uv;
	glm::vec4 color;
	glm::vec2 texId; // texId, stackIdx
};

struct Batch {
	explicit Batch(GLuint prog);
	Batch(Batch&& rhs);
	Batch& operator = (Batch&& rhs);
	Batch(Batch const&) = delete;
	Batch& operator = (Batch const&) = delete;
	~Batch();

	GLuint prog;
	GLint xyAttr;
	GLint uvAttr;
	GLint tintAttr;
	GLint texIdAttr;

	std::vector<Vertex> vertices;

	void Execute(GLuint sharedVbo, size_t vertexBase);
};

Batch::Batch(GLuint prog)
	: prog(prog)
{
	xyAttr = glGetAttribLocation(prog, "a_vertex");
	uvAttr = glGetAttribLocation(prog, "a_texcoord");
	tintAttr = glGetAttribLocation(prog, "a_tint");
	texIdAttr = glGetAttribLocation(prog, "a_texId");
}

Batch::Batch(Batch&& rhs)
	: prog(rhs.prog)
	, xyAttr(rhs.xyAttr)
	, uvAttr(rhs.uvAttr)
	, tintAttr(rhs.tintAttr)
	, texIdAttr(rhs.texIdAttr)
	, vertices(std::move(rhs.vertices))
{
}

Batch& Batch::operator = (Batch&& rhs) {
	prog = rhs.prog;
	xyAttr = rhs.xyAttr;
	uvAttr = rhs.uvAttr;
	tintAttr = rhs.tintAttr;
	texIdAttr = rhs.texIdAttr;
	vertices = std::move(rhs.vertices);

	return *this;
}

Batch::~Batch() {}

void Batch::Execute(GLuint sharedVbo, size_t vertexBase)
{
	if (vertices.empty()) {
		return;
	}

	glBindBuffer(GL_ARRAY_BUFFER, sharedVbo);
	auto dataPtr = (uint8_t const*)vertices.data();
	auto dataOff = vertexBase * sizeof(Vertex);
	auto dataSize = vertices.size() * sizeof(Vertex);
	glBufferSubData(GL_ARRAY_BUFFER, dataOff, dataSize, dataPtr);
	glVertexAttribPointer(xyAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, pos));
	glVertexAttribPointer(uvAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, uv));
	glVertexAttribPointer(tintAttr, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, color));
	glVertexAttribPointer(texIdAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, texId));
	glEnableVertexAttribArray(xyAttr);
	glEnableVertexAttribArray(uvAttr);
	glEnableVertexAttribArray(tintAttr);
	glEnableVertexAttribArray(texIdAttr);
	glDrawArrays(GL_TRIANGLES, 0, (GLsizei)vertices.size());
	glDisableVertexAttribArray(xyAttr);
	glDisableVertexAttribArray(uvAttr);
	glDisableVertexAttribArray(tintAttr);
	glDisableVertexAttribArray(texIdAttr);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	vertices.clear();
}

struct AdjacentMergeStrategy : r_IRenderStrategy {
	AdjacentMergeStrategy(const r_layer_c* layer, r_renderer_c* renderer, GLuint prog)
		: layer_(layer), renderer_(renderer), prog_(prog), batch_(prog)
	{
		for (size_t i = 0;; ++i) {
			GLint loc = glGetUniformLocation(prog, fmt::format("s_tex[{}]", i).c_str());
			if (loc == -1) {
				break;
			}
			texLocs_.push_back(loc);
		}
		uScreenSizeLoc_ = glGetUniformLocation(prog_, "u_screenSize");
		batchTextureCap_ = texLocs_.size();
		glGenBuffers(1, &vbo_);
	}

	~AdjacentMergeStrategy() {
		glDeleteBuffers(1, &vbo_);
	}

	void ProcessCommand(r_layerCmd_s* cmd) override {
		switch (cmd->cmd) {
		case r_layerCmd_s::VIEWPORT: {
			auto* c = (r_layerCmdViewport_s*)cmd;
			nextViewport_ = c->viewport;
			if (showStats_) {
				// ImGui::Text("VIEWPORT: %dx%d @ %d,%d", c->viewport.width, c->viewport.height, c->viewport.x, c->viewport.y);
			}
		} break;
		case r_layerCmd_s::BIND: {
			auto* c = (r_layerCmdBind_s*)cmd;
			nextTex_ = c->tex;
			if (nextTex_->GetStatus() != r_tex_c::Status::DONE) {
				nextTex_ = nullptr;
			}
			if (showStats_) {
				// ImGui::Text("TEX: %s", c->tex->fileName.c_str());
			}
		} break;
		case r_layerCmd_s::COLOR: {
			auto* c = (r_layerCmdColor_s*)cmd;
			tint_ = glm::make_vec4(c->col);
		} break;
		case r_layerCmd_s::QUAD: {
			auto* c = (r_layerCmdQuad_s*)cmd;
			if (showStats_) {
				// ImGui::Text("QUAD");
			}

			const auto inQ = c->quad;
			// Cull the quad first before it influences any boundary cuts.
			if (!!renderer_->r_drawCull->intVal) {
				const auto [minX, maxX] = std::ranges::minmax(inQ.x);
				if (maxX <= 0.0f || minX >= nextViewport_.extent.x)
					break;
				const auto [minY, maxY] = std::ranges::minmax(inQ.y);
				if (maxY <= 0.0f || minY >= nextViewport_.extent.y)
					break;
			}

			// Refuse to draw geometry if texture isn't loaded as this may lead to UB in the shader.
			if (!nextTex_) {
				usedIncompleteTextures = true;
				break;
			}

			// Check current (and only) batch if the texture set has the latched texture.
			// If it's there, use its index as vertex attribute.
			// If it's not, insert it if room, otherwise dispatch batch and prepare a fresh one.
			size_t texSlot{};
			{
				auto& textures = batch_.textures;
				auto texI = std::find(textures.begin(), textures.end(), nextTex_);
				if (texI == textures.end()) {
					if (textures.size() == batchTextureCap_) {
						Dispatch();
					}
					texI = textures.insert(textures.end(), nextTex_);
				}
				texSlot = std::distance(textures.begin(), texI);
			}

			Vertex quad[4]{};
			for (int v = 0; v < 4; v++) {
				auto& q = quad[v];
				auto& vp = nextViewport_;
				q.uv = {inQ.s[v], inQ.t[v]};
				q.pos = glm::vec2{inQ.x[v], inQ.y[v]} + glm::vec2(vp.lo);
				q.color = tint_;
				q.texId = {(float)texSlot, (float)inQ.stackLayer};
			}
			// 3-2
			// |/|
			// 0-1
			size_t indices[] = {0, 1, 2, 0, 2, 3};
			for (auto idx : indices) {
				batch_.batch.vertices.push_back(quad[idx]);
			}
			totalVertexCount_ += std::size(indices);
		} break;
		}
	}

	void Flush() {
		if (!batch_.batch.vertices.empty()) {
			Dispatch();
		}
		if (showStats_) {
			ImGui::BulletText("Layer %d:%d - %d batches", layer_->id.layer, layer_->id.subLayer, batchIndex);
		}
	}

	bool UsedIncompleteTextures() const override { return usedIncompleteTextures; };

private:
	void Dispatch() {
		glBindBuffer(GL_ARRAY_BUFFER, vbo_);
		auto& batch = batch_.batch;
		auto& textures = batch_.textures;
		size_t vertexCount = batch.vertices.size();
		glBufferData(GL_ARRAY_BUFFER, vertexCount * sizeof(Vertex), nullptr, GL_STREAM_DRAW);
		glUseProgram(prog_);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

		if (showStats_) {
			ImGui::Text("Batch %d", batchIndex);
			ImGui::Text("%d verts", batch.vertices.size());
		}

		{
			auto& vid = renderer_->sys->video->vid;
			float fbScaleX = vid.fbSize[0] / (float)vid.size[0];
			float fbScaleY = vid.fbSize[1] / (float)vid.size[1];
			int virtualW = renderer_->VirtualScreenWidth();
			int virtualH = renderer_->VirtualScreenHeight();
			glViewport(0, 0, virtualW, virtualH);
			glm::vec2 uScreenSize(virtualW, virtualH);
			glUniform2fv(uScreenSizeLoc_, 1, glm::value_ptr(uScreenSize));
		}
		{
			for (size_t i = 0, numTex = texLocs_.size(); i < numTex; ++i) {
				glUniform1i(texLocs_[i], (GLint)i);
				glActiveTexture((GLenum)(GL_TEXTURE0 + i));
				if (i < textures.size()) {
					auto tex = textures[i];
					const auto texData = std::static_pointer_cast<TexDataGL>(tex->apiData);
					glBindTexture((GLenum)texData->target, (GLuint)texData->texId);
					if (showStats_) {
						ImGui::Text("New tex %llX (%s) %d", texData ? (uintptr_t)texData->texId : 0u, tex->fileName.c_str(), tex->status.load());
					}
					if (!usedIncompleteTextures && tex->status != r_tex_c::Status::DONE) {
						usedIncompleteTextures = true;
					}
				}
				else {
					glBindTexture(GL_TEXTURE_2D_ARRAY, 0);
				}
			}
			glActiveTexture(GL_TEXTURE0);
		}

		batch.Execute(vbo_, 0);

		batch_.batch.vertices.clear();
		batch_.textures.clear();

		glUseProgram(0);

		batchIndex += 1;
	}

	const r_layer_c* layer_{};
	r_renderer_c* renderer_{};
	GLuint prog_{};
	std::vector<GLint> texLocs_;
	GLint uScreenSizeLoc_{};

	size_t batchTextureCap_{};
	GLuint vbo_{};

	struct TexturedBatch {
		explicit TexturedBatch(GLuint prog) : batch(prog) {
			textures.reserve(128);
		}

		Batch batch;
		std::vector<r_tex_c*> textures;
	};

	r_viewport_s nextViewport_{};
	r_tex_c* nextTex_{};
	TexturedBatch batch_;

	glm::vec4 tint_{1.0f};

	size_t totalVertexCount_ = 0;
	size_t batchIndex = 0;

	bool usedIncompleteTextures = false;
};

std::shared_ptr<r_IRenderStrategy> r_stateGL_s::GetRenderStrategy(const r_layer_c& layer)
{
	return std::make_shared<AdjacentMergeStrategy>(&layer, renderer, impl->tintedTextureProgram);
}

std::shared_ptr<r_api_c> MakeANGLERendererAPI(r_renderer_c* renderer)
{
	return std::make_shared<r_stateGL_s>(renderer);
}
