#include "r_local.h"

#define GLAD_GLES2_IMPLEMENTATION
#include <glad/gles2.h>
#include <glad/egl.h>

#include <imgui_impl_glfw.h>
#include <imgui_impl_opengl3.h>

#include <gli/gl.hpp>

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

	// Get strings
	impl->st_vendor = (const char8_t*)glGetString(GL_VENDOR);
	impl->st_renderer = (const char8_t*)glGetString(GL_RENDERER);
	impl->st_ver = (const char8_t*)glGetString(GL_VERSION);
	impl->st_ext = (const char8_t*)glGetString(GL_EXTENSIONS);

	glGetIntegerv(GL_MAX_TEXTURE_SIZE, (int*)&texMaxDim);
	sys->con->Print(fmt::format(u8"GL_MAX_TEXTURE_SIZE: {}\n", texMaxDim));

	// Set default state
	glClearColor(0.0, 0.0, 0.0, 1.0);
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);

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
	float x, y;
	float u, v;
	float r, g, b, a;
	float viewX, viewY, viewW, viewH;
	float texId, stackIdx, maskIdx;
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
	GLint viewportAttr;
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
	viewportAttr = glGetAttribLocation(prog, "a_viewport");
	texIdAttr = glGetAttribLocation(prog, "a_texId");
}

Batch::Batch(Batch&& rhs)
	: prog(rhs.prog)
	, xyAttr(rhs.xyAttr)
	, uvAttr(rhs.uvAttr)
	, tintAttr(rhs.tintAttr)
	, viewportAttr(rhs.viewportAttr)
	, texIdAttr(rhs.texIdAttr)
	, vertices(std::move(rhs.vertices))
{
}

Batch& Batch::operator = (Batch&& rhs) {
	prog = rhs.prog;
	xyAttr = rhs.xyAttr;
	uvAttr = rhs.uvAttr;
	tintAttr = rhs.tintAttr;
	viewportAttr = rhs.viewportAttr;
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
	glVertexAttribPointer(xyAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, x));
	glVertexAttribPointer(uvAttr, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, u));
	glVertexAttribPointer(tintAttr, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, r));
	glVertexAttribPointer(viewportAttr, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, viewX));
	glVertexAttribPointer(texIdAttr, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void const*)offsetof(Vertex, texId));
	glEnableVertexAttribArray(xyAttr);
	glEnableVertexAttribArray(uvAttr);
	glEnableVertexAttribArray(tintAttr);
	glEnableVertexAttribArray(viewportAttr);
	glEnableVertexAttribArray(texIdAttr);
	glDrawArrays(GL_TRIANGLES, 0, (GLsizei)vertices.size());
	glDisableVertexAttribArray(xyAttr);
	glDisableVertexAttribArray(uvAttr);
	glDisableVertexAttribArray(tintAttr);
	glDisableVertexAttribArray(viewportAttr);
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
		mvpMatrixLoc_ = glGetUniformLocation(prog_, "mvp_matrix");
		batchTextureCap_ = texLocs_.size();
		glGenBuffers(1, &vbo_);
	}

	~AdjacentMergeStrategy() {
		glDeleteBuffers(1, &vbo_);
	}

	struct BatchKey {
		int blendMode = -1;

		bool operator < (BatchKey const& rhs) const {
			return blendMode < rhs.blendMode;
		}

		bool operator == (BatchKey const& rhs) const {
			return !(*this < rhs) && !(rhs < *this);
		}

		bool operator != (BatchKey const& rhs) const {
			return !(*this == rhs);
		}
	};

	void ProcessCommand(r_layerCmd_s* cmd) override {
		switch (cmd->cmd) {
		case r_layerCmd_s::VIEWPORT: {
			auto* c = (r_layerCmdViewport_s*)cmd;
			nextViewport_ = c->viewport;
			if (showStats_) {
				// ImGui::Text("VIEWPORT: %dx%d @ %d,%d", c->viewport.width, c->viewport.height, c->viewport.x, c->viewport.y);
			}
		} break;
		case r_layerCmd_s::BLEND: {
			auto* c = (r_layerCmdBlend_s*)cmd;
			latchKey_.blendMode = c->blendMode;
			if (showStats_) {
				// const auto blendModeName = magic_enum::enum_name((r_blendMode_e)c->blendMode);
				// ImGui::Text("BLEND: %.*s", (int)blendModeName.size(), blendModeName.data());
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
			std::copy_n(c->col, 4, tint_.data());
		} break;
		case r_layerCmd_s::QUAD: {
			auto* c = (r_layerCmdQuad_s*)cmd;
			if (showStats_) {
				// ImGui::Text("QUAD");
			}

			// Cull the quad first before it influences any boundary cuts.
			if (!!renderer_->r_drawCull->intVal) {
				auto a = AabbOffset(AabbFromCmdQuad(c->quad), nextViewport_.lo);
				auto b = AabbFromViewport(nextViewport_);
				bool intersects = AabbAabbIntersects(a, b);
				if (!intersects) {
					break;
				}
			}

			// If the current batch is incompatible key-wise, dispatch it to get a fresh
			// batch to grow in.
			if (!batch_.batch.vertices.empty() && batch_.key != latchKey_) {
				Dispatch();
			}
			batch_.key = latchKey_;

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
				q.u = c->quad.s[v];
				q.v = c->quad.t[v];
				q.x = c->quad.x[v];
				q.y = c->quad.y[v];
				q.r = tint_[0];
				q.g = tint_[1];
				q.b = tint_[2];
				q.a = tint_[3];
				q.viewX = (float)vp.lo.x;
				q.viewY = (float)vp.lo.y;
				q.viewW = (float)vp.extent.x;
				q.viewH = (float)vp.extent.y;
				q.texId = (float)texSlot;
				q.stackIdx = (float)c->quad.stackLayer;
				q.maskIdx = (float)c->quad.maskLayer;
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

		auto& key = batch_.key;
		auto& lastKey = lastDispatchKey_;

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
			r_mat4_s mvpMatrix = OrthoMatrix(0, virtualW, virtualH, 0, -9999, 9999);
			glUniformMatrix4fv(mvpMatrixLoc_, 1, GL_FALSE, mvpMatrix.data());
		}
		if (!lastKey || lastKey->blendMode != key.blendMode) {
			if (showStats_) {
				const auto blendModeName = magic_enum::enum_name((r_blendMode_e)key.blendMode);
				ImGui::Text("New blend mode %.*s", (int)blendModeName.size(), blendModeName.data());
			}
			switch (key.blendMode) {
			case RB_ALPHA:
				glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
				break;
			case RB_PRE_ALPHA:
				glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
				break;
			case RB_ADDITIVE:
				glBlendFunc(GL_ONE, GL_ONE);
				break;
			}
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

		lastDispatchKey_ = key;
		batch_.batch.vertices.clear();
		batch_.textures.clear();

		glUseProgram(0);

		batchIndex += 1;
	}

	const r_layer_c* layer_{};
	r_renderer_c* renderer_{};
	GLuint prog_{};
	std::vector<GLint> texLocs_;
	GLint mvpMatrixLoc_{};

	size_t batchTextureCap_{};
	GLuint vbo_{};

	struct TexturedBatch {
		explicit TexturedBatch(GLuint prog) : batch(prog) {
			textures.reserve(128);
		}

		BatchKey key{};
		Batch batch;
		std::vector<r_tex_c*> textures;
	};

	BatchKey latchKey_{};
	r_viewport_s nextViewport_{};
	r_tex_c* nextTex_{};
	std::optional<BatchKey> lastDispatchKey_;
	TexturedBatch batch_;

	std::array<float, 4> tint_{1.0f, 1.0f, 1.0f, 1.0f};

	size_t totalVertexCount_ = 0;
	size_t batchIndex = 0;

	bool usedIncompleteTextures = false;
};

std::shared_ptr<r_IRenderStrategy> r_stateGL_s::GetRenderStrategy(const r_layer_c& layer)
{
	return std::make_shared<AdjacentMergeStrategy>(&layer, renderer, impl->tintedTextureProgram);
}
