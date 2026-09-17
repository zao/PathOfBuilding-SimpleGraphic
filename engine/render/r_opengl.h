// Render OpenGL Header

#include <cmrc/cmrc.hpp>
#include <flat_map>
#include <span>
#include <tuple>

// =======
// Helpers
// =======

std::tuple<const GLchar*, GLint> ResourceViewOpenGL(cmrc::file file);
std::string_view ResourceStringView(cmrc::file file);

struct OpenGLObjectBase
{
	explicit OpenGLObjectBase(GLuint id) : id(id) {}
	virtual ~OpenGLObjectBase() = default;

	OpenGLObjectBase& operator = (const OpenGLObjectBase& rhs) = delete;
	OpenGLObjectBase(const OpenGLObjectBase& rhs) = delete;
	OpenGLObjectBase(OpenGLObjectBase&& rhs) noexcept : id(rhs.id) { rhs.id = 0; }

	GLuint GetId() const noexcept { return id; }

	GLuint id{};
};

struct OpenGLShader : OpenGLObjectBase
{
	explicit OpenGLShader(GLenum kind);
	~OpenGLShader();
	OpenGLShader(OpenGLShader&& rhs) noexcept;

	void CompileShader();
	[[nodiscard]] bool GetCompileSuccess() const noexcept;
	[[nodiscard]] std::string GetInfoLog() const;
	void ShaderSource(cmrc::file file);
	void ShaderSource(std::string_view source);

	GLenum kind{};
};

struct OpenGLProgram : OpenGLObjectBase
{
	OpenGLProgram();
	~OpenGLProgram();

	OpenGLProgram& operator = (const OpenGLProgram& rhs) = delete;
	OpenGLProgram(const OpenGLProgram& rhs) = delete;

	struct UseScope {
		UseScope();
		~UseScope();

		GLint prevUse{};
	};

	void Use() const noexcept;
	[[nodiscard]] UseScope ScopedUse() const noexcept;

	void AttachShader(const OpenGLShader& shader) const noexcept;
	void AttachShader(GLuint shaderId) const noexcept;
	void LinkProgram() const noexcept;

	[[nodiscard]] bool GetLinkSuccess() const noexcept;
	[[nodiscard]] std::string GetInfoLog() const;
	[[nodiscard]] std::optional<GLint> GetAttribLocation(const std::string& name) const noexcept;
	[[nodiscard]] std::optional<GLint> GetUniformLocation(const std::string& name) const noexcept;
};

struct OpenGLBuffer : OpenGLObjectBase
{
	explicit OpenGLBuffer(GLenum target);
	~OpenGLBuffer();

	OpenGLBuffer& operator = (const OpenGLBuffer& rhs) = delete;
	OpenGLBuffer(const OpenGLBuffer& rhs) = delete;

	GLenum Target() const noexcept;

	struct BindScope {
		BindScope(GLenum target);
		~BindScope();

		GLenum target{};
		GLint prevBind{};
	};

	void Bind() const noexcept;
	[[nodiscard]] BindScope ScopedBind() const noexcept;

	template <typename T>
	void BufferData(size_t count, GLenum usage) const noexcept {
		glBufferData(target, count * sizeof(T), nullptr, usage);
	}

	template <typename T>
	void BufferData(std::span<const T> elems, GLenum usage) const noexcept {
		glBufferData(target, elems.size_bytes(), elems.data(), usage);
	}

	template <typename T>
	void BufferData(std::span<T> elems, GLenum usage) const noexcept {
		glBufferData(target, elems.size_bytes(), elems.data(), usage);
	}

	template <typename T>
	void BufferSubData(std::span<const T> elems, GLintptr dataOff = 0) {
		glBufferSubData(GL_ARRAY_BUFFER, dataOff, elems.size_bytes(), elems.data());
	}

	template <typename T>
	void BufferSubData(std::span<T> elems, GLintptr dataOff = 0) {
		glBufferSubData(GL_ARRAY_BUFFER, dataOff, elems.size_bytes(), elems.data());
	}

	GLenum target{};

private:
	GLuint CreateObject();
};