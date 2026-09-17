// Module: Render OpenGL

#include "r_local.h"

std::tuple<const GLchar*, GLint> ResourceViewOpenGL(cmrc::file file)
{
	return {(const GLchar*)file.begin(), (GLint)file.size()};
}

std::string_view ResourceStringView(cmrc::file file)
{
	return std::string_view(file.begin(), file.size());
}

OpenGLShader::OpenGLShader(GLenum kind)
	: OpenGLObjectBase(glCreateShader(kind)), kind(kind)
{
}

OpenGLShader::~OpenGLShader()
{
	glDeleteShader(id);
}

OpenGLShader::OpenGLShader(OpenGLShader&& rhs) noexcept
	: OpenGLObjectBase(std::move(rhs)), kind(kind)
{
}

void OpenGLShader::CompileShader()
{
	glCompileShader(id);
}

[[nodiscard]]
bool OpenGLShader::GetCompileSuccess() const noexcept
{
	GLint success{};
	glGetShaderiv(id, GL_COMPILE_STATUS, &success);
	return success == GL_TRUE;
}

[[nodiscard]]
std::string OpenGLShader::GetInfoLog() const
{
	GLint len{};
	glGetShaderiv(id, GL_INFO_LOG_LENGTH, &len);
	std::vector<char> msg(len);
	glGetShaderInfoLog(id, (GLsizei)msg.size(), &len, msg.data());
	return std::string(msg.data(), msg.data() + len);
}

void OpenGLShader::ShaderSource(cmrc::file file)
{
	auto [sourceText, sourceLen] = ResourceViewOpenGL(file);
	glShaderSource(id, 1, &sourceText, &sourceLen);
}

void OpenGLShader::ShaderSource(std::string_view source)
{
	const auto sourceText = (const GLchar*)source.data();
	const auto sourceLen = (GLint)source.size();
	glShaderSource(id, 1, &sourceText, &sourceLen);
}

OpenGLProgram::OpenGLProgram()
	: OpenGLObjectBase(glCreateProgram())
{}

OpenGLProgram::~OpenGLProgram()
{
	glDeleteProgram(id);
}

void OpenGLProgram::Use() const noexcept
{
	glUseProgram(id);
}

[[nodiscard]]
OpenGLProgram::UseScope OpenGLProgram::ScopedUse() const noexcept
{
	auto ret = UseScope{};
	glUseProgram(id);
	return ret;
}

void OpenGLProgram::AttachShader(const OpenGLShader& shader) const noexcept
{
	glAttachShader(id, shader.GetId());
}

void OpenGLProgram::AttachShader(GLuint shaderId) const noexcept
{
	glAttachShader(id, shaderId);
}

void OpenGLProgram::LinkProgram() const noexcept
{
	glLinkProgram(id);
}

[[nodiscard]]
bool OpenGLProgram::GetLinkSuccess() const noexcept
{
	GLint success{};
	glGetProgramiv(id, GL_LINK_STATUS, &success);
	return success == GL_TRUE;
}

[[nodiscard]]
std::string OpenGLProgram::GetInfoLog() const
{
	GLint len{};
	glGetProgramiv(id, GL_INFO_LOG_LENGTH, &len);
	std::vector<char> msg(len);
	glGetProgramInfoLog(id, (GLsizei)msg.size(), &len, msg.data());
	return std::string(msg.data(), msg.data() + len);
}

[[nodiscard]]
std::optional<GLint> OpenGLProgram::GetAttribLocation(const std::string& name) const noexcept
{
	const auto ret = glGetAttribLocation(id, (const GLchar*)name.c_str());
	if (ret == -1) {
		return std::nullopt;
	}
	return ret;
}

[[nodiscard]]
std::optional<GLint> OpenGLProgram::GetUniformLocation(const std::string& name) const noexcept
{
	const auto ret = glGetUniformLocation(id, (const GLchar*)name.c_str());
	if (ret == -1) {
		return std::nullopt;
	}
	return ret;
}

OpenGLProgram::UseScope::UseScope()
{
	glGetIntegerv(GL_CURRENT_PROGRAM, &prevUse);
}

OpenGLProgram::UseScope::~UseScope()
{
	glUseProgram((GLuint)prevUse);
}

OpenGLBuffer::OpenGLBuffer(GLenum target)
	: OpenGLObjectBase(CreateObject()), target(target)
{}

OpenGLBuffer::~OpenGLBuffer()
{
	glDeleteBuffers(1, &id);
}

GLenum OpenGLBuffer::Target() const noexcept
{
	return target;
}

void OpenGLBuffer::Bind() const noexcept
{
	glBindBuffer(target, id);
}

[[nodiscard]]
OpenGLBuffer::BindScope OpenGLBuffer::ScopedBind() const noexcept
{
	auto ret = BindScope(target);
	Bind();
	return ret;
}

GLuint OpenGLBuffer::CreateObject()
{
	GLuint id{};
	glGenBuffers(1, &id);
	return id;
}

static const std::flat_map<GLenum, GLenum> bindingGetEnum = {
	{GL_ARRAY_BUFFER, GL_ARRAY_BUFFER_BINDING},
	{GL_ELEMENT_ARRAY_BUFFER, GL_ELEMENT_ARRAY_BUFFER_BINDING},
	{GL_UNIFORM_BUFFER, GL_UNIFORM_BUFFER_BINDING},
};

OpenGLBuffer::BindScope::BindScope(GLenum target)
{
	glGetIntegerv(bindingGetEnum.at(target), &prevBind);
}

OpenGLBuffer::BindScope::~BindScope()
{
	glBindBuffer(target, prevBind);
}
