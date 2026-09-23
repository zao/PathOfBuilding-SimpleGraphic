#version 300 es

uniform vec2 u_screenSize;

// Common
in vec2 a_vertex;
in vec2 a_texcoord;
in vec4 a_tint;
in vec2 a_texId;
in vec4 a_clipMin_clipMax;

// Specific
in vec2 a_uvLocal;

out vec2 v_screenPos;
out vec2 v_texcoord;
out vec4 v_tint;
out vec2 v_texId;
out vec2 v_clipMin;
out vec2 v_clipMax;
out vec2 v_uvLocal;

vec2 SceneToNdc(vec2 scenePos) {
	vec2 unitPos = scenePos / u_screenSize;
	return mix(vec2(-1.0, 1.0), vec2(1.0, -1.0), unitPos);
}

void main(void)
{
	v_texcoord = a_texcoord;
	v_tint = a_tint;
	v_texId = a_texId;
	v_clipMin = a_clipMin_clipMax.xy;
	v_clipMax = a_clipMin_clipMax.zw;
	v_uvLocal = a_uvLocal;
	vec2 pos = SceneToNdc(a_vertex);
	v_screenPos = a_vertex;
	gl_Position = vec4(pos, 0, 1);
}
