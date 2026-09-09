#version 300 es

uniform vec2 u_screenSize;

in vec2 a_vertex;
in vec2 a_texcoord;
in vec4 a_tint;
in vec2 a_texId;

out vec2 v_screenPos;
out vec2 v_texcoord;
out vec4 v_tint;
out vec2 v_texId;

vec2 SceneToNdc(vec2 scenePos) {
	vec2 unitPos = scenePos / u_screenSize;
	return mix(vec2(-1.0, 1.0), vec2(1.0, -1.0), unitPos);
}

void main(void)
{
	v_texcoord = a_texcoord;
	v_tint = a_tint;
	v_texId = a_texId;
	vec2 pos = SceneToNdc(a_vertex);
	v_screenPos = pos;
	gl_Position = vec4(pos, 0, 1);
}
