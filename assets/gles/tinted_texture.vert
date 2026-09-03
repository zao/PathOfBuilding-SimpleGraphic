#version 300 es

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
