#version 300 es

uniform vec2 u_screenSize;

// Common
in vec2 a_vertex;
in vec2 a_texcoord;
in vec4 a_tint;
in vec2 a_texId;

// Specific
in vec2 a_localCoord;
in vec4 a_startDir_stopDir;
in vec2 a_innerRadius_fullBand;

out vec2 v_screenPos;
out vec2 v_texcoord;
out vec4 v_tint;
out vec2 v_texId;
out vec2 v_localCoord;
out vec4 v_startDir_stopDir;
out vec2 v_innerRadius_fullBand;

vec2 SceneToNdc(vec2 scenePos) {
	vec2 unitPos = scenePos / u_screenSize;
	return mix(vec2(-1.0, 1.0), vec2(1.0, -1.0), unitPos);
}

void main(void)
{
	v_texcoord = a_texcoord;
	v_tint = a_tint;
	v_texId = a_texId;
	v_localCoord = a_localCoord;
    v_startDir_stopDir = a_startDir_stopDir;
    v_innerRadius_fullBand = a_innerRadius_fullBand;
	vec2 pos = SceneToNdc(a_vertex);
	v_screenPos = pos;
	gl_Position = vec4(pos, 0, 1);
}
