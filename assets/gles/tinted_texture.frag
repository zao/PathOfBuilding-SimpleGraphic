#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];

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
