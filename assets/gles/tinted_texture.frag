#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];

in vec2 v_screenPos;
in vec2 v_texcoord;
in vec4 v_tint;
in vec2 v_texId;
in vec2 v_clipMin;
in vec2 v_clipMax;

out vec4 f_fragColor;

void main(void)
{{
	if (any(lessThan(v_screenPos, v_clipMin)) ||
		any(greaterThanEqual(v_screenPos, v_clipMax)))
	{{
		discard;
	}}
	vec4 color;
	{SG_TEXTURE_SWITCH}
	f_fragColor = color * v_tint;
}}
