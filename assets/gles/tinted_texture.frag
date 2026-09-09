#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];

in vec2 v_screenPos;
in vec2 v_texcoord;
in vec4 v_tint;
in vec2 v_texId;

out vec4 f_fragColor;

void main(void)
{{
	vec4 color;
	{SG_TEXTURE_SWITCH}
	f_fragColor = color * v_tint;
}}
