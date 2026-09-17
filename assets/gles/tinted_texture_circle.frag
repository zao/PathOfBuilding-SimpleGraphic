#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];

in vec2 v_screenPos;
in vec2 v_texcoord;
in vec4 v_tint;
in vec2 v_texId;
in vec2 v_uvLocal;

out vec4 f_fragColor;

bool CullFragment(vec2 unitPos)
{{
	vec2 offCentre = unitPos - 0.5;
	float dist2 = dot(offCentre, offCentre);
	return dist2 > 0.5*0.5;
}}

void main(void)
{{
	if (CullFragment(v_uvLocal)) {{
		discard;
	}}
	vec4 color;
	{SG_TEXTURE_SWITCH}
	f_fragColor = color * v_tint;
}}
