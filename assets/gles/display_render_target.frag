#version 300 es
precision mediump float;

uniform highp sampler2D s_tex;

in vec2 v_texcoord;

out vec4 f_fragColor;

void main(void) {
	vec3 color = texture(s_tex, v_texcoord).rgb;
	f_fragColor = vec4(color, 1.0);
}
