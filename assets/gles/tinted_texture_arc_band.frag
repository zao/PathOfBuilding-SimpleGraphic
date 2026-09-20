#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];

in vec2 v_screenPos;
in vec2 v_texcoord;
in vec4 v_tint;
in vec2 v_texId;

in vec2 v_localCoord;
in vec4 v_startDir_stopDir;
in vec2 v_innerRadius_fullBand;

out vec4 f_fragColor;

float Cross2(vec2 a, vec2 b) {{
    return a.x*b.y - a.y*b.x;
}}

bool OnArcBand(vec2 localCoord, float rInner, vec2 startDir, vec2 stopDir, bool fullBand)
{{
    float r2 = dot(localCoord, localCoord);
    if (r2 < rInner*rInner) {{
        return false;
    }}

    if (fullBand) {{
        return true;
    }}

    // Short arc
    if (Cross2(startDir, stopDir) >= 0.0) {{
        if (Cross2(startDir, localCoord) < 0.0 || Cross2(localCoord, stopDir) < 0.0) {{
            return false;
        }}
    }}
    else {{
        if (Cross2(startDir, localCoord) < 0.0 && Cross2(localCoord, stopDir) < 0.0) {{
            return false;
        }}
    }}

    return true;
}}

void main(void)
{{
	if (!OnArcBand(v_localCoord, v_innerRadius_fullBand.x, v_startDir_stopDir.xy, v_startDir_stopDir.zw, v_innerRadius_fullBand.y > 0.0)) {{
		discard;
	}}
	vec4 color;
	{SG_TEXTURE_SWITCH}
	f_fragColor = color * v_tint;
}}
