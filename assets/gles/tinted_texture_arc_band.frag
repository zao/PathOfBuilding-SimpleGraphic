#version 300 es
precision mediump float;

uniform highp sampler2DArray s_tex[{SG_TEXTURE_COUNT}];

in vec2 v_screenPos;
in vec2 v_texcoord;
in vec4 v_tint;
in vec2 v_texId;

in vec2 v_localCoord;
in vec4 v_startDir_stopDir;
in vec2 v_innerRadius_angleSpan;
in vec4 v_texMin_texMax;

out vec4 f_fragColor;

float Cross2(vec2 a, vec2 b) {{
    return a.x*b.y - a.y*b.x;
}}

bool OnArcBand(vec2 localCoord, float rInner, vec2 startDir, vec2 stopDir, bool fullBand)
{{
    float r2 = dot(localCoord, localCoord);
    if (r2 < rInner*rInner || r2 > 1.f) {{
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

bool OnArcBand(vec2 localCoord, float rInner, vec2 startDir, vec2 stopDir, bool fullBand, inout vec2 bentCoord)
{{
    float r = length(localCoord);
    if (r < rInner || r > 1.f || rInner == 1.f) {{
        bentCoord = vec2(0.5, 0.0);
        return false;
    }}

    bentCoord.y = (r - rInner) / (1.f - rInner);

    // Short arc
    if (Cross2(startDir, stopDir) >= 0.0) {{
        if (Cross2(startDir, localCoord) < 0.0 || Cross2(localCoord, stopDir) < 0.0) {{
            bentCoord.x = 0.f;
            return false;
        }}
    }}
    else {{
        if (Cross2(startDir, localCoord) < 0.0 && Cross2(localCoord, stopDir) < 0.0) {{
            bentCoord.x = 0.f;
            return false;
        }}
    }}
    
    bentCoord.x = degrees(acos(abs(localCoord.x / r))) / 90.f;
    return true;
}}

void main(void)
{{
    float rInner = v_innerRadius_angleSpan.x;
    if (rInner == 1.f) {{
        discard;
    }}
    vec2 bentCoord = vec2(0.f, 0.f);
	if (!OnArcBand(v_localCoord, rInner, v_startDir_stopDir.xy, v_startDir_stopDir.zw, abs(v_innerRadius_angleSpan.y) >= 360.0, bentCoord)) {{
		discard;
	}}
    vec2 texMin = v_texMin_texMax.xy;
    vec2 texMax = v_texMin_texMax.zw;
    vec2 texcoord = mix(texMin, texMax, bentCoord);//vec2(1.f - abs(v_localCoord.x), 1.f - abs(v_localCoord.y));
    vec2 localPerp = vec2(-v_localCoord.y, v_localCoord.x);
    vec2 gradX = dFdx(localPerp / 2.f);
    vec2 gradY = dFdx(v_localCoord / 2.f);
	vec4 color;
	{SG_TEXTURE_SWITCH}
	f_fragColor = color * v_tint;
}}
