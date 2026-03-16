#include "r_dx11_shaders.h"
#include <string>

const std::string s_dx11_tintedTextureVertexSource = R"(// Vertex shader for tinted 2D sprites
cbuffer FrameCB : register(b0)
{
    float4x4 mvpMatrix;
};

struct VSInput
{
    float2 vertex: POSITION0;
    float2 texcoord : TEXCOORD0;
    float4 tint : TINT;
    int4 viewport : VIEWPORT;
};

struct PSInput
{
	float4 position : SV_Position;
    float2 screenPos : SCREEN_POS;
    float2 texcoord : TEXCOORD0;
    nointerpolation float4 tint : TINT;
    nointerpolation float4 viewport : VIEWPORT;
};

PSInput VSMain(VSInput input)
{
    PSInput result;

    result.texcoord = input.texcoord;
    result.tint = input.tint;
    float2 vp0 = input.viewport.xy + float2(0.0, input.viewport.w);
    float2 vp1 = input.viewport.xy + float2(input.viewport.z, 0.0);
    result.viewport = float4(
        mul(mvpMatrix, float4(vp0, 0.0, 1.0)).xy,
        mul(mvpMatrix, float4(vp1, 0.0, 1.0)).xy);
    float4 pos = mul(mvpMatrix, float4(input.vertex + input.viewport.xy, 0.0, 1.0));
    result.screenPos = pos.xy;
	result.position = pos;
    return result;
}
)";

const std::string s_dx11_tintedTexturePixelSource = R"(// Pixel shader for tinted 2D sprites
StructuredBuffer<uint> s_primitiveData : register(t0);
SamplerState s_smpWrap : register(s0);
SamplerState s_smpClamp : register(s1);
Texture2DArray s_tex[64] : register(t1);

struct PSInput
{
	float4 position : SV_Position;
    float2 screenPos : SCREEN_POS;
    float2 texcoord : TEXCOORD0;
    nointerpolation float4 tint : TINT;
    nointerpolation float4 viewport : VIEWPORT;
    uint primId : SV_PrimitiveId;
};

#define STACK_SHIFT 0
#define MASK_SHIFT (STACK_SHIFT + 8)
#define TEX_SHIFT (MASK_SHIFT + 8)
#define CLAMP_SHIFT (TEX_SHIFT + 8)
#define MONO_SHIFT (CLAMP_SHIFT + 1)
#define STACK_BITS 0xFFu
#define MASK_BITS 0xFFu
#define TEX_BITS 0xFFu
#define CLAMP_BITS 0x1u
#define MONO_BITS 0x1u

float4 ShadeColor(Texture2DArray tex, bool isClamping, bool isMono, float2 texcoord, int stack, int mask)
{
    float4 color;
	if (isClamping)
		color = tex.Sample(s_smpClamp, float3(texcoord, stack));
	else
		color = tex.Sample(s_smpWrap, float3(texcoord, stack));

	if (mask != MASK_BITS) {
		if (isClamping)
			color *= tex.Sample(s_smpClamp, float3(texcoord, mask));
		else
			color *= tex.Sample(s_smpWrap, float3(texcoord, mask));
	}
	return isMono ? float4(1.0, 1.0, 1.0, color.r) : color;
}

float4 PSMain(PSInput input) : SV_TARGET
{
    float x = input.screenPos.x, y = input.screenPos.y;
    if (x < input.viewport[0] || y < input.viewport[1] || x >= input.viewport[2] || y >= input.viewport[3])
        discard;

    uint prim = s_primitiveData[input.primId];
    uint texId = (prim >> TEX_SHIFT) & TEX_BITS;
    uint maskId = (prim >> MASK_SHIFT) & MASK_BITS;
    uint stackId = (prim >> STACK_SHIFT) & STACK_BITS;
	bool isClamping = !!((prim >> CLAMP_SHIFT) & CLAMP_BITS);
	bool isMono = !!((prim >> MONO_SHIFT) & MONO_BITS);
    float4 color = float4(0.0, 0.0, 0.0, 0.0);
    [branch] if (texId < 32) {
        [branch] if (texId < 16) {
            [branch] if (texId < 8) {
                [branch] if (texId < 4) {
                    [branch] if (texId < 2) {
                        [branch] if (texId < 1) {
							if (isClamping)
								color = ShadeColor(s_tex[0], isClamping, isMono, input.texcoord, stackId, maskId);
							else
								color = ShadeColor(s_tex[0], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[1], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 3) {
                            color = ShadeColor(s_tex[2], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[3], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 6) {
                        [branch] if (texId < 5) {
                            color = ShadeColor(s_tex[4], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[5], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 7) {
                            color = ShadeColor(s_tex[6], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[7], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            } else {
                [branch] if (texId < 12) {
                    [branch] if (texId < 10) {
                        [branch] if (texId < 9) {
                            color = ShadeColor(s_tex[8], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[9], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 11) {
                            color = ShadeColor(s_tex[10], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[11], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 14) {
                        [branch] if (texId < 13) {
                            color = ShadeColor(s_tex[12], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[13], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 15) {
                            color = ShadeColor(s_tex[14], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[15], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            }
        } else {
            [branch] if (texId < 24) {
                [branch] if (texId < 20) {
                    [branch] if (texId < 18) {
                        [branch] if (texId < 17) {
                            color = ShadeColor(s_tex[16], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[17], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 19) {
                            color = ShadeColor(s_tex[18], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[19], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 22) {
                        [branch] if (texId < 21) {
                            color = ShadeColor(s_tex[20], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[21], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 23) {
                            color = ShadeColor(s_tex[22], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[23], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            } else {
                [branch] if (texId < 28) {
                    [branch] if (texId < 26) {
                        [branch] if (texId < 25) {
                            color = ShadeColor(s_tex[24], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[25], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 27) {
                            color = ShadeColor(s_tex[26], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[27], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 30) {
                        [branch] if (texId < 29) {
                            color = ShadeColor(s_tex[28], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[29], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 31) {
                            color = ShadeColor(s_tex[30], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[31], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            }
        }
    } else {
        [branch] if (texId < 48) {
            [branch] if (texId < 40) {
                [branch] if (texId < 36) {
                    [branch] if (texId < 34) {
                        [branch] if (texId < 33) {
                            color = ShadeColor(s_tex[32], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[33], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 35) {
                            color = ShadeColor(s_tex[34], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[35], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 38) {
                        [branch] if (texId < 37) {
                            color = ShadeColor(s_tex[36], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[37], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 39) {
                            color = ShadeColor(s_tex[38], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[39], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            } else {
                [branch] if (texId < 44) {
                    [branch] if (texId < 42) {
                        [branch] if (texId < 41) {
                            color = ShadeColor(s_tex[40], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[41], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 43) {
                            color = ShadeColor(s_tex[42], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[43], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 46) {
                        [branch] if (texId < 45) {
                            color = ShadeColor(s_tex[44], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[45], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 47) {
                            color = ShadeColor(s_tex[46], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[47], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            }
        } else {
            [branch] if (texId < 56) {
                [branch] if (texId < 52) {
                    [branch] if (texId < 50) {
                        [branch] if (texId < 49) {
                            color = ShadeColor(s_tex[48], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[49], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 51) {
                            color = ShadeColor(s_tex[50], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[51], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 54) {
                        [branch] if (texId < 53) {
                            color = ShadeColor(s_tex[52], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[53], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 55) {
                            color = ShadeColor(s_tex[54], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[55], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            } else {
                [branch] if (texId < 60) {
                    [branch] if (texId < 58) {
                        [branch] if (texId < 57) {
                            color = ShadeColor(s_tex[56], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[57], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 59) {
                            color = ShadeColor(s_tex[58], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[59], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                } else {
                    [branch] if (texId < 62) {
                        [branch] if (texId < 61) {
                            color = ShadeColor(s_tex[60], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[61], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    } else {
                        [branch] if (texId < 63) {
                            color = ShadeColor(s_tex[62], isClamping, isMono, input.texcoord, stackId, maskId);
                        } else {
                            color = ShadeColor(s_tex[63], isClamping, isMono, input.texcoord, stackId, maskId);
                        }
                    }
                }
            }
        }
    }
    return color * input.tint;
}
)";