#pragma pack(push, r_layerCmd, 1)
struct r_layerCmd_s {
	enum Command : uint8_t {
		VIEWPORT,
		BLEND,
		BIND,
		COLOR,
		QUAD,
	} cmd;
};

struct r_layerCmdViewport_s {
	r_layerCmd_s::Command cmd;
	r_viewport_s viewport;
};

struct r_layerCmdBlend_s {
	r_layerCmd_s::Command cmd;
	int blendMode;
};

struct r_layerCmdBind_s {
	r_layerCmd_s::Command cmd;
	r_tex_c* tex;
};

struct r_layerCmdColor_s {
	r_layerCmd_s::Command cmd;
	col4_t col;
};

struct r_layerCmdQuad_s {
	r_layerCmd_s::Command cmd;
	struct Quad {
		float s[4];
		float t[4];
		float x[4];
		float y[4];
		int stackLayer, maskLayer;
	} quad;
};
#pragma pack(pop, r_layerCmd)

r_aabb_s AabbFromCmdQuad(const r_layerCmdQuad_s::Quad& q);
