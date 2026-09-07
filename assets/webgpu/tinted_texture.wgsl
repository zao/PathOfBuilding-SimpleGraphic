struct VertexInput {
	@location(0) position: vec2<f32>,
	@location(1) uv: vec2<f32>,
	@location(2) tint: vec4<f32>,
	@location(3) texId: vec2<i32>,
};

struct VertexOutput {
	@builtin(position) position: vec4<f32>,
	@location(0) screenPos: vec2<f32>,
	@location(1) uv: vec2<f32>,
	@location(2) @interpolate(flat) tint: vec4<f32>,
	@location(3) @interpolate(flat) viewport: vec4<f32>,
	@location(4) @interpolate(flat) texId: vec2<i32>
}

struct FrameUniforms {
	screenSize: vec2<u32>,
}

@group(0) @binding(0) var<uniform> frameData: FrameUniforms;
@group(1) @binding(0) var texSampler: sampler;
@group(1) @binding(1) var colorTex: texture_2d_array<f32>;

fn SceneToNdc(scenePos: vec2<f32>) -> vec2<f32> {
	let unitPos = scenePos / vec2<f32>(frameData.screenSize);
	return mix(vec2(-1.0, 1.0), vec2(1.0, -1.0), unitPos);
}

@vertex
fn vsMain(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
	out.uv = in.uv;
	out.tint = in.tint;
	out.texId = in.texId;
    let pos = SceneToNdc(in.position);
	out.screenPos = pos;
	out.position = vec4(pos, 0, 1);
    return out;
}

@fragment
fn fsMain(in: VertexOutput) -> @location(0) vec4<f32> {
	var color: vec4<f32> = textureSample(colorTex, texSampler, in.uv, in.texId.y);
	return color * in.tint;
}
