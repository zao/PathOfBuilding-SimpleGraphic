struct VertexInput {
	@builtin(vertex_index) vIdx: u32,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@group(0) @binding(0) var s: sampler;
@group(0) @binding(1) var t: texture_2d<f32>;

@vertex
fn vsMain(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.position = vec4<f32>(
		select(-1.0, 3.0, in.vIdx == 1),
		select(-1.0, 3.0, in.vIdx == 2), 0.0, 1.0);
    out.uv = vec2<f32>(
		select(0.0, 2.0, in.vIdx == 1),
		select(1.0, -1.0, in.vIdx == 2));
    return out;
}

@fragment
fn fsMain(in: VertexOutput) -> @location(0) vec4<f32> {
    let color = textureSample(t, s, in.uv).rgb;
    return vec4<f32>(color, 1.0);
}
