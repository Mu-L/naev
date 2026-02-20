struct CircleHollowData {
   transform: mat3x3f,
   colour: vec4f,
   radius: f32,
   width: f32,
}
@binding(0) @group(0) var<uniform> circledata: CircleHollowData;

struct VertexInput {
   @location(0) vertex: vec2f,
}
struct VertexOutput {
   @builtin(position) position: vec4f,
   @location(0) uv: vec2f,
   @location(1) m: f32,
   @location(2) w: f32,
}
struct FragmentInput {
   @location(0) uv: vec2f,
   @location(1) m: f32,
   @location(2) w: f32,
}

@vertex
fn main_vs( vs: VertexInput ) -> VertexOutput {
   var output: VertexOutput;
   output.position = vec4( ( circledata.transform * vec3f( vs.vertex, 1.0 ) ).xy, 0.0, 1.0 );
   output.uv = vs.vertex;
   output.m = 1.0 / (2.0 * circledata.radius);
   output.w = circledata.width / circledata.radius * 0.5;
   return output;
}

@fragment
fn main_fs( fs: FragmentInput ) -> @location(0) vec4f {
   let pos  = fs.uv;
   let m    = fs.m;
   let w    = fs.w;
   let rad  = 1.0 - m - w;
   let d    = abs( length( pos ) - rad ) - w;
   let alpha = smoothstep( -m, 0.0, -d );
   return circledata.colour * vec4f( vec3f( 1.0 ), alpha );
}
