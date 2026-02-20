struct RectHollowData {
   transform: mat3x3f,
   colour: vec4f,
   dims: vec2f,
   border: f32,
}
@binding(0) @group(0) var<uniform> rectdata: RectHollowData;

struct VertexInput {
   @location(0) vertex: vec2f,
}
struct VertexOutput {
   @builtin(position) position: vec4f,
   @location(0) uv: vec2f,
   @location(1) b: f32,
}
struct FragmentInput {
   @location(0) uv: vec2f,
   @location(1) b: f32,
}

@vertex
fn main_vs( vs: VertexInput ) -> VertexOutput {
   var output: VertexOutput;
   let H = rectdata.transform;
   output.position = vec4( ( H * vec3f( vs.vertex, 1.0 ) ).xy, 0.0, 1.0 );
   let scale   = vec2f( length(H[0].xy), length(H[1].xy) );
   output.uv   = (vs.vertex * 2.0 - 1.0) * rectdata.dims;
   output.b    = rectdata.border * 0.5;
   return output;
}

fn sdBox( p: vec2f, b: vec2f ) -> f32
{
   let d = abs(p) - b;
   return length(max(d,vec2f(0.0))) + min(max(d.x,d.y),0.0);
}

@fragment
fn main_fs( fs: FragmentInput ) -> @location(0) vec4f {
   let pos  = fs.uv;
   let m    = 1.0;
   let b    = fs.b;
   let rad  = 1.0 - m;
   let d    = abs(sdBox( pos, pos-vec2f(m + b) )) - b;
   let alpha = smoothstep( -m, 0.0, -d );
   return rectdata.colour * vec4f( vec3f( 1.0 ), alpha );
}
