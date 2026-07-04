#include "lib/sdf.glsl"
#include "lib/simplex.glsl"

uniform float u_r       = 0.0;
uniform float u_time    = 0.0;
uniform float u_fade    = 1.0;

in vec2 pos;
out vec4 colour_out;

void main (void)
{
   float t = u_time * 0.8 + 1000.0*u_r;

   vec2 warp = vec2(
         snoise(pos * 3.0 + vec2(t, 0.0)),
         snoise(pos * 3.0 + vec2(0.0, t))
         );
   vec2 p = pos + warp * 0.08;
   p *= 0.6;

   float surfaceNoise = snoise(p * 3.6 + t)
      + 0.5 * snoise(p * 7.2 - t * 1.5);
   float d = sdCircle(p, 0.45 + surfaceNoise * 0.04);

   float glow  = smoothstep( 0.25, -0.25, d);
   float shell = smoothstep( 0.04, -0.04, d);
   float core  = smoothstep(-0.18, -0.34, d);

   vec3 col;
   col  = glow  * vec3(0.1, 0.5, 1.0) * 0.4;
   col += shell * vec3(0.2, 0.8, 1.0);
   col += core  * vec3(1.0);

   colour_out = vec4(col, glow * u_fade);
}
