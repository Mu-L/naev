#pragma language glsl3

#include "lib/sdf.glsl"
#include "lib/math.glsl"

uniform float u_time;
uniform float u_r;

in vec2 pos;
out vec4 colour_out;

const int SPIKES = 7;

void main (void)
{
   float t = u_time * 2.7 + 1000.0 * u_r;

   float r = length(pos);
   float a = atan(pos.y, pos.x);

   float radius = 0.55;
   for (int i = 0; i < SPIKES; i++) {
      float fi = float(i);

      float rnd1 = random(fi * 11.1);
      float rnd2 = random(fi * 91.7);
      float rnd3 = random(fi * 19.3);

      // angular center with slow drift
      float center =
         fi * M_PI * 2.0 / float(SPIKES)
         + (rnd1 - 0.5) * 0.5
         + 0.2 * sin(t + fi * 2.7);

      // angular distance
      float d = abs(atan(sin(a - center), cos(a - center)));

      // shape
      float w = (0.50 + 0.3 * rnd3)
         * (0.9 + 0.1 * sin(t * 0.8 + fi));
      float h = (0.20 + 0.22 * rnd2)
         * (0.75 + 0.25 * sin(t * 1.3 + fi * 4.1));

      float shape = pow(max(0.0, 1.0 - d / w), 3.0);

      radius += h * shape;
   }

   float sdf = r - radius;
   float cell = smoothstep(0.1, 0.0, sdf);
   float outter = smoothstep(0.1, 0.0, sdf);
   float inner = smoothstep(0.0, -0.5, sdf);

   vec3 col;
   // Outer cell
   if (sdf > 0.0)
      col = vec3( 1.2, 0.8, 0.3 );
   else
      col = pow(1.0-inner, 4.0) * vec3( 0.2, 0.5, 0.05 );

   // Inner core
   t *= 1.7; // faster movement
   float d = 1000.0;
   for (int i = 0; i < 3; i++) {
      float fi = float(i);
      float a = t * (0.6 + fi * 0.2) + fi * 2.1;
      vec2 center = vec2(
            cos(a * 1.3 + fi * 3.1),
            sin(a * 1.1 + fi * 4.7)
            ) * 0.15;
      float r = 0.15 + 0.01 * sin(t * 2.0 + fi * 10.0);
      float blob = sdCircle(pos - center, r);
      d = sdSmoothUnion( d, blob, 0.04 );
   }
   float blobs = smoothstep(0.02, -0.1, d);
   vec3 red = vec3(0.95, 0.15, 0.20);
   col += blobs * red;

   colour_out = vec4(col, cell);
}
