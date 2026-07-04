uniform float u_r      = 0.0;
uniform float u_time   = 0.0;
uniform float u_fade   = 1.0;

in vec2 pos;
out vec4 colour_out;

const vec3 COLOUR = vec3( 1.0, 0.15, 1.0);

void main( void)
{
   float x = pos.x * 0.3;
   float y = pos.y * 0.4;

   float ax = abs(x);
   float yy = y*y;

   // Main beam
   float beam = exp(-80.0 * yy)
      * smoothstep(0.25, 0.10, ax); // rounding

   // pulsations
   beam *= 0.9 + 0.1 * sin(120.0 * x + u_time * 25.0);

   // white core
   float core = exp(-450.0 * yy)
      * smoothstep(0.18, 0.02, ax); // rounding

   // soft glow
   float glow = exp(-18.0 * yy)
      * smoothstep(0.45, 0.10, ax); // rounding

   vec3 color = glow * vec3(0.5, 0.05, 0.05);
   color    += beam * COLOUR;
   color    += core * vec3(1.0);

   float alpha = max(glow * 0.5, beam);

   colour_out = vec4(color, alpha * u_fade);
}
