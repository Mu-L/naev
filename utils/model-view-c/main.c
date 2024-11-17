#include <stdio.h>
#include <libgen.h>

#include "common.h"

#include "SDL.h"
#include "SDL_image.h"
#include "physfs.h"

#include "glad.h"

#include "gltf.h"
#include "shader_min.h"
#include "mat4.h"

PlayerConf_t conf = {
   .low_memory = 0,
   .max_3d_tex_size = 0,
};

typedef struct glColour_ {
   GLfloat r; /**< Red value of the colour (0 to 1). */
   GLfloat g; /**< Green value of the colour (0 to 1). */
   GLfloat b; /**< Blue value of the colour (0 to 1). */
   GLfloat a; /**< Alpha value of the colour (0 to 1). */
} __attribute__( ( packed ) ) glColour;
__attribute__( ( const ) ) double gammaToLinear( double x )
{
   if ( x <= 0.04045 )
      return x / 12.92;
   return pow( ( x + 0.055 ) / 1.055, 2.4 );
}
void col_gammaToLinear( glColour *c )
{
   c->r = gammaToLinear( c->r );
   c->g = gammaToLinear( c->g );
   c->b = gammaToLinear( c->b );
}
void col_hsv2rgb( glColour *c, float h, float s, float v )
{
   float var_h, var_i, var_1, var_2, var_3;

   if ( v > 1 )
      v = 1;

   if ( s == 0 ) {
      c->r = v;
      c->g = v;
      c->b = v;
   } else {
      var_h = h * 6 / 360.;
      var_i = floor( var_h );
      var_1 = v * ( 1 - s );
      var_2 = v * ( 1 - s * ( var_h - var_i ) );
      var_3 = v * ( 1 - s * ( 1 - ( var_h - var_i ) ) );

      if ( var_i == 0 ) {
         c->r = v;
         c->g = var_3;
         c->b = var_1;
      } else if ( var_i == 1 ) {
         c->r = var_2;
         c->g = v;
         c->b = var_1;
      } else if ( var_i == 2 ) {
         c->r = var_1;
         c->g = v;
         c->b = var_3;
      } else if ( var_i == 3 ) {
         c->r = var_1;
         c->g = var_2;
         c->b = v;
      } else if ( var_i == 4 ) {
         c->r = var_3;
         c->g = var_1;
         c->b = v;
      } else {
         c->r = v;
         c->g = var_1;
         c->b = var_2;
      }
   }

   c->r = gammaToLinear( c->r );
   c->g = gammaToLinear( c->g );
   c->b = gammaToLinear( c->b );
}

int main( int argc, char *argv[] )
{
   (void) argc;
   (void) argv;
   GLuint VaoId;
   int shadowmap_sel = 0;
   char *path, **search_path;
   SDL_GLContext *context;

   if (argc < 2) {
      DEBUG("Usage: %s FILENAME", argv[0]);
      return -1;
   }

   PHYSFS_init( argv[0] );
   PHYSFS_mount( "../../dat/glsl/", "/", 1 );
   path = strdup(argv[1]);
   PHYSFS_mount( dirname(path), "/", 1 );
   free(path);
   path = strdup(argv[0]);
   PHYSFS_mount( dirname(path), "/", 1 );
   free(path);

   search_path = PHYSFS_getSearchPath();
   LOG( "%s", _( "Read locations, searched in order:" ) );
   for ( char **p = search_path; *p != NULL; p++ )
      LOG( "    %s", *p );
   PHYSFS_freeList( search_path );

   SDL_Init( SDL_INIT_VIDEO );
   SDL_GL_SetAttribute(SDL_GL_FRAMEBUFFER_SRGB_CAPABLE, 1);
   SDL_Window *win = SDL_CreateWindow( "Naev Model Viewer", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_W, SCREEN_H, SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN );
   SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 4 );
   SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 6 );
   SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK,
                        SDL_GL_CONTEXT_PROFILE_CORE );
   context = SDL_GL_CreateContext( win );
   if (context==NULL) {
      SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
      SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 2 );
      context = SDL_GL_CreateContext( win );
      assert( context != NULL );
   }
   gladLoadGLLoader(SDL_GL_GetProcAddress);

   IMG_Init(IMG_INIT_PNG);

   glGenVertexArrays(1, &VaoId);
   glBindVertexArray(VaoId);

   glEnable( GL_FRAMEBUFFER_SRGB );
   glClearColor( 0.1, 0.1, 0.1, 1.0 );
   glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

   if (gltf_init())
      return -1;

   /* Load the object. */
   path = strdup(argv[1]);
   GltfObject *obj = gltf_loadFromFile( basename(path) );
   free( path );

   /* Set some lighting parameters. */
   double al = 0.1;
   gltf_lightAmbient( al, al, al );
   gltf_lightIntensity( 1. );

   /* Set up some stuff. */
   GLuint shadowvbo;
   const GLfloat shadowvbo_data[8] = {
      0., 0.,
      1., 0.,
      0., 1.,
      1., 1. };
   glGenBuffers( 1, &shadowvbo );
   glBindBuffer( GL_ARRAY_BUFFER, shadowvbo );
   glBufferData( GL_ARRAY_BUFFER, sizeof(GLfloat) * 8, shadowvbo_data, GL_STATIC_DRAW );
   glBindBuffer( GL_ARRAY_BUFFER, 0 );
   GLuint shadowshader = gl_program_backend( "depth.vert", "depth.frag", NULL, "" );
   glUseProgram( shadowshader );
   GLuint shadowvertex = glGetAttribLocation( shadowshader, "vertex" );
   GLuint shadowtex    = glGetUniformLocation( shadowshader, "sampler" );
   glUniform1i( shadowtex, 0 );
   glUseProgram( 0 );

   int rendermode = 1;
   int engine = 0;
   int nebula = 0;
   int quit = 0;
   float rotx = 0.;
   float roty = -M_PI_2;
   const double dt = 1.0/60.0;
   while (!quit) {
      SDL_Event event;

      while (SDL_PollEvent( &event )) {
         if (event.type == SDL_QUIT)
            quit = 1;
         else if (event.type == SDL_KEYDOWN) {
            SDL_Keycode key = event.key.keysym.sym;
            switch (key) {
               case SDLK_q:
               case SDLK_ESCAPE:
                  quit = 1;
                  break;

               case SDLK_m:
                  rendermode = !rendermode;
                  break;

               case SDLK_n:
                  nebula = !nebula;
                  if (nebula) {
                     glColour col;
                     const double str = 3.0;
                     col_hsv2rgb( &col, 1.0 * 360., 1., 1. );
                     gltf_lightAmbient( str * col.r, str * col.g, str * col.b );
                     gltf_lightIntensity( 0.5 );
                  }
                  else {
                     gltf_lightAmbient( al, al, al );
                     gltf_lightIntensity( 1. );
                  }
                  break;

               case SDLK_e:
                  engine = !engine;
                  break;

               case SDLK_1:
                  shadowmap_sel = 0;
                  break;

               case SDLK_2:
                  shadowmap_sel = 1;
                  break;

               case SDLK_3:
                  shadowmap_sel = 2;
                  break;

               default:
                  break;
            }
         }
      }
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

      const Uint8 *state = SDL_GetKeyboardState(NULL);
      float rotxspeed = 0.0;
      float rotyspeed = 0.0;
      if (state[SDL_SCANCODE_LEFT])
         rotyspeed -= M_PI_2;
      if (state[SDL_SCANCODE_RIGHT])
         rotyspeed += M_PI_2;
      if (state[SDL_SCANCODE_DOWN])
         rotxspeed -= M_PI_2;
      if (state[SDL_SCANCODE_UP])
         rotxspeed += M_PI_2;
      rotx += rotxspeed * dt;
      roty += rotyspeed * dt;
      GLfloat c = cos(rotx);
      GLfloat s = sin(rotx);
      mat4 Hx = { .m = {
         {  c,  -s,  0.0, 0.0 },
         {  s,   c,  0.0, 0.0 },
         { 0.0, 0.0, 1.0, 0.0 },
         { 0.0, 0.0, 0.0, 1.0 }
      } };
      c = cos(roty);
      s = sin(roty);
      mat4 Hy = { .m = {
         {  c,  0.0,  s,  0.0 },
         { 0.0, 1.0, 0.0, 0.0 },
         { -s,  0.0,  c,  0.0 },
         { 0.0, 0.0, 0.0, 1.0 }
      } };
      const GLfloat sca = 1.0;
      const mat4 Hscale = { .m = {
         { sca, 0.0, 0.0, 0.0 },
         { 0.0, sca, 0.0, 0.0 },
         { 0.0, 0.0, sca, 0.0 },
         { 0.0, 0.0, 0.0, 1.0 } } };

      mat4 H;
      mat4_mul( &H, &Hx, &Hy );
      mat4_apply( &H, &Hscale );

      /* Draw the object. */
      int scene = obj->scene_body;
      if (obj->scene_engine >= 0 && engine)
         scene = obj->scene_engine;
      gltf_renderScene( 0, obj, scene, &H, (float)SDL_GetTicks64() / 1000., SCREEN_W, 0 );

      /* Draw the shadowmap to see what's going on (clear the shadowmap). */
      if (rendermode) {
         GLuint shadowmap = gltf_shadowmap( shadowmap_sel );
         glUseProgram( shadowshader );

         glBindBuffer( GL_ARRAY_BUFFER, shadowvbo );
         glVertexAttribPointer( shadowvertex, 2, GL_FLOAT, GL_FALSE, 0, NULL );
         glEnableVertexAttribArray( shadowvertex );

         glActiveTexture( GL_TEXTURE0 );
         glBindTexture( GL_TEXTURE_2D, shadowmap );

         glDrawArrays( GL_TRIANGLE_STRIP, 0, 4 );

         glBindBuffer( GL_ARRAY_BUFFER, 0 );
         glDisableVertexAttribArray( shadowvertex );
         glUseProgram( 0 );
      }

      SDL_GL_SwapWindow( win );

      gl_checkErr();

      SDL_Delay( 1000 * dt );
   }

   gltf_free( obj );

   gltf_exit();

   SDL_Quit();
   PHYSFS_deinit();

   return 0;
}
