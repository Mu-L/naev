#include <SDL3/SDL.h>
#include <stdio.h>
#include <stdlib.h>

#define GL_VERSION 0x1F02

typedef unsigned int GLenum;

int main (void)
{
   SDL_Window *  window;
   SDL_GLContext context;
   char *( *glGetString )( GLenum name );

   if ( !SDL_Init( SDL_INIT_VIDEO ) ) {
      exit( -1 );
   }

   window = SDL_CreateWindow( "Get GL Version", 1, 1,
                              SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN );
   if ( window == NULL ) {
      exit( -1 );
   }

   SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
   SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 3 );
   SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE );

   context = SDL_GL_CreateContext( window );
   if ( !context ) {
      exit( -1 );
   }

   glGetString = (char *(*)(GLenum)) SDL_GL_GetProcAddress( "glGetString" );
   if ( glGetString == NULL ) {
      exit( -1 );
   }

   printf( "%s", glGetString( GL_VERSION ) );

   exit( 0 );
}
