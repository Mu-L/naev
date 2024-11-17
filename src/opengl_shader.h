/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

#include "opengl.h"

#include "colour.h"
#include "mat4.h"
#include "shaders.gen.h"

GLuint gl_program_backend( const char *vert, const char *frag, const char *geom,
                           const char *prependtext );
GLuint gl_program_vert_frag_geom( const char *vert, const char *frag,
                                  const char *geom );
GLuint gl_program_vert_frag( const char *vert, const char *frag );
GLuint gl_program_vert_frag_string( const char *vert, size_t vert_size,
                                    const char *frag, size_t frag_size );
void   gl_uniformColour( GLint location, const glColour *c );
void   gl_uniformAColour( GLint location, const glColour *c, GLfloat a );
void   gl_uniformMat4( GLint location, const mat4 *m );
