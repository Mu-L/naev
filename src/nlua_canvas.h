/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

#include "nlua.h"

#include "opengl.h"

#define CANVAS_METATABLE "canvas" /**< Canvas metatable identifier. */

typedef struct LuaCanvas_s LuaCanvas_t;

/* Basic operations. */
LuaCanvas_t *lua_tocanvas( lua_State *L, int ind );
GLuint       luaL_checkcanvasfbo( lua_State *L, int ind );
void         lua_pushcanvas( lua_State *L, const LuaCanvas_t *canvas );
int          lua_iscanvas( lua_State *L, int ind );

/*
 * Misc helpers.
 */
const LuaCanvas_t *canvas_new( int w, int h );
GLuint             canvas_fbo( const LuaCanvas_t *lc );
glTexture         *canvas_tex( const LuaCanvas_t *lc );
void               canvas_reset( void );
