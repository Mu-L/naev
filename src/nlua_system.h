/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

#include "nlua.h"
#include "space.h"

#define SYSTEM_METATABLE "system" /**< System metatable identifier. */

/**
 * @brief Lua StarSystem Wrapper.
 */
typedef int LuaSystem;

/*
 * Load the system library.
 */
int nlua_loadSystem( nlua_env env );

/*
 * System operations.
 */
LuaSystem   lua_tosystem( lua_State *L, int ind );
LuaSystem   luaL_checksystem( lua_State *L, int ind );
LuaSystem  *lua_pushsystem( lua_State *L, LuaSystem sys );
StarSystem *luaL_validsystem( lua_State *L, int ind );
int         lua_issystem( lua_State *L, int ind );
