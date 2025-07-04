/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

#include "commodity.h"
#include "nlua.h"

#define COMMODITY_METATABLE                                                    \
   "commodity" /**< Commodity metatable identifier.                            \
                */

/*
 * Library loading
 */
int nlua_loadCommodity( nlua_env *env );

/*
 * Commodity operations
 */
Commodity  *lua_tocommodity( lua_State *L, int ind );
Commodity  *luaL_checkcommodity( lua_State *L, int ind );
Commodity  *luaL_validcommodity( lua_State *L, int ind );
Commodity **lua_pushcommodity( lua_State *L, Commodity *commodity );
int         lua_iscommodity( lua_State *L, int ind );
