/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

/** @cond */
#include <lua.h>
/** @endcond */

#include "mission.h"

/* load the libraries for a Lua state */
Mission *misn_getFromLua( lua_State *L );
int      misn_loadLibs( nlua_env *env );
int      misn_loadCondLibs( lua_State *L ); /* safe read only stuff */

/* individual library stuff */
int nlua_loadMisn( nlua_env *env );

/* Useful stuff. */
void misn_pushMissionData( lua_State *L, const MissionData *md );
