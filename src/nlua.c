/*
 * See Licensing and Copyright notice in naev.h
 */
/**
 * @file nlua.c
 *
 * @brief Handles creating and setting up basic Lua environments.
 */

/** @cond */
#include "physfs.h"

#include "naev.h"
/** @endcond */

#include "nlua.h"

#include "array.h"
#include "cmark_wrap.h"
#include "conf.h"
#include "console.h"
#include "debug.h"
#include "log.h"
#include "lua_enet.h"
#include "lutf8lib.h"
#include "lyaml.h"
#include "ndata.h"
#include "nlua_audio.h"
#include "nlua_cli.h"
#include "nlua_commodity.h"
#include "nlua_data.h"
#include "nlua_diff.h"
#include "nlua_faction.h"
#include "nlua_file.h"
#include "nlua_jump.h"
#include "nlua_linopt.h"
#include "nlua_naev.h"
#include "nlua_news.h"
#include "nlua_outfit.h"
#include "nlua_pilot.h"
#include "nlua_player.h"
#include "nlua_rnd.h"
#include "nlua_safelanes.h"
#include "nlua_shiplog.h"
#include "nlua_spfx.h"
#include "nlua_spob.h"
#include "nlua_system.h"
#include "nlua_time.h"
#include "nlua_var.h"
#include "nlua_vec2.h"
#include "nluadef.h"
#include "nstring.h"

lua_State *naevL         = NULL;      /**< Global Naev Lua state. */
nlua_env   __NLUA_CURENV = LUA_NOREF; /**< Current environment. */
static char
   *common_script; /**< Common script to run when creating environments. */
static size_t common_sz; /**< Common script size. */
static int    nlua_envs = LUA_NOREF;

/**
 * @brief Cache structure for loading chunks.
 */
typedef struct LuaCache_ {
   char *path; /**< Path of the file. */
   int   idx;  /**< Index of the loaded cache. */
} LuaCache_t;
static LuaCache_t *lua_cache = NULL;

/*
 * prototypes
 */
static int        nlua_package_preload( lua_State *L );
static int        nlua_package_loader_lua( lua_State *L );
static int        nlua_package_loader_c( lua_State *L );
static int        nlua_package_loader_croot( lua_State *L );
static int        nlua_require( lua_State *L );
static lua_State *nlua_newState( void ); /* creates a new state */
static int        nlua_loadBasic( lua_State *L );
static int        luaB_loadstring( lua_State *L );
static int        lua_cache_cmp( const void *p1, const void *p2 );
static int        nlua_errTraceInternal( lua_State *L, int idx );

/* gettext */
static int            nlua_gettext( lua_State *L );
static int            nlua_ngettext( lua_State *L );
static int            nlua_pgettext( lua_State *L );
static int            nlua_gettext_noop( lua_State *L );
static const luaL_Reg gettext_methods[] = {
   { "gettext", nlua_gettext },
   { "ngettext", nlua_ngettext },
   { "pgettext", nlua_pgettext },
   { "gettext_noop", nlua_gettext_noop },
   { 0, 0 } }; /**< Vector metatable methods. */

static const lua_CFunction loaders[] = {
   nlua_package_preload, nlua_package_loader_lua, nlua_package_loader_c,
   nlua_package_loader_croot, NULL }; /**< Our loaders. */

/**
 * @brief gettext support.
 *
 * @usage _( str )
 *    @luatparam str String to gettext on.
 *    @luatreturn The string converted to gettext.
 * @luafunc gettext
 */
static int nlua_gettext( lua_State *L )
{
   const char *str = luaL_checkstring( L, 1 );
   lua_pushstring( L, _( str ) );
   return 1;
}

/**
 * @brief gettext support for singular and plurals.
 *
 * @usage ngettext( msgid1, msgid2, n )
 *    @luatparam msgid1 Singular form.
 *    @luatparam msgid2 Plural form.
 *    @luatparam n Number of elements.
 *    @luatreturn The string converted to gettext.
 * @luafunc ngettext
 */
static int nlua_ngettext( lua_State *L )
{
   const char *stra = luaL_checkstring( L, 1 );
   const char *strb = luaL_checkstring( L, 2 );
   int         n    = luaL_checkinteger( L, 3 );
   lua_pushstring( L, n_( stra, strb, n ) );
   return 1;
}

/**
 * @brief gettext support with context.
 *
 * @usage pgettext( context, msg )
 *    @luatparam context Context of the message.
 *    @luatparam msg Message to translate.
 *    @luatreturn The string converted to gettext.
 * @luafunc pgettext
 */
static int nlua_pgettext( lua_State *L )
{
   const char *msgctxt = luaL_checkstring( L, 1 );
   const char *msgid   = luaL_checkstring( L, 2 );
   lua_pushstring( L, pgettext_var( msgctxt, msgid ) );
   return 1;
}

/**
 * @brief gettext support (noop). Does not actually do anything, but gets
 * detected by gettext.
 *
 * @usage N_( str )
 *    @luatparam str String to gettext on.
 *    @luatreturn The string converted to gettext.
 * @luafunc gettext_noop
 */
static int nlua_gettext_noop( lua_State *L )
{
   const char *str = luaL_checkstring( L, 1 );
   lua_pushstring( L, str );
   return 1;
}

/** @brief Implements the Lua function math.log2 (base-2 logarithm). */
static int nlua_log2( lua_State *L )
{
   double n = luaL_checknumber( L, 1 );
   lua_pushnumber( L, log2( n ) );
   return 1;
}

/**
 * @brief Implements the Lua function os.getenv. In the sandbox we only make a
 * fake $HOME visible.
 */
static int nlua_os_getenv( lua_State *L )
{
   const char *var = luaL_checkstring( L, 1 );
   if ( strcmp( var, "HOME" ) )
      return 0;
   lua_pushstring( L, "lua_home" );
   return 1;
}

/**
 * @brief Handles what to do when Lua panics.
 *
 * By default it uses exit( EXIT_FAILURE );, but we want to generate a backtrace
 * or let gdb catch it if possible.
 */
static int nlua_panic( lua_State *L )
{
   ERR( _( "LUA PANIC: %s" ), lua_tostring( L, -1 ) );
   return 0;
}

/*
 * @brief Initializes the global Lua state.
 */
void lua_init( void )
{
   naevL = nlua_newState();
   nlua_loadBasic( naevL );

   /* Environment table. */
   lua_newtable( naevL );
   nlua_envs = luaL_ref( naevL, LUA_REGISTRYINDEX );

   /* Better clean up. */
   lua_atpanic( naevL, nlua_panic );

   /* Initialize the caches. */
   lua_cache = array_create( LuaCache_t );
}

/**
 * @brief Replacement for the internal Lua loadstring().
 */
static int luaB_loadstring( lua_State *L )
{
   size_t      l;
   const char *s         = luaL_checklstring( L, 1, &l );
   const char *chunkname = luaL_optstring( L, 2, s );
   int         status    = luaL_loadbuffer( L, s, l, chunkname );
   if ( status == 0 ) /* OK? */
      return 1;
   else {
      lua_pushnil( L );
      lua_insert( L, -2 ); /* put before error message */
      return 2;            /* return nil plus error message */
   }
}

/*
 * @brief Closes the global Lua state.
 */
void lua_exit( void )
{
   lua_clearCache();
   array_free( lua_cache );
   lua_cache = NULL;

   free( common_script );
   lua_close( naevL );
   naevL = NULL;
}

int nlua_warn( lua_State *L, int idx )
{
   const char *msg = luaL_checkstring( L, idx );
#if DEBUGGING
   nlua_errTraceInternal( L, idx );
   const char *dbgmsg = lua_tostring( L, -1 );
   LOGERR( "%s", dbgmsg );
   cli_printCoreString( dbgmsg, 1 );
   lua_pop( L, 1 );
#endif /* DEBUGGING */
   WARN( "%s", msg );
   /* Add to console. */
   cli_printCoreString( msg, 1 );
   return 0;
}

/**
 * @brief Clears the cached stuff.
 */
void lua_clearCache( void )
{
   for ( int i = 0; i < array_size( lua_cache ); i++ ) {
      LuaCache_t *lc = &lua_cache[i];
      free( lc->path );
      luaL_unref( naevL, LUA_REGISTRYINDEX,
                  lc->idx ); /* lua_close should have taken care of this. */
   }
   array_erase( &lua_cache, array_begin( lua_cache ), array_end( lua_cache ) );
}

/*
 * @brief Run code from buffer in Lua environment.
 *
 *    @param env Lua environment.
 *    @param buff Pointer to buffer.
 *    @param sz Size of buffer.
 *    @param name Name to use in error messages.
 *    @return 0 on success.
 */
int nlua_dobufenv( nlua_env env, const char *buff, size_t sz, const char *name )
{
   int ret;
#if DEBUGGING
   /* We don't really want to do this, but Lua seems to trigger all sorts of
    * FPE exceptions on a daily basis.
    * TODO get rid of if possible. */
   if ( conf.fpu_except )
      debug_disableFPUExcept();
#endif /* DEBUGGING */
   ret = luaL_loadbuffer( naevL, buff, sz, name );
   if ( ret != 0 )
      return ret;
#if DEBUGGING
   if ( conf.fpu_except )
      debug_enableFPUExcept();
#endif /* DEBUGGING */

   ret = nlua_pcall( env, 0, LUA_MULTRET );
   if ( ret != 0 )
      return ret;
   return 0;
}

/*
 * @brief Run code a file in Lua environment.
 *
 *    @param env Lua environment.
 *    @param filename Filename of Lua script.
 */
int nlua_dofileenv( nlua_env env, const char *filename )
{
   if ( luaL_loadfile( naevL, filename ) != 0 )
      return -1;
   if ( nlua_pcall( env, 0, LUA_MULTRET ) != 0 )
      return -1;
   return 0;
}

int nlua_loadbuffer( lua_State *L, const char *buff, size_t sz,
                     const char *name )
{
#if DEBUGGING
   /* We don't really want to do this, but Lua seems to trigger all sorts of
    * FPE exceptions on a daily basis.
    * TODO get rid of if possible. */
   if ( conf.fpu_except )
      debug_disableFPUExcept();
#endif /* DEBUGGING */
   int ret = luaL_loadbuffer( L, buff, sz, name );
#if DEBUGGING
   if ( conf.fpu_except )
      debug_enableFPUExcept();
#endif /* DEBUGGING */
   return ret;
}

/*
 * @brief Run code from chunk in Lua environment.
 *
 *    @param env Lua environment.
 *    @param chunk Chunk to run.
 *    @return 0 on success.
 */
int nlua_dochunkenv( nlua_env env, int chunk, const char *name )
{
   (void)name;
   int ret;
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, chunk );
   ret = nlua_pcall( env, 0, LUA_MULTRET );
   if ( ret != 0 )
      return ret;
#if DEBUGGING
   lua_pushstring( naevL, name );
   nlua_setenv( naevL, env, "__name" );
#endif /* DEBUGGING */
   return 0;
}

#if DEBUGGING
void nlua_pushEnvTable( lua_State *L )
{
   lua_rawgeti( L, LUA_REGISTRYINDEX, nlua_envs );
}
#endif /* DEBUGGING */

/*
 * @brief Create an new environment in global Lua state.
 *
 * An "environment" is a table used with setfenv for sandboxing.
 */
nlua_env nlua_newEnv( const char *name )
{
   nlua_env ref;
   if ( conf.fpu_except )
      debug_disableFPUExcept();

   /* Create new table and register it. */
   lua_newtable( naevL );                      /* t */
   lua_pushvalue( naevL, -1 );                 /* t, t */
   ref = luaL_ref( naevL, LUA_REGISTRYINDEX ); /* t */

   /* Store in the environment table. */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, nlua_envs ); /* t, e */
   lua_pushvalue( naevL, -2 );                         /* t, e, t */
   lua_rawseti( naevL, -2, ref );                      /* t, e */
   lua_pop( naevL, 1 );                                /* t */

   if ( name != NULL ) {
      lua_pushstring( naevL, name );
      nlua_setenv( naevL, ref, "__name" );
   }

   /* Metatable */
   lua_newtable( naevL );                    /* t, m */
   lua_pushvalue( naevL, LUA_GLOBALSINDEX ); /* t, m, g */
   lua_setfield( naevL, -2, "__index" );     /* t, m */
   lua_setmetatable( naevL, -2 );            /* t */

   /* Replace require() function with one that considers fenv */
   lua_pushvalue( naevL, -1 );                 /* t, t, */
   lua_pushcclosure( naevL, nlua_require, 1 ); /* t, t, c */
   lua_setfield( naevL, -2, "require" );       /* t, t */

   /* Set up paths.
    * "package.path" to look in the data.
    * "package.cpath" unset */
   nlua_getenv( naevL, ref, "package" );       /* t, t, p */
   lua_newtable( naevL );                      /* t, t, p, t */
   lua_pushvalue( naevL, -1 );                 /* t, t, p, t, t */
   nlua_setenv( naevL, ref, NLUA_LOAD_TABLE ); /* t, t, p, t */
   lua_setfield( naevL, -2, "loaded" );        /* t, t, p */
   lua_newtable( naevL );                      /* t, t, p, t */
   lua_setfield( naevL, -2, "preload" );       /* t, t, p */
   lua_pushstring( naevL, "?.lua;" LUA_INCLUDE_PATH "?.lua" ); /* t, t, p, s */
   lua_setfield( naevL, -2, "path" );                          /* t, t, p */
   lua_pushstring( naevL, "" );                                /* t, t, p, s */
   lua_setfield( naevL, -2, "cpath" );                         /* t, t, p */
   lua_getfield( naevL, -1, "loaders" );                       /* t, t, p, l */
   for ( int i = 0; loaders[i] != NULL; i++ ) {
      lua_pushcfunction( naevL, loaders[i] ); /* t, t, p, l, f */
      lua_rawseti( naevL, -2, i + 1 );        /* t, t, p, l */
   }
   lua_pop( naevL, 2 ); /* t, t */

   /* The global table _G should refer back to the environment. */
   lua_pushvalue( naevL, -1 );      /* t, t, t */
   lua_setfield( naevL, -2, "_G" ); /* t, t */

   /* Push if naev is built with debugging. */
#if DEBUGGING
   lua_pushboolean( naevL, 1 );              /* t, t, b */
   lua_setfield( naevL, -2, "__debugging" ); /* t, t, */
#endif                                       /* DEBUGGING */

   /* Set up naev namespace. */
   lua_newtable( naevL );             /* t, t, n */
   lua_setfield( naevL, -2, "naev" ); /* t, t */

   /* Run common script. */
   if ( conf.loaded && common_script == NULL ) {
      common_script = ndata_read( LUA_COMMON_PATH, &common_sz );
      if ( common_script == NULL )
         WARN( _( "Unable to load common script '%s'!" ), LUA_COMMON_PATH );
   }
   if ( common_script != NULL ) {
      if ( luaL_loadbuffer( naevL, common_script, common_sz,
                            LUA_COMMON_PATH ) == 0 ) {
         if ( nlua_pcall( ref, 0, 0 ) != 0 ) {
            WARN( _( "Failed to run '%s':\n%s" ), LUA_COMMON_PATH,
                  lua_tostring( naevL, -1 ) );
            lua_pop( naevL, 1 );
         }
      } else {
         WARN( _( "Failed to load '%s':\n%s" ), LUA_COMMON_PATH,
               lua_tostring( naevL, -1 ) );
         lua_pop( naevL, 1 );
      }
   }

   lua_pop( naevL, 1 ); /* t */

   if ( conf.fpu_except )
      debug_enableFPUExcept();
   return ref;
}

/*
 * @brief Frees an environment created with nlua_newEnv()
 *
 *    @param env Environment to free.
 */
void nlua_freeEnv( nlua_env env )
{
   if ( naevL == NULL )
      return;
   if ( env == LUA_NOREF )
      return;

   /* Run gc script if applicable when removing env. */
   nlua_getenv( naevL, env, "__gc" );
   if ( !lua_isnil( naevL, -1 ) ) {
      if ( nlua_pcall( env, 0, 0 ) != 0 ) {
         WARN( "Error when running Lua __gc: %s", lua_tostring( naevL, -1 ) );
         lua_pop( naevL, 1 );
      }
   } else
      lua_pop( naevL, 1 );

   /* Remove from the environment table. */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, nlua_envs ); /* t */
   lua_pushnil( naevL );                               /* t, e */
   lua_rawseti( naevL, -2, env );                      /* t */
   lua_pop( naevL, 1 );                                /* */

   /* Unref. */
   luaL_unref( naevL, LUA_REGISTRYINDEX, env );
}

/*
 * @brief Push environment table to stack
 *
 *    @param env Environment.
 */
void nlua_pushenv( lua_State *L, nlua_env env )
{
   lua_rawgeti( L, LUA_REGISTRYINDEX, env );
}

/*
 * @brief Gets variable from environment and pushes it to stack
 *
 * This is meant to replace lua_getglobal()
 *
 *    @param env Environment.
 *    @param name Name of variable.
 */
void nlua_getenv( lua_State *L, nlua_env env, const char *name )
{
   nlua_pushenv( L, env );      /* env */
   lua_getfield( L, -1, name ); /* env, value */
   lua_remove( L, -2 );         /* value */
}

/*
 * @brief Pops a value from the stack and sets it in the environment.
 *
 * This is meant to replace lua_setglobal()
 *
 *    @param env Environment.
 *    @param name Name of variable.
 */
void nlua_setenv( lua_State *L, nlua_env env, const char *name )
{
   /* value */
   nlua_pushenv( L, env );      /* value, env */
   lua_insert( L, -2 );         /* env, value */
   lua_setfield( L, -2, name ); /* env */
   lua_pop( L, 1 );             /*  */
}

/*
 * @brief Registers C functions as lua library in environment
 *
 * This is meant to replace luaL_register()
 *
 *    @param env Environment.
 *    @param libname Name of library table.
 *    @param l Array of functions to register.
 *    @param metatable Library will be used as metatable (so register __index).
 */
void nlua_register( nlua_env env, const char *libname, const luaL_Reg *l,
                    int metatable )
{
   if ( luaL_newmetatable( naevL, libname ) ) {
      if ( metatable ) {
         lua_pushvalue( naevL, -1 );
         lua_setfield( naevL, -2, "__index" );
      }
      luaL_register( naevL, NULL, l );
   } /* lib */
   nlua_getenv( naevL, env, "naev" );  /* lib, naev */
   lua_pushvalue( naevL, -2 );         /* lib, naev, lib */
   lua_setfield( naevL, -2, libname ); /* lib, naev */
   lua_pop( naevL, 1 );                /* lib  */
   nlua_setenv( naevL, env, libname ); /* */
}

/**
 * @brief Wrapper around luaL_newstate.
 *
 *    @return A newly created lua_State.
 */
static lua_State *nlua_newState( void )
{
   /* Try to create the new state */
   lua_State *L = luaL_newstate();
   if ( L == NULL ) {
      WARN( _( "Failed to create new Lua state." ) );
      return NULL;
   }
   return L;
}

/**
 * @brief Loads specially modified basic stuff.
 *
 *    @param L Lua State to load the basic stuff into.
 *    @return 0 on success.
 */
static int nlua_loadBasic( lua_State *L )
{
   const char *override[] = { /* unsafe functions */
                              /*"collectgarbage",*/
                              "dofile", "getfenv", "load", "loadfile", NULL };

   luaL_openlibs( L );

   /* move [un]pack to table.[un]pack as in Lua5.2 */
   lua_getglobal( L, "table" );     /* t */
   lua_getglobal( L, "unpack" );    /* t, u */
   lua_setfield( L, -2, "unpack" ); /* t */
   lua_pop( L, 1 );                 /* */
   lua_pushnil( L );                /* nil */
   lua_setglobal( L, "unpack" );    /* */

   /* replace non-safe functions */
   for ( int i = 0; override[i] != NULL; i++ ) {
      lua_pushnil( L );
      lua_setglobal( L, override[i] );
   }

   /* Override built-ins to use Naev for I/O. */
   lua_register( L, "loadstring", luaB_loadstring );
   lua_register( L, "print", cli_print );
   lua_register( L, "printRaw", cli_printRaw );
   lua_register( L, "warn", cli_warn );

   /* Gettext functionality. */
   lua_register( L, "_", nlua_gettext );
   lua_register( L, "N_", nlua_gettext_noop );
   lua_register( L, "n_", nlua_ngettext );
   lua_register( L, "p_", nlua_pgettext );
   luaL_register( L, "gettext", gettext_methods );

   /* Sandbox "io" and "os". */
   lua_newtable( L ); /* io table */
   lua_setglobal( L, "io" );
   lua_newtable( L ); /* os table */
   lua_pushcfunction( L, nlua_os_getenv );
   lua_setfield( L, -2, "getenv" );
   lua_setglobal( L, "os" );

   /* Special math functions function. */
   lua_getglobal( L, "math" );
   lua_pushcfunction( L, nlua_log2 );
   lua_setfield( L, -2, "log2" );
   lua_pushnil( L );
   lua_setfield( L, -2, "mod" ); /* Get rid of math.mod */
   lua_pop( L, 1 );

   return 0;
}

/**
 * @brief Compares two Lua caches.
 */
static int lua_cache_cmp( const void *p1, const void *p2 )
{
   const LuaCache_t *lc1 = p1;
   const LuaCache_t *lc2 = p2;
   return strcmp( lc1->path, lc2->path );
}

static int nlua_package_preload( lua_State *L )
{
   const char *name = luaL_checkstring( L, 1 );

   lua_getglobal( L, "package" ); /* p */
   if ( !lua_istable( L, -1 ) ) {
      lua_pop( L, 1 );
      lua_pushstring( L, _( " package not found." ) );
      return 1;
   }

   lua_getfield( L, -1, "preload" ); /* p, p */
   lua_remove( L, -2 );              /* p */
   if ( !lua_istable( L, -1 ) )
      luaL_error( L, LUA_QL( "package.preload" ) " must be a table" );
   lua_getfield( L, -1, name );
   if ( lua_isnil( L, -1 ) ) /* not found? */
      lua_pushfstring( L, "\n\tno field package.preload['%s']", name );
   return 1;
}

/**
 * @brief load( string module ) -- searcher function to replace
 * package.loaders[2] (Lua 5.1), i.e., for Lua modules.
 *
 *    @param L Lua Environment.
 *    @return Stack depth (1), and on the stack: a loader function, a string
 * explaining there is none, or nil (no explanation).
 */
static int nlua_package_loader_lua( lua_State *L )
{
   LuaCache_t *lc;
   size_t      bufsize, l = 0;
   char       *buf = NULL;
   char        path_filename[PATH_MAX], tmpname[PATH_MAX], tried_paths[STRMAX];
   const char *packagepath, *start, *end;
   const char *name = luaL_checkstring( L, 1 );
   int         done = 0;

   /* Get paths to check. */
   lua_getglobal( L, "package" );
   if ( !lua_istable( L, -1 ) ) {
      lua_pop( L, 1 );
      lua_pushstring( L, _( " package not found." ) );
      return 1;
   }
   lua_getfield( L, -1, "path" );
   if ( !lua_isstring( L, -1 ) ) {
      lua_pop( L, 2 );
      lua_pushstring( L, _( " package.path not found." ) );
      return 1;
   }
   packagepath = lua_tostring( L, -1 );
   lua_pop( L, 2 );

   /* Parse path. */
   start = packagepath;
   while ( !done ) {
      char *q;
      end = strchr( start, ';' );
      if ( end == NULL ) {
         done = 1;
         end  = &start[strlen( start )];
      }
      strncpy( tmpname, start, end - start );
      tmpname[end - start] = '\0';
      q                    = strchr( tmpname, '?' );
      if ( q == NULL ) {
         snprintf( path_filename, sizeof( path_filename ), "%s%s", tmpname,
                   name );
      } else {
         *q = '\0';
         snprintf( path_filename, sizeof( path_filename ), "%s%s%s", tmpname,
                   name, q + 1 );
      }
      start = end + 1;

      /* Replace all '.' before the last '.' with '/' as they are a security
       * risk. */
      q = strrchr( path_filename, '.' );
      for ( int i = 0; i < q - path_filename; i++ )
         if ( path_filename[i] == '.' )
            path_filename[i] = '/';

      /* See if cached. */
      if ( L == naevL ) {
         const LuaCache_t lcq = { .path = path_filename };
         lc = bsearch( &lcq, lua_cache, array_size( lua_cache ),
                       sizeof( LuaCache_t ), lua_cache_cmp );
         if ( lc != NULL ) {
            lua_rawgeti( naevL, LUA_REGISTRYINDEX, lc->idx );
            return 1;
         }
      }

      /* Try to load the file. */
      if ( PHYSFS_exists( path_filename ) ) {
         buf = ndata_read( path_filename, &bufsize );
         if ( buf != NULL )
            break;
      }

      /* Didn't get to load it. */
      l += scnprintf( &tried_paths[l], sizeof( tried_paths ) - l,
                      _( "\n   no ndata path '%s'" ), path_filename );
   }

   /* Must have buf by now. */
   if ( buf == NULL ) {
      lua_pushstring( L, tried_paths );
      return 1;
   }

   /* Try to process the Lua. It will leave a function or message on the stack,
    * as required. */
   luaL_loadbuffer( L, buf, bufsize, path_filename );
   free( buf );

   /* Cache the result. */
   if ( L == naevL ) {
      lc       = &array_grow( &lua_cache );
      lc->path = strdup( path_filename );
      lua_pushvalue( L, -1 );
      lc->idx = luaL_ref( naevL, LUA_REGISTRYINDEX ); /* pops 1 */
      qsort( lua_cache, array_size( lua_cache ), sizeof( LuaCache_t ),
             lua_cache_cmp );
   }
   return 1;
}

/**
 * @brief load( string module ) -- searcher function to replace
 * package.loaders[3] (Lua 5.1), i.e., for C modules.
 *
 *    @param L Lua Environment.
 *    @return Stack depth (1), and on the stack: a loader function, a string
 * explaining there is none, or nil (no explanation).
 */
static int nlua_package_loader_c( lua_State *L )
{
   const char *name = luaL_checkstring( L, 1 );
   /* Hardcoded libraries only: we DO NOT honor package.cpath. */
   if ( strcmp( name, "utf8" ) == 0 )
      lua_pushcfunction( L, luaopen_utf8 );
   else if ( strcmp( name, "enet" ) == 0 && conf.lua_enet )
      lua_pushcfunction( L, luaopen_enet );
   else if ( strcmp( name, "cmark" ) == 0 )
      lua_pushcfunction( L, luaopen_cmark );
   else if ( strcmp( name, "yaml" ) == 0 )
      lua_pushcfunction( L, luaopen_yaml );
   else
      lua_pushnil( L );
   return 1;
}

/**
 * @brief load( string module ) -- searcher function to replace
 * package.loaders[4] (Lua 5.1), i.e., for C packages.
 *
 *    @param L Lua Environment.
 *    @return Stack depth (1), and on the stack: a loader function, a string
 * explaining there is none, or nil (no explanation).
 */
static int nlua_package_loader_croot( lua_State *L )
{
   lua_pushnil( L );
   return 1;
}

/**
 * @brief include( string module )
 *
 * Loads a module into the current Lua state from inside the data file.
 *
 *    @param L Lua Environment to load modules into.
 *    @return The return value of the chunk, or true.
 */
static int nlua_require( lua_State *L )
{
   const char *filename = luaL_checkstring( L, 1 );

   /* Environment table to load module into */
   int envtab = lua_upvalueindex( 1 );

   /* Check to see if already included. */
   // lua_getglobal( L, NLUA_LOAD_TABLE ); /* t */
   lua_getfield( L, envtab, NLUA_LOAD_TABLE ); /* t */
   if ( !lua_isnil( L, -1 ) ) {
      lua_getfield( L, -1, filename ); /* t, f */
      /* Already included. */
      if ( !lua_isnil( L, -1 ) ) {
         lua_remove( L, -2 ); /* val */
         return 1;
      }
      lua_pop( L, 2 ); /* */
   } else
      luaL_error( L, _( "_LOADED must be a table" ) );

   lua_getglobal( L, "package" ); /* p */
   if ( !lua_istable( L, -1 ) )
      luaL_error( L, _( "package must be a table" ) );
   lua_getfield( L, -1, "loaders" ); /* p, l */
   lua_remove( L, -2 );              /* l */
   if ( !lua_istable( L, -1 ) )
      luaL_error( L, _( "package.loaders must be a table" ) );
   lua_pushliteral( L, "" ); /* l, str */ /* error message accumulator */
   for ( int i = 1;; i++ ) {
      lua_rawgeti( L, -2, i ); /* l, str, i */ /* get a loader */
      if ( lua_isnil( L, -1 ) )
         luaL_error( L, _( "module '%s' not found:%s" ), filename,
                     lua_tostring( L, -2 ) );
      lua_pushstring( L, filename );       /* l, str, i, f */
      lua_call( L, 1, 1 ); /* l, str, r */ /* call it */
      if ( lua_isfunction( L, -1 ) )       /* did it find module? */
         break;                            /* module loaded successfully */
      else if ( lua_isstring( L, -1 ) )    /* loader returned error message? */
         lua_concat( L, 2 ); /* l, str */  /* accumulate it */
      else
         lua_pop( L, 1 ); /* l, str */
   }
   lua_remove( L, -2 ); /* l, r */
   lua_remove( L, -2 ); /* r */

   /* Set the environment for the call. */
   /* TODO this is wrong when using setfenv Lua-side... */
   lua_pushvalue( L, envtab ); /* r, e */
   lua_setfenv( L, -2 );       /* r */

   /* run the buffer */
   lua_pushstring( L, filename ); /* pass name as first parameter */
#if 0
   if (lua_pcall(L, 1, 1, 0) != 0) {
      /* will push the current error from the dobuffer */
      lua_error(L);
      return 1;
   }
#endif
   lua_call( L, 1, 1 ); /* val */

   /* Mark as loaded. */
   if ( lua_isnil( L, -1 ) ) {
      lua_pop( L, 1 );
      lua_pushboolean( L, 1 );
   }
   // lua_getglobal( L,NLUA_LOAD_TABLE ); /* val, t */
   lua_getfield( L, envtab, NLUA_LOAD_TABLE ); /* val, t */
   lua_pushvalue( L, -2 );                     /* val, t, val */
   lua_setfield( L, -2, filename );            /* val, t */
   lua_pop( L, 1 );                            /* val */

   /* cleanup, success */
   return 1;
}

/**
 * @brief Loads the standard Naev Lua API.
 *
 * Loads the modules:
 *  - naev
 *  - var
 *  - space
 *    - spob
 *    - system
 *    - jumps
 *  - time
 *  - player
 *  - pilot
 *  - rnd
 *  - diff
 *  - faction
 *  - vec2
 *  - outfit
 *  - commodity
 *
 * Only is missing:
 *  - misn
 *  - tk
 *  - hook
 *  - music
 *  - ai
 *
 *    @param env Environment.
 *    @return 0 on success.
 */
int nlua_loadStandard( nlua_env env )
{
   int r = 0;
   r |= nlua_loadNaev( env );
   r |= nlua_loadVar( env );
   r |= nlua_loadSpob( env );
   r |= nlua_loadSystem( env );
   r |= nlua_loadJump( env );
   r |= nlua_loadTime( env );
   r |= nlua_loadPlayer( env );
   r |= nlua_loadPilot( env );
   r |= nlua_loadRnd( env );
   r |= nlua_loadDiff( env );
   r |= nlua_loadFaction( env );
   r |= nlua_loadVector( env );
   r |= nlua_loadOutfit( env );
   r |= nlua_loadCommodity( env );
   r |= nlua_loadNews( env );
   r |= nlua_loadShiplog( env );
   r |= nlua_loadFile( env );
   r |= nlua_loadData( env );
   r |= nlua_loadLinOpt( env );
   r |= nlua_loadSafelanes( env );
   r |= nlua_loadSpfx( env );
   r |= nlua_loadAudio( env );

   return r;
}

/**
 * @brief Gets a trace from Lua.
 */
int nlua_errTrace( lua_State *L )
{
   return nlua_errTraceInternal( L, 1 );
}

static int nlua_errTraceInternal( lua_State *L, int idx )
{
   /* Handle special done case. */
   const char *str = lua_tostring( L, idx );
   if ( ( str != NULL ) && ( strcmp( str, NLUA_DONE ) == 0 ) )
      return 1;

   /* str */
   /* Otherwise execute "debug.traceback( str, int )". */
   lua_getglobal( L, "debug" ); /* str, debug */
   if ( !lua_istable( L, -1 ) ) {
      lua_pop( L, 1 );
      return 1;
   }
   lua_getfield( L, -1, "traceback" ); /* str, debug, traceback */
   if ( !lua_isfunction( L, -1 ) ) {
      lua_pop( L, 2 );
      return 1;
   }
   lua_pushvalue( L, idx ); /* str, debug, traceback, str */
   lua_call( L, 1, 1 );     /* str, debug, ret */
   lua_remove( L, -2 );     /* str, ret */
   lua_remove( L, -2 );     /* ret */
   return 1;
}

/*
 * @brief Wrapper around lua_pcall() that handles errors and environments
 *
 *    @param env Environment.
 *    @param nargs Number of arguments to pass.
 *    @param nresults Number of return values to take.
 */
int nlua_pcall( nlua_env env, int nargs, int nresults )
{
   int errf, ret, prev_env;

#if DEBUGGING
   errf = lua_gettop( naevL ) - nargs;
   lua_pushcfunction( naevL, nlua_errTrace );
   lua_insert( naevL, errf );

   /* We don't really want to do this, but Lua seems to trigger all sorts of
    * FPE exceptions on a daily basis.
    * TODO get rid of if possible. */
   if ( conf.fpu_except )
      debug_disableFPUExcept();
#else  /* DEBUGGING */
   errf = 0;
#endif /* DEBUGGING */

   prev_env      = __NLUA_CURENV;
   __NLUA_CURENV = env;

   nlua_pushenv( naevL, env );
   lua_setfenv( naevL, -2 - nargs );
   ret = lua_pcall( naevL, nargs, nresults, errf );

   __NLUA_CURENV = prev_env;

#if DEBUGGING
   lua_remove( naevL, errf );

   if ( conf.fpu_except )
      debug_enableFPUExcept();
#endif /* DEBUGGING */

   return ret;
}

/**
 * @brief Gets the reference of a global in a lua environment.
 *
 *    @param env Environment.
 *    @param name Name of the global to get.
 *    @return LUA_NOREF if no global found, reference otherwise.
 */
int nlua_refenv( nlua_env env, const char *name )
{
   nlua_getenv( naevL, env, name );
   if ( !lua_isnil( naevL, -1 ) )
      return luaL_ref( naevL, LUA_REGISTRYINDEX );
   lua_pop( naevL, 1 );
   return LUA_NOREF;
}

/**
 * @brief Gets the reference of a global in a lua environment if it matches a
 * type.
 *
 *    @param env Environment.
 *    @param name Name of the global to get.
 *    @param type Type to match, e.g., LUA_TFUNCTION.
 *    @return LUA_NOREF if no global found, reference otherwise.
 */
int nlua_refenvtype( nlua_env env, const char *name, int type )
{
   nlua_getenv( naevL, env, name );
   if ( lua_type( naevL, -1 ) == type )
      return luaL_ref( naevL, LUA_REGISTRYINDEX );
   lua_pop( naevL, 1 );
   return LUA_NOREF;
}

/**
 * @brief Gets the reference to the specified field from an object reference.
 *
 *    @param objref Reference to the object to be indexed.
 *    @param name Name of the field to get.
 *    @return LUA_NOREF if no field found, reference otherwise.
 */
int nlua_reffield( int objref, const char *name )
{
   if ( objref == LUA_NOREF )
      return LUA_NOREF;
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, objref );
   lua_getfield( naevL, -1, name );
   lua_remove( naevL, -2 );
   if ( !lua_isnil( naevL, -1 ) )
      return luaL_ref( naevL, LUA_REGISTRYINDEX );
   lua_pop( naevL, 1 );
   return LUA_NOREF;
}

void nlua_dumpstack( lua_State *L )
{
   int top = lua_gettop( L );
   for ( int i = 1; i <= top; i++ ) {
      LOG( "%d\t%s\t", i, luaL_typename( L, i ) );
      switch ( lua_type( L, i ) ) {
      case LUA_TNUMBER:
         LOG( "%g\n", lua_tonumber( L, i ) );
         break;
      case LUA_TSTRING:
         LOG( "%s\n", lua_tostring( L, i ) );
         break;
      case LUA_TBOOLEAN:
         LOG( "%s\n", ( lua_toboolean( L, i ) ? "true" : "false" ) );
         break;
      case LUA_TNIL:
         LOG( "%s\n", "nil" );
         break;
      default:
         LOG( "%p\n", lua_topointer( L, i ) );
         break;
      }
   }
}

/**
 * @brief Creates a new reference to a Lua structure at a position.
 */
int nlua_ref( lua_State *L, int idx )
{
   lua_pushvalue( L, idx );
   return luaL_ref( L, LUA_REGISTRYINDEX );
}

/**
 * @brief Removes a reference set with nlua_ref.
 */
void nlua_unref( lua_State *L, int idx )
{
   if ( idx != LUA_NOREF )
      luaL_unref( L, LUA_REGISTRYINDEX, idx );
}

/**
 * @brief Propagates a resize event to all the environments forcibly.
 */
void nlua_resize( void )
{
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, nlua_envs ); /* t */
   lua_pushnil( naevL );                               /* t, n */
   while ( lua_next( naevL, -2 ) != 0 ) {              /* t, k, v */
      int env = lua_tointeger( naevL, -2 );            /* t, k, v */
      lua_getfield( naevL, -1, "__resize" );           /* t, k, v, f */
      if ( !lua_isnil( naevL, -1 ) ) {
         lua_pushinteger( naevL, SCREEN_W ); /* t, k, v, f, w */
         lua_pushinteger( naevL, SCREEN_H ); /* t, k, v, f, w, h */
         nlua_pcall( env, 2, 0 );            /* t, k, v */
         lua_pop( naevL, 1 );                /* t, k */
      } else
         lua_pop( naevL, 2 ); /* t, k */
   } /* t */
   lua_pop( naevL, 1 ); /* */
}

/**
 * @brief Helper function to deal with tags.
 */
int nlua_helperTags( lua_State *L, int idx, char *const *tags )
{
   if ( lua_isnoneornil( L, idx ) ) {
      lua_newtable( L );
      for ( int i = 0; i < array_size( tags ); i++ ) {
         lua_pushstring( L, tags[i] );
         lua_pushboolean( L, 1 );
         lua_rawset( L, -3 );
      }
      return 1;
   } else {
      const char *s = luaL_checkstring( L, idx );
      for ( int i = 0; i < array_size( tags ); i++ ) {
         if ( strcmp( s, tags[i] ) == 0 ) {
            lua_pushboolean( L, 1 );
            return 1;
         }
      }
      lua_pushboolean( L, 0 );
      return 1;
   }
}
