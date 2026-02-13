/*
 * See Licensing and Copyright notice in naev.h
 */
#pragma once

#include <stdint.h>

#include "colour.h"
#include "nlua.h"
#include "opengl_tex.h"
#include "space_fdecl.h"

extern int64_t faction_player;

#define FACTION_PLAYER                                                         \
   faction_player          /**< Hardcoded player faction identifier. */
#define FACTION_LOGO_SM 64 /**< Size of "small" logo. */

typedef struct FactionGenerator_ {
   int64_t id;     /**< Id of the generator. */
   double  weight; /**< Weight modifier. */
} FactionGenerator;

/* Get stuff */
int                     faction_isFaction( int64_t f );
int64_t                 faction_exists( const char *name );
int64_t                 faction_get( const char *name );
int64_t                *faction_getAll( void );
int64_t                *faction_getAllVisible( void );
int64_t                *faction_getKnown();
int                     faction_isStatic( int64_t id );
int                     faction_isInvisible( int64_t id );
int                     faction_setInvisible( int64_t id, int state );
int                     faction_isKnown( int64_t id );
int                     faction_isDynamic( int64_t id );
const char             *faction_name( int64_t f );
const char             *faction_shortname( int64_t f );
const char             *faction_longname( int64_t f );
const char             *faction_mapname( int64_t f );
const char             *faction_description( int64_t f );
const char             *faction_default_ai( int64_t f );
const char *const      *faction_tags( int64_t f );
double                  faction_lane_length_per_presence( int64_t f );
double                  faction_lane_base_cost( int64_t f );
void                    faction_clearEnemy( int64_t f );
void                    faction_addEnemy( int64_t f, int64_t o );
void                    faction_rmEnemy( int64_t f, int64_t o );
void                    faction_clearAlly( int64_t f );
void                    faction_addAlly( int64_t f, int64_t o );
void                    faction_rmAlly( int64_t f, int64_t o );
void                    faction_addNeutral( int64_t f, int64_t o );
void                    faction_rmNeutral( int64_t f, int64_t o );
nlua_env               *faction_getScheduler( int64_t f );
nlua_env               *faction_getEquipper( int64_t f );
const glTexture        *faction_logo( int64_t f );
const glColour         *faction_colour( int64_t f );
const int64_t          *faction_getEnemies( int64_t f );
const int64_t          *faction_getAllies( int64_t f );
int64_t                *faction_getGroup( int which );
int                     faction_usesHiddenJumps( int64_t f );
const FactionGenerator *faction_generators( int64_t f );

/* Set stuff */
int    faction_setKnown( int64_t id, int state );
double faction_reputationOverride( int64_t f, int *set );
void   faction_setReputationOverride( int64_t f, int set, double value );

/* player stuff */
double faction_hit( int64_t f, const StarSystem *sys, double mod,
                    const char *source, int single );
double faction_hitTest( int64_t f, const StarSystem *sys, double mod,
                        const char *source );
void   faction_modPlayer( int64_t f, double mod, const char *source );
void   faction_modPlayerSingle( int64_t f, double mod, const char *source );
void   faction_modPlayerRaw( int64_t f, double mod );
void   faction_setReputation( int64_t f, double value );
double faction_reputation( int64_t f );
double faction_reputationDefault( int64_t f );
int    faction_isPlayerFriend( int64_t f );
int    faction_isPlayerEnemy( int64_t f );
int    faction_isPlayerFriendSystem( int64_t f, const StarSystem *sys );
int    faction_isPlayerEnemySystem( int64_t f, const StarSystem *sys );
const char     *faction_getStandingText( int64_t f );
const char     *faction_getStandingTextAtValue( int64_t f, double value );
const char     *faction_getStandingBroad( int64_t f, int bribed, int override );
double          faction_reputationMax( int64_t f );
const glColour *faction_reputationColour( int64_t f );
char            faction_reputationColourChar( int64_t f );
const glColour *faction_reputationColourSystem( int64_t           f,
                                                const StarSystem *sys );
char faction_reputationColourCharSystem( int64_t f, const StarSystem *sys );
void faction_applyLocalThreshold( int64_t f, StarSystem *sys );
void faction_updateSingle( int64_t f );
void faction_updateGlobal( void );

/* Works with only factions */
int areEnemies( int64_t a, int64_t b );
int areNeutral( int64_t a, int64_t b );
int areAllies( int64_t a, int64_t b );
int areEnemiesSystem( int64_t a, int64_t b, const StarSystem *sys );
int areAlliesSystem( int64_t a, int64_t b, const StarSystem *sys );

/* load/free */
int  factions_load( void );
int  factions_loadPost( void );
void factions_free( void );
void factions_reset( void );
void factions_resetLocal( void );
void factions_cleanLocal( void );
void faction_clearKnown( void );

/* Dynamic factions. */
void    factions_clearDynamic( void );
int64_t faction_dynAdd( int64_t base, const char *name, const char *display,
                        const char *ai, const glColour *colour );
