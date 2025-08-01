/*
 * See Licensing and Copyright notice in naev.h
 */
/**
 * @file pilot_outfit.c
 *
 * @brief Handles pilot outfits.
 */
/** @cond */
#include "naev.h"
/** @endcond */

#include "array.h"
#include "constants.h"
#include "difficulty.h"
#include "escort.h"
#include "gui.h"
#include "land.h"
#include "log.h"
#include "nlua.h"
#include "nlua_outfit.h"
#include "nlua_pilot.h"
#include "nlua_pilotoutfit.h"
#include "nlua_vec2.h"
#include "nstring.h"
#include "ntracing.h"
#include "outfit.h"
#include "pilot.h"
#include "player.h"
#include "slots.h"
#include "space.h"

static int stealth_break = 0; /**< Whether or not to break stealth. */

/*
 * Prototypes.
 */
static void        pilot_calcStatsSlot( Pilot *pilot, PilotOutfitSlot *slot,
                                        ShipStats *s );
static const char *outfitkeytostr( OutfitKey key );

/**
 * @brief Updates the lockons on the pilot's launchers
 *
 *    @param p Pilot being updated.
 *    @param o Slot being updated.
 *    @param t Pilot that is currently the target of p (or NULL if not
 * applicable).
 *    @param wt Pilot's target.
 *    @param a Angle to update if necessary. Should be initialized to -1 before
 * the loop.
 *    @param dt Current delta tick.
 */
void pilot_lockUpdateSlot( Pilot *p, PilotOutfitSlot *o, Pilot *t, Target *wt,
                           double *a, double dt )
{
   double arc, max;
   int    locked;

   /* No target. */
   if ( wt->type == TARGET_NONE )
      return;

   /* Nota  seeker. */
   if ( !outfit_isSeeker( o->outfit ) )
      return;

   /* Check arc. */
   arc = outfit_launcherArc( o->outfit );
   if ( arc > 0. ) {

      /* We use an external variable to set and update the angle if necessary.
       */
      if ( *a < 0. ) {
         double x, y, ang;
         if ( wt->type == TARGET_PILOT ) {
            x = t->solid.pos.x - p->solid.pos.x;
            y = t->solid.pos.y - p->solid.pos.y;
         } else if ( wt->type == TARGET_ASTEROID ) {
            const Asteroid *ast = &cur_system->asteroids[wt->u.ast.anchor]
                                      .asteroids[wt->u.ast.asteroid];
            x = ast->sol.pos.x - p->solid.pos.x;
            y = ast->sol.pos.y - p->solid.pos.y;
         } else {
            x = y = 0.;
         }
         ang = ANGLE( x, y );
         *a  = FABS( angle_diff( ang, p->solid.dir ) );
      }

      /* Decay if not in arc. */
      if ( *a > arc ) {
         /* When a lock is lost, immediately gain half the lock timer.
          * This is meant as an incentive for the aggressor not to lose the
          * lock, and for the target to try and break the lock. */
         double old = o->u.ammo.lockon_timer;
         /* Limit decay to the lock-on time for this launcher. */
         max = outfit_launcherLockon( o->outfit );
         o->u.ammo.lockon_timer += dt;
         if ( ( old <= 0. ) && ( o->u.ammo.lockon_timer > 0. ) )
            o->u.ammo.lockon_timer += max / 2.;

         /* Cap at max. */
         if ( o->u.ammo.lockon_timer > max )
            o->u.ammo.lockon_timer = max;

         /* Out of arc. */
         o->u.ammo.in_arc = 0;
         return;
      }
   }

   /* In arc. */
   o->u.ammo.in_arc = 1;
   locked           = ( o->u.ammo.lockon_timer < 0. );

   /* Lower timer. When the timer reaches zero, the lock is established. */
   max = -outfit_launcherLockon( o->outfit ) / 3.;
   if ( o->u.ammo.lockon_timer > max ) {
      /* Targetting is linear and can't be faster than the time specified (can
       * be slower though). */
      double mod = pilot_ewWeaponTrack( p, t, outfit_trackmin( o->outfit ),
                                        outfit_trackmax( o->outfit ) );
      if ( p->stats.launch_lockon <= 0. )
         o->u.ammo.lockon_timer = max;
      else
         o->u.ammo.lockon_timer -= dt * mod / p->stats.launch_lockon;

      /* Cap at -max/3. */
      if ( o->u.ammo.lockon_timer < max )
         o->u.ammo.lockon_timer = max;

      /* Trigger lockon hook. */
      if ( !locked && ( o->u.ammo.lockon_timer < 0. ) )
         pilot_runHook( p, PILOT_HOOK_LOCKON );
   }
}

/**
 * @brief Clears pilot's missile lockon timers.
 *
 *    @param p Pilot to clear missile lockon timers.
 */
void pilot_lockClear( Pilot *p )
{
   for ( int i = 0; i < array_size( p->outfits ); i++ ) {
      PilotOutfitSlot *o = p->outfits[i];
      if ( o->outfit == NULL )
         continue;
      if ( !outfit_isSeeker( o->outfit ) )
         continue;

      /* Clear timer. */
      o->u.ammo.lockon_timer = outfit_launcherLockon( o->outfit );

      /* Clear arc. */
      o->u.ammo.in_arc = 0;
   }
}

/**
 * @brief Gets the mount position of a pilot.
 *
 * Position is relative to the pilot.
 *
 *    @param p Pilot to get mount position of.
 *    @param w Slot of the mount.
 *    @param[out] v Position of the mount.
 *    @return 0 on success.
 */
int pilot_getMount( const Pilot *p, const PilotOutfitSlot *w, vec2 *v )
{
   double           x, y;
   const ShipMount *m = &w->sslot->mount;

   if ( ship_isFlag( p->ship, SHIP_3DMOUNTS ) ) {
      vec3 v2;
      mat4 H = pilot_local_transform( p );
      mat4_mul_vec( &v2, &H, &m->pos );
      x = v2.v[0];
      y = v2.v[1];
   } else {
      double cm, sm;
      cm = cos( p->solid.dir );
      sm = sin( p->solid.dir );
      x  = m->pos.v[0] * cm - m->pos.v[1] * sm;
      y  = m->pos.v[0] * sm + m->pos.v[1] * cm + m->pos.v[2];
      y *= M_SQRT1_2;
   }

   /* Get the mount and add the player.p offset. */
   vec2_cset( v, x, y );
   return 0;
}

/**
 * @brief Docks the pilot on its target pilot.
 *
 *    @param p Pilot that wants to dock.
 *    @param target Pilot to dock on.
 *    @return 0 on successful docking.
 */
int pilot_dock( Pilot *p, Pilot *target )
{
   int              i;
   PilotOutfitSlot *dockslot;

   /* Must belong to target */
   if ( p->dockpilot != target->id )
      return -1;

   /* Must have a dockslot */
   dockslot = pilot_getDockSlot( p );
   if ( dockslot == NULL )
      return -1;

   /* Must be close. */
   if ( vec2_dist( &p->solid.pos, &target->solid.pos ) >
        target->ship->size * PILOT_SIZE_APPROX )
      return -1;

   /* Cannot be going much faster. */
   if ( vec2_dist2( &p->solid.vel, &target->solid.vel ) >
        pow2( MAX_HYPERSPACE_VEL ) )
      return -1;

   /* Grab dock ammo */
   i = p->dockslot;

   /* Try to add fighter. */
   dockslot->u.ammo.deployed--;
   p->dockpilot = 0;
   p->dockslot  = -1;

   /* Add the pilot's outfit. */
   if ( pilot_addAmmo( target, target->outfits[i], 1 ) != 1 )
      WARN( _( "Unable to add ammo to '%s' from docking pilot '%s'!" ),
            target->name, p->name );

   /* Remove from pilot's escort list. */
   for ( i = 0; i < array_size( target->escorts ); i++ ) {
      if ( ( target->escorts[i].type == ESCORT_TYPE_BAY ) &&
           ( target->escorts[i].id == p->id ) )
         break;
   }
   /* Not found as pilot's escorts. */
   if ( i >= array_size( target->escorts ) )
      WARN( _( "Docking pilot '%s' not found in pilot '%s's escort list!" ),
            target->name, p->name );
   /* Remove escort pilot. */
   escort_rmListIndex( target, i );

   /* Destroy the pilot. */
   pilot_delete( p );

   return 0;
}

/**
 * @brief Checks to see if the pilot has deployed ships.
 *
 *    @param p Pilot to see if has deployed ships.
 *    @return 1 if pilot has deployed ships, 0 otherwise.
 */
int pilot_hasDeployed( const Pilot *p )
{
   for ( int i = 0; i < array_size( p->outfits ); i++ ) {
      if ( p->outfits[i]->outfit == NULL )
         continue;
      if ( outfit_isFighterBay( p->outfits[i]->outfit ) )
         if ( p->outfits[i]->u.ammo.deployed > 0 )
            return 1;
   }
   return 0;
}

/**
 * @brief Adds an outfit to the pilot, ignoring CPU or other limits.
 *
 * @note Does not call pilot_calcStats().
 *
 *    @param pilot Pilot to add the outfit to.
 *    @param outfit Outfit to add to the pilot.
 *    @param s Slot to add ammo to.
 *    @return 0 on success.
 */
int pilot_addOutfitRaw( Pilot *pilot, const Outfit *outfit, PilotOutfitSlot *s )
{
   pilot_addOutfitRawNoLua( pilot, outfit, s );

   /* Initialize if active thingy if necessary. */
   pilot_outfitLAdd( pilot, s );
   pilot_outfitLOutfitChange( pilot );

   return 0;
}

/**
 * @brief Same as pilot_addOutfitRaw, but without running Lua.
 */
int pilot_addOutfitRawNoLua( Pilot *pilot, const Outfit *outfit,
                             PilotOutfitSlot *s )
{
   /* Set the outfit. */
   s->flags  = 0;
   s->state  = PILOT_OUTFIT_OFF;
   s->outfit = outfit;

   /* Set some default parameters. */
   s->timer = 0.;

   /* Some per-case scenarios. */
   if ( outfit_isFighterBay( outfit ) ) {
      s->u.ammo.quantity = 0;
      s->u.ammo.deployed = 0;
      pilot->nfighterbays++;
   } else if ( outfit_isTurret( outfit ) ) /* used to speed up AI */
      pilot->nturrets++;
   else if ( outfit_isBolt( outfit ) )
      pilot->ncannons++;
   else if ( outfit_isAfterburner( outfit ) )
      pilot->nafterburners++;
   if ( outfit_isLauncher( outfit ) ) {
      s->u.ammo.quantity = 0;
      s->u.ammo.deployed = 0; /* Just in case. */
   }

   if ( outfit_isBeam( outfit ) ) { /* Used to speed up some calculations. */
      s->u.beamid = 0;
      pilot->nbeams++;
   }

   /* Check if active. */
   if ( outfit_isActive( outfit ) )
      s->flags |= PILOTOUTFIT_ACTIVE;
   else
      s->flags &= ~PILOTOUTFIT_ACTIVE;

   /* Check if toggleable. */
   if ( outfit_isToggleable( outfit ) )
      s->flags |= PILOTOUTFIT_TOGGLEABLE;
   else
      s->flags &= ~PILOTOUTFIT_TOGGLEABLE;

   /* Disable lua for now. */
   s->lua_mem = LUA_NOREF;
   ss_free( s->lua_stats ); /* Just in case. */
   s->lua_stats = NULL;

   return 0;
}

/**
 * @brief Tests to see if an outfit can be added.
 *
 *    @param pilot Pilot to add outfit to.
 *    @param outfit Outfit to add.
 *    @param s Slot adding outfit to.
 *    @param warn Whether or not should generate a warning.
 *    @return 0 if can add, -1 if can't.
 */
int pilot_addOutfitTest( Pilot *pilot, const Outfit *outfit,
                         const PilotOutfitSlot *s, int warn )
{
   const char *str;

   /* See if slot has space. */
   if ( s->outfit != NULL ) {
      if ( warn )
         WARN( _( "Pilot '%s': trying to add outfit '%s' to slot that already "
                  "has an outfit" ),
               pilot->name, outfit_name( outfit ) );
      return -1;
   } else if ( ( outfit_cpu( outfit ) < 0 ) &&
               ( pilot->cpu < ABS( outfit_cpu( outfit ) ) ) ) {
      if ( warn )
         WARN( _( "Pilot '%s': Not enough CPU to add outfit '%s'" ),
               pilot->name, outfit_name( outfit ) );
      return -1;
   } else if ( ( str = pilot_canEquip( pilot, s, outfit ) ) != NULL ) {
      if ( warn )
         WARN( _( "Pilot '%s': Trying to add outfit but %s" ), pilot->name,
               str );
      return -1;
   }
   return 0;
}

/**
 * @brief Adds an outfit to the pilot.
 *
 *    @param pilot Pilot to add the outfit to.
 *    @param outfit Outfit to add to the pilot.
 *    @param s Slot to add ammo to.
 *    @return 0 on success.
 */
int pilot_addOutfit( Pilot *pilot, const Outfit *outfit, PilotOutfitSlot *s )
{
   /* Test to see if outfit can be added. */
   int ret = pilot_addOutfitTest( pilot, outfit, s, 1 );
   if ( ret != 0 )
      return -1;

   /* Add outfit. */
   ret = pilot_addOutfitRaw( pilot, outfit, s );

   /* Recalculate the stats */
   pilot_calcStats( pilot );

   return ret;
}

/**
 * @brief Adds an outfit as an intrinsic slot.
 */
int pilot_addOutfitIntrinsicRaw( Pilot *pilot, const Outfit *outfit )
{
   PilotOutfitSlot *s;

   if ( !outfit_isMod( outfit ) ) {
      WARN( _( "Instrinsic outfits must be modifiers!" ) );
      return -1;
   }

   if ( pilot->outfit_intrinsic == NULL )
      pilot->outfit_intrinsic = array_create( PilotOutfitSlot );

   s = &array_grow( &pilot->outfit_intrinsic );
   memset( s, 0, sizeof( PilotOutfitSlot ) );
   return pilot_addOutfitRaw( pilot, outfit, s );
}

/**
 * @brief Adds an outfit as an intrinsic slot.
 */
int pilot_addOutfitIntrinsic( Pilot *pilot, const Outfit *outfit )
{
   PilotOutfitSlot *s;
   int              ret;

   if ( !outfit_isMod( outfit ) ) {
      WARN( _( "Instrinsic outfits must be modifiers!" ) );
      return -1;
   }

   if ( pilot->outfit_intrinsic == NULL )
      pilot->outfit_intrinsic = array_create( PilotOutfitSlot );

   s = &array_grow( &pilot->outfit_intrinsic );
   memset( s, 0, sizeof( PilotOutfitSlot ) );
   ret = pilot_addOutfitRaw( pilot, outfit, s );
   if ( pilot->id > 0 )
      pilot_outfitLInit( pilot, s );

   return ret;
}

/**
 * @brief Removes an outfit from an intrinsic slot.
 */
int pilot_rmOutfitIntrinsic( Pilot *pilot, const Outfit *outfit )
{
   int ret = 0;
   for ( int i = 0; i < array_size( pilot->outfit_intrinsic ); i++ ) {
      PilotOutfitSlot *s = &pilot->outfit_intrinsic[i];
      if ( s->outfit != outfit )
         continue;
      ret = pilot_rmOutfitRaw( pilot, s );
      array_erase( &pilot->outfit_intrinsic, s, s + 1 );
      break;
   }
   /* Recalculate the stats */
   if ( ret )
      pilot_calcStats( pilot );
   return ret;
}

/**
 * @brief Gets how many copies of an intrinsic a pilot has.
 */
int pilot_hasIntrinsic( const Pilot *pilot, const Outfit *outfit )
{
   int ret = 0;
   for ( int i = 0; i < array_size( pilot->outfit_intrinsic ); i++ ) {
      const PilotOutfitSlot *s = &pilot->outfit_intrinsic[i];
      if ( s->outfit != outfit )
         continue;
      ret++;
   }
   return ret;
}

/**
 * @brief Removes an outfit from the pilot without doing any checks.
 *
 * @note Does not run pilot_calcStats().
 *
 *    @param pilot Pilot to remove the outfit from.
 *    @param s Slot to remove.
 *    @return 0 on success.
 */
int pilot_rmOutfitRaw( Pilot *pilot, PilotOutfitSlot *s )
{
   int ret;

   /* Force turn off if necessary. */
   if ( s->state == PILOT_OUTFIT_ON )
      pilot_outfitOff( pilot, s, 0 );

   /* Run remove hook if necessary. */
   pilot_outfitLRemove( pilot, s );

   /* Decrement counters if necessary. */
   if ( s->outfit != NULL ) {
      if ( outfit_isTurret( s->outfit ) )
         pilot->nturrets--;
      else if ( outfit_isBolt( s->outfit ) )
         pilot->ncannons--;
      else if ( outfit_isAfterburner( s->outfit ) )
         pilot->nafterburners--;
      else if ( outfit_isFighterBay( s->outfit ) )
         pilot->nfighterbays--;
      if ( outfit_isBeam( s->outfit ) )
         pilot->nbeams--;
   }

   /* Remove the outfit. */
   ret       = ( s->outfit == NULL );
   s->outfit = NULL;
   s->flags  = 0; /* Clear flags. */
   // s->weapset  = -1;

   /* Remove secondary and such if necessary. */
   if ( pilot->afterburner == s )
      pilot->afterburner = NULL;

   /* Clear Lua if necessary. */
   if ( s->lua_mem != LUA_NOREF ) {
      luaL_unref( naevL, LUA_REGISTRYINDEX, s->lua_mem );
      s->lua_mem = LUA_NOREF;
   }

   /* Clean up stats. */
   ss_free( s->lua_stats );
   s->lua_stats = NULL;

   /* Outfit changed. */
   pilot_outfitLOutfitChange( pilot );

   return ret;
}

/**
 * @brief Tries to add an outfit to the first possible free slot on the pilot.
 */
int pilot_addOutfitRawAnySlotNoLua( Pilot *p, const Outfit *o )
{
   /* Test special slots first. */
   for ( int spid = 1; spid >= 0; spid-- ) {
      /* Try to find the first smallest size it fits into. */
      for ( OutfitSlotSize size = outfit_slotSize( o );
            size <= OUTFIT_SLOT_SIZE_HEAVY; size++ ) {
         for ( int i = 0; i < array_size( p->outfits ); i++ ) {
            PilotOutfitSlot *s = p->outfits[i];

            /* Must match special property status. */
            if ( spid != ( !!s->sslot->slot.spid ) )
               continue;

            /* Must be correct size. */
            if ( s->sslot->slot.size != size )
               continue;

            /* Must not be full already. */
            if ( s->outfit != NULL )
               continue;

            /* Test. */
            if ( !outfit_fitsSlot( o, &s->sslot->slot ) )
               continue;

            /* Try to add. */
            if ( pilot_addOutfitRawNoLua( p, o, s ) == 0 )
               return i;
         }
      }
   }
   return -1;
}

/**
 * @brief Removes an outfit from the pilot.
 *
 *    @param pilot Pilot to remove the outfit from.
 *    @param s Slot to remove.
 *    @return 0 on success.
 */
int pilot_rmOutfit( Pilot *pilot, PilotOutfitSlot *s )
{
   int         ret;
   const char *str = pilot_canEquip( pilot, s, NULL );
   if ( str != NULL ) {
      WARN( _( "Pilot '%s': Trying to remove outfit but %s" ), pilot->name,
            str );
      return -1;
   }

   ret = pilot_rmOutfitRaw( pilot, s );

   /* recalculate the stats */
   pilot_calcStats( pilot );

   return ret;
}

/**
 * @brief Pilot slot safety check - makes sure stats are safe.
 *
 *    @param p Pilot to check.
 *    @return 0 if a slot doesn't fit, !0 otherwise.
 */
int pilot_slotsCheckSafety( const Pilot *p )
{
   for ( int i = 0; i < array_size( p->outfits ); i++ )
      if ( ( p->outfits[i]->outfit != NULL ) &&
           !outfit_fitsSlot( p->outfits[i]->outfit,
                             &p->outfits[i]->sslot->slot ) )
         return 0;
   return 1;
}

/**
 * @brief Pilot required (core) slot filled check - makes sure they are filled.
 *
 *    @param p Pilot to check.
 *    @return 0 if a slot is missing, !0 otherwise.
 */
int pilot_slotsCheckRequired( const Pilot *p )
{
   for ( int i = 0; i < array_size( p->outfits ); i++ )
      if ( p->outfits[i]->sslot->required && p->outfits[i]->outfit == NULL )
         return 0;
   return 1;
}

/**
 * @brief Pilot safety check - makes sure stats are safe.
 *
 *    @param p Pilot to check.
 *    @return The reason why the pilot is not safe (or NULL if safe).
 */
int pilot_isSpaceworthy( const Pilot *p )
{
   return !pilot_reportSpaceworthy( p, NULL, 0 );
}

/**
 * @brief Pilot safety report - makes sure stats are safe.
 *
 *    @param p Pilot to check.
 *    @param buf Buffer to fill.
 *    @param bufSize Size of the buffer.
 *    @return Number of issues encountered.
 */
int pilot_reportSpaceworthy( const Pilot *p, char *buf, int bufSize )
{
#define SPACEWORTHY_CHECK( cond, msg )                                         \
   if ( cond ) {                                                               \
      ret++;                                                                   \
      if ( buf != NULL ) {                                                     \
         if ( pos > 0 )                                                        \
            pos += scnprintf( &buf[pos], bufSize - pos, "\n" );                \
         pos += scnprintf( &buf[pos], bufSize - pos, ( msg ) );                \
      }                                                                        \
   }
   int pos = 0;
   int ret = 0;

   /* Core Slots */
   SPACEWORTHY_CHECK( !pilot_slotsCheckRequired( p ),
                      _( "!! Not All Core Slots are equipped" ) );
   /* CPU. */
   SPACEWORTHY_CHECK( p->cpu < 0, _( "!! Insufficient CPU" ) );

   /* Movement. */
   SPACEWORTHY_CHECK( p->accel < 0, _( "!! Insufficient Accel" ) );
   SPACEWORTHY_CHECK( p->speed < 0, _( "!! Insufficient Speed" ) );
   SPACEWORTHY_CHECK( p->turn < 0, _( "!! Insufficient Turn" ) );
   SPACEWORTHY_CHECK( p->stats.time_speedup <= 0.,
                      _( "!! Defies Laws of Physics" ) );

   /* Health. */
   SPACEWORTHY_CHECK( p->armour < 0., _( "!! Insufficient Armour" ) );
   SPACEWORTHY_CHECK( p->armour_regen < 0.,
                      _( "!! Insufficient Armour Regeneration" ) );
   SPACEWORTHY_CHECK( p->shield < 0., _( "!! Insufficient Shield" ) );
   SPACEWORTHY_CHECK( p->shield_regen < 0.,
                      _( "!! Insufficient Shield Regeneration" ) );
   SPACEWORTHY_CHECK( p->energy_max < 0., _( "!! Insufficient Energy" ) );
   SPACEWORTHY_CHECK( p->energy_regen <= 0.,
                      _( "!! Insufficient Energy Regeneration" ) );

   /* Misc. */
   SPACEWORTHY_CHECK( p->fuel_max < 0, _( "!! Insufficient Fuel Maximum" ) );
   SPACEWORTHY_CHECK( p->fuel_consumption < 0,
                      _( "!! Insufficient Fuel Consumption" ) );
   SPACEWORTHY_CHECK( p->cargo_free < 0,
                      _( "!! Insufficient Free Cargo Space" ) );
   SPACEWORTHY_CHECK( p->crew < 0, _( "!! Insufficient Crew" ) );
   SPACEWORTHY_CHECK( pilot_massFactor( p ) < 0.05, _( "!! Too Heavy" ) );
   SPACEWORTHY_CHECK( p->solid.mass <= 0., _( "!! Defies Laws of Physics" ) );

   /* No need to mess with the string. */
   if ( buf == NULL )
      return ret;

   /* Buffer is full, lets write that there is more then what's copied */
   if ( pos > bufSize - 1 ) {
      buf[bufSize - 4] = '.';
      buf[bufSize - 3] = '.';
      buf[bufSize - 2] = '.';
      /* buf[bufSize-1]='\0'; already done for us */
   } else if ( pos == 0 ) {
      /* String is empty so no errors encountered */
      pos += scnprintf( buf, bufSize, _( "Spaceworthy" ) );
      if ( ship_isFlag( p->ship, SHIP_NOPLAYER ) )
         pos += scnprintf( &buf[pos], bufSize - pos, "\n#o%s#0",
                           _( "Escort only" ) );
      if ( ship_isFlag( p->ship, SHIP_NOESCORT ) )
         /* pos +=*/scnprintf( &buf[pos], bufSize - pos, "\n#o%s#0",
                               _( "Lead ship only" ) );
   }

   return ret;
}
#undef SPACEWORTHY_CHECK

/**
 * @brief Checks to see if a pilot has an outfit with a specific outfit type.
 *
 *    @param p Pilot to check.
 *    @param limit Outfit (limiting) type to check.
 *    @return the amount of outfits of this type the pilot has.
 */
int pilot_hasOutfitLimit( const Pilot *p, const char *limit )
{
   if ( limit == NULL )
      return 0;
   for ( int i = 0; i < array_size( p->outfits ); i++ ) {
      const Outfit *o = p->outfits[i]->outfit;
      if ( o == NULL )
         continue;
      if ( ( outfit_limit( o ) != NULL ) &&
           ( strcmp( outfit_limit( o ), limit ) == 0 ) )
         return 1;
   }
   for ( int i = 0; i < array_size( p->outfit_intrinsic ); i++ ) {
      const Outfit *o = p->outfit_intrinsic[i].outfit;
      if ( ( outfit_limit( o ) != NULL ) &&
           ( strcmp( outfit_limit( o ), limit ) == 0 ) )
         return 1;
   }
   return 0;
}

/**
 * @brief Checks to see if can equip/remove an outfit from a slot.
 *
 *    @param p Pilot to check if can equip.
 *    @param s Slot being checked to see if it can equip/remove an outfit.
 *    @param o Outfit to check (NULL if being removed).
 *    @return NULL if can swap, or error message if can't.
 */
const char *pilot_canEquip( const Pilot *p, const PilotOutfitSlot *s,
                            const Outfit *o )
{
   /* Just in case. */
   if ( ( p == NULL ) || ( s == NULL ) )
      return _( "Nothing selected." );

   if ( o != NULL ) {
      /* Check slot type. */
      if ( !outfit_fitsSlot( o, &s->sslot->slot ) )
         return _( "Does not fit slot." );
      /* Check outfit limit. */
      if ( ( outfit_limit( o ) != NULL ) &&
           pilot_hasOutfitLimit( p, outfit_limit( o ) ) )
         return _( "Already have an outfit of this type installed" );
      /* Check to see if already equipped unique. */
      if ( outfit_isProp( o, OUTFIT_PROP_UNIQUE ) &&
           ( pilot_numOutfit( p, o ) > 0 ) )
         return _( "Can only install unique outfit once." );
   }

   return NULL;
}

/**
 * @brief Adds some ammo to the pilot stock.
 *
 *    @param pilot Pilot to add ammo to.
 *    @param s Slot to add ammo to.
 *    @param quantity Amount to add.
 *    @return Amount actually added.
 */
int pilot_addAmmo( Pilot *pilot, PilotOutfitSlot *s, int quantity )
{
   int q, max;

   /* Failure cases. */
   if ( s->outfit == NULL ) {
      WARN( _( "Pilot '%s': Trying to add ammo to unequipped slot." ),
            pilot->name );
      return 0;
   } else if ( !outfit_isLauncher( s->outfit ) &&
               !outfit_isFighterBay( s->outfit ) )
      return 0;

   /* Add the ammo. */
   max = pilot_maxAmmoO( pilot, s->outfit ) - s->u.ammo.deployed;
   q   = s->u.ammo.quantity; /* Amount have. */
   s->u.ammo.quantity += quantity;
   s->u.ammo.quantity = MIN( max, s->u.ammo.quantity );
   q                  = s->u.ammo.quantity - q; /* Amount actually added. */
   pilot->mass_outfit += q * outfit_ammoMass( s->outfit );
   pilot_updateMass( pilot );

   return q;
}

/**
 * @brief Removes some ammo from the pilot stock.
 *
 *    @param pilot Pilot to remove ammo from.
 *    @param s Slot to remove ammo from.
 *    @param quantity Amount to remove.
 *    @return Amount actually removed.
 */
int pilot_rmAmmo( Pilot *pilot, PilotOutfitSlot *s, int quantity )
{
   int q;

   /* Failure cases. */
   if ( !outfit_isLauncher( s->outfit ) && !outfit_isFighterBay( s->outfit ) )
      return 0;
   else if ( s->outfit == NULL ) {
      WARN( _( "Pilot '%s': Trying to remove ammo from unequipped slot." ),
            pilot->name );
      return 0;
   }

   /* Remove ammo. */
   q = MIN( quantity, s->u.ammo.quantity );
   s->u.ammo.quantity -= q;
   pilot->mass_outfit -= q * outfit_ammoMass( s->outfit );
   pilot_updateMass( pilot );
   /* We don't set the outfit to null so it "remembers" old ammo. */

   return q;
}

/**
 * @brief Gets the number of ammo units on the ship
 *
 *    @param pilot Pilot to count the ammo on
 *    @@return The integer count of ammo units on pilot
 */
int pilot_countAmmo( const Pilot *pilot )
{
   int nammo = 0;
   for ( int i = 0; i < array_size( pilot->outfits ); i++ ) {
      const Outfit    *outfit;
      PilotOutfitSlot *po = pilot->outfits[i];
      if ( po == NULL )
         continue;
      outfit = po->outfit;
      if ( outfit == NULL )
         continue;
      if ( !outfit_isLauncher( po->outfit ) )
         continue;
      nammo += po->u.ammo.quantity;
   }
   return nammo;
}

/**
 * @brief The maximum amount of ammo the pilot's current ship can hold.
 *
 *    @param pilot Pilot to get the count from
 *    @@return An integer, the max amount of ammo that can be held.
 */
int pilot_maxAmmo( const Pilot *pilot )
{
   int max = 0;
   for ( int i = 0; i < array_size( pilot->outfits ); i++ ) {
      const Outfit    *outfit;
      PilotOutfitSlot *po = pilot->outfits[i];
      if ( po == NULL )
         continue;
      outfit = po->outfit;
      if ( outfit == NULL )
         continue;
      if ( !outfit_isLauncher( outfit ) )
         continue;
      max += outfit_amount( outfit );
   }
   max = round( (double)max * pilot->stats.ammo_capacity );
   return max;
}

/**
 * @brief Gets the maximum available ammo for a pilot for a specific outfit.
 */
int pilot_maxAmmoO( const Pilot *p, const Outfit *o )
{
   int max;
   if ( o == NULL )
      return 0;
   else if ( outfit_isLauncher( o ) )
      max =
         MAX( 0, round( (double)outfit_amount( o ) * p->stats.ammo_capacity ) );
   else if ( outfit_isFighterBay( o ) )
      max =
         MAX( 0, round( (double)outfit_amount( o ) * p->stats.fbay_capacity ) );
   else
      max = 0;
   return max;
}

/**
 * @brief Fills pilot's ammo completely.
 *
 *    @param pilot Pilot to add ammo to.
 */
void pilot_fillAmmo( Pilot *pilot )
{
   for ( int i = 0; i < array_size( pilot->outfits ); i++ ) {
      int           ammo_threshold;
      const Outfit *o = pilot->outfits[i]->outfit;

      /* Must be valid outfit. */
      if ( o == NULL )
         continue;

      /* Initial (raw) ammo threshold */
      ammo_threshold = pilot_maxAmmoO( pilot, o );

      /* Adjust for deployed fighters if needed */
      if ( outfit_isFighterBay( o ) )
         ammo_threshold -= pilot->outfits[i]->u.ammo.deployed;

      /* Add ammo. */
      pilot_addAmmo( pilot, pilot->outfits[i],
                     ammo_threshold - pilot->outfits[i]->u.ammo.quantity );
   }
}

double pilot_outfitRange( const Pilot *p, const Outfit *o )
{
   if ( outfit_isBolt( o ) ) {
      double range = outfit_falloff( o ) +
                     ( outfit_rangeRaw( o ) - outfit_falloff( o ) ) / 2.;
      if ( p != NULL ) {
         if ( outfit_isTurret( o ) )
            range *= p->stats.tur_range * p->stats.weapon_range;
         else if ( outfit_isForward( o ) )
            range *= p->stats.fwd_range * p->stats.weapon_range;
      }
      return range;
   } else if ( outfit_isBeam( o ) ) {
      double range = outfit_rangeRaw( o );
      if ( p != NULL ) {
         if ( outfit_isTurret( o ) )
            range *= p->stats.tur_range * p->stats.weapon_range;
         else if ( outfit_isForward( o ) )
            range *= p->stats.fwd_range * p->stats.weapon_range;
      }
      return range;
   } else if ( outfit_isLauncher( o ) ) {
      double duration  = outfit_duration( o );
      double accel     = outfit_launcherAccel( o );
      double speed     = outfit_launcherSpeed( o );
      double speed_max = outfit_launcherSpeedMax( o );
      if ( p != NULL ) {
         duration *= p->stats.launch_range * p->stats.weapon_range;
         speed *= p->stats.launch_speed;
         accel *= p->stats.launch_accel;
         speed_max *= p->stats.launch_speed;
      }
      if ( outfit_launcherAccel( o ) != 0. ) {
         double speedinc;
         if ( speed >
              0. ) /* Ammo that don't start stopped don't have max speed. */
            speedinc = INFINITY;
         else
            speedinc = speed_max - speed;
         double at = speedinc / accel;
         if ( at < duration )
            return speedinc * ( duration - at / 2. ) + speed * duration;

         /* Maximum speed will never be reached. */
         return pow2( duration ) * accel / 2. + duration * speed;
      }
      return speed * duration;
   } else if ( outfit_isFighterBay( o ) )
      return INFINITY;
   return -1.;
}

/**
 * @brief Computes the stats for a pilot's slot.
 */
static void pilot_calcStatsSlot( Pilot *pilot, PilotOutfitSlot *slot,
                                 ShipStats *s )
{
   const Outfit *o = slot->outfit;

   /* Outfit must exist. */
   if ( o == NULL )
      return;

   /* Modify CPU. */
   pilot->cpu += outfit_cpu( o );

   /* Add mass. */
   pilot->mass_outfit += outfit_mass( o );

   /* Keep a separate counter for required (core) outfits. */
   if ( sp_required( outfit_slotProperty( o ) ) )
      pilot->base_mass += outfit_mass( o );

   /* Add ammo mass. */
   pilot->mass_outfit += slot->u.ammo.quantity * outfit_ammoMass( o );

   if ( outfit_isAfterburner( o ) ) /* Afterburner */
      pilot->afterburner = slot;    /* Set afterburner */

   /* Lua mods apply their stats. */
   if ( slot->lua_mem != LUA_NOREF )
      ss_statsMergeFromList( s, slot->lua_stats, 0 );

   /* Has update function. */
   if ( outfit_luaUpdate( o ) != LUA_NOREF )
      pilot->outfitlupdate = 1;

   /* Apply modifications. */
   if ( outfit_isMod( o ) ) { /* Modification */
      /* Active outfits must be on to affect stuff. */
      if ( ( slot->flags & PILOTOUTFIT_ACTIVE ) &&
           !( slot->state == PILOT_OUTFIT_ON ) )
         return;
      /* Add stats. */
      ss_statsMergeFromList( s, outfit_stats( o ), 0 );

   } else if ( outfit_isAfterburner( o ) ) { /* Afterburner */
      /* Active outfits must be on to affect stuff. */
      if ( ( slot->flags & PILOTOUTFIT_ACTIVE ) &&
           !( slot->state == PILOT_OUTFIT_ON ) )
         return;
      /* Add stats. */
      ss_statsMergeFromList( s, outfit_stats( o ), 0 );
      pilot_setFlag(
         pilot,
         PILOT_AFTERBURNER ); /* We use old school flags for this still... */
      pilot->stats.energy_regen_malus +=
         outfit_energy( pilot->afterburner->outfit ); /* energy loss */
   } else {
      /* Always add stats for non mod/afterburners. */
      ss_statsMergeFromList( s, outfit_stats( o ), 0 );
   }
}

/**
 * @brief Recalculates the pilot's stats based on his outfits.
 *
 *    @param pilot Pilot to recalculate his stats.
 */
void pilot_calcStats( Pilot *pilot )
{
   double     ac, sc, ec, fc, tm; /* temporary health coefficients to set */
   ShipStats *s;

   /*
    * Set up the basic stuff
    */
   /* mass */
   pilot->solid.mass = pilot->ship->mass;
   pilot->base_mass  = pilot->solid.mass;
   /* cpu */
   pilot->cpu = 0.;
   /* movement */
   pilot->accel_base = pilot->ship->accel;
   pilot->turn_base  = pilot->ship->turn;
   pilot->speed_base = pilot->ship->speed;
   /* crew */
   pilot->crew = pilot->ship->crew;
   /* cargo */
   pilot->cap_cargo = pilot->ship->cap_cargo;
   /* fuel_consumption. */
   pilot->fuel_consumption = pilot->ship->fuel_consumption;
   /* health */
   ac = ( pilot->armour_max > 0. ) ? pilot->armour / pilot->armour_max : 0.;
   sc = ( pilot->shield_max > 0. ) ? pilot->shield / pilot->shield_max : 0.;
   ec = ( pilot->energy_max > 0. ) ? pilot->energy / pilot->energy_max : 0.;
   fc = ( pilot->fuel_max > 0. ) ? pilot->fuel / pilot->fuel_max : 0.;
   if ( landed && ( land_spob != NULL ) &&
        spob_hasService( land_spob, SPOB_SERVICE_REFUEL ) )
      fc = 1.;
   pilot->armour_max   = pilot->ship->armour;
   pilot->shield_max   = pilot->ship->shield;
   pilot->fuel_max     = pilot->ship->fuel;
   pilot->armour_regen = pilot->ship->armour_regen;
   pilot->shield_regen = pilot->ship->shield_regen;
   /* Absorption. */
   pilot->dmg_absorb = pilot->ship->dmg_absorb;
   /* Energy. */
   pilot->energy_max   = pilot->ship->energy;
   pilot->energy_regen = pilot->ship->energy_regen;
   /* Misc. */
   pilot->outfitlupdate = 0;
   /* Stats.
    *
    * In general, stats are applied in two ways:
    * 1. Stats from same sources are additive if positive, multiplicative if
    * negative (e.g., ship base stats and ship Lua stats or different outfit
    * stats).
    * 2. Stats from different sources are always multiplicative (e.g., ship
    * stats and outfit stats).
    */
   s  = &pilot->stats;
   tm = s->time_mod;

   /* Initialize ship stats (additive). */
   *s = pilot->ship->stats_array;
   ss_statsMergeFromList( &pilot->stats, pilot->ship_stats,
                          0 ); /* From ship Lua if applicable. */

   /* Apply intrinsic stats. */
   ss_statsMergeFromList( &pilot->stats, pilot->intrinsic_stats, 1 );

   /* Now add outfit changes */
   ShipStats outfit_stats;
   ss_statsInit( &outfit_stats );
   pilot->mass_outfit = 0.;
   for ( int i = 0; i < array_size( pilot->outfit_intrinsic ); i++ )
      pilot_calcStatsSlot( pilot, &pilot->outfit_intrinsic[i], &outfit_stats );
   for ( int i = 0; i < array_size( pilot->outfits ); i++ )
      pilot_calcStatsSlot( pilot, pilot->outfits[i], &outfit_stats );
   ss_statsMerge( &pilot->stats, &outfit_stats, 1 );

   /* Compute effects. */
   effect_compute( &pilot->stats, pilot->effects );

   /* Apply system effects. */
   ss_statsMergeFromList( &pilot->stats, cur_system->stats, 1 );

   /* Player and escorts gets difficulty applied. */
   if ( pilot_isWithPlayer( pilot ) )
      difficulty_apply( s );

   /* Apply stealth malus. */
   if ( pilot_isFlag( pilot, PILOT_STEALTH ) ) {
      s->accel_mod *= 0.8;
      s->turn_mod *= 0.8;
      s->speed_mod *= 0.5;
   }

   /*
    * Absolute increases.
    */
   /* Movement. */
   pilot->accel_base += s->accel;
   pilot->turn_base += s->turn * M_PI / 180.;
   pilot->speed_base += s->speed;
   /* Health. */
   pilot->armour_max += s->armour;
   pilot->armour_regen += s->armour_regen;
   pilot->shield_max += s->shield;
   pilot->shield_regen += s->shield_regen;
   pilot->energy_max += s->energy;
   pilot->energy_regen += s->energy_regen;
   /* Misc. */
   pilot->fuel_max += s->fuel;
   pilot->cap_cargo += s->cargo;

   /*
    * Relative increases.
    */
   /* Movement. */
   pilot->accel_base *= s->accel_mod;
   pilot->turn_base *= s->turn_mod;
   pilot->speed_base *= s->speed_mod;
   pilot->solid.aerodynamics = s->speed_mod;
   /* Health. */
   pilot->armour_max *= s->armour_mod;
   pilot->armour_regen *= s->armour_regen_mod;
   pilot->shield_max *= s->shield_mod;
   pilot->shield_regen *= s->shield_regen_mod;
   pilot->energy_max *= s->energy_mod;
   pilot->energy_regen *= s->energy_regen_mod;
   /* We'll be kind and allow negative shield and energy. */
   pilot->shield_max = MAX( 0., pilot->shield_max );
   pilot->energy_max = MAX( 0., pilot->energy_max );
   /* Enforce health to be at least 0 after mods, so that something like -1000%
    * would just set it to 0 instead of negative. */
   pilot->armour_regen = MAX( 0., pilot->armour_regen );
   pilot->shield_regen = MAX( 0., pilot->shield_regen );
   pilot->energy_regen = MAX( 0., pilot->energy_regen );
   /* cpu */
   pilot->cpu_max =
      (int)floor( (float)( pilot->ship->cpu + s->cpu_max ) * s->cpu_mod );
   pilot->cpu += pilot->cpu_max; /* CPU is negative, this just sets it so it's
                                    based off of cpu_max. */
   pilot->cpu += (int)floor(
      s->cpu ); /* CPU consumption by outfits. Does not get multiplied. */
   /* Misc. */
   pilot->mass_outfit += s->mass;
   pilot->crew = pilot->crew * s->crew_mod + s->crew;
   pilot->fuel_consumption *= s->fuel_usage_mod;
   pilot->fuel_max *= s->fuel_mod;
   pilot->cap_cargo *= s->cargo_mod;
   s->engine_limit *= s->engine_limit_rel;

   /*
    * Flat increases.
    */
   pilot->armour_regen -= s->armour_regen_malus;
   pilot->shield_regen -= s->shield_regen_malus;
   pilot->energy_regen -= s->energy_regen_malus;
   pilot->dmg_absorb = pilot->dmg_absorb + s->absorb;

   /* Give the pilot his health proportion back */
   pilot->armour = ac * pilot->armour_max;
   pilot->shield = sc * pilot->shield_max;
   pilot->energy = ec * pilot->energy_max;
   pilot->fuel   = fc * pilot->fuel_max;

   /* Some sanity checks. */
   pilot->stats.time_speedup = MAX( pilot->stats.time_speedup, 0. );

   /* Deployed fighters with no mothership take damage over time. */
   if ( pilot_isFlag( pilot, PILOT_CARRIER_DIED ) ) {
      const Pilot *parent = pilot_get( pilot->parent );
      if ( parent == NULL ) {
         pilot->armour_regen = MIN( -3.5, pilot->armour_regen - 10. );
         pilot->stats.disable += 35.;
      }
   }

   /* Dump excess fuel */
   pilot->fuel = MIN( pilot->fuel, pilot->fuel_max );

   /* Cargo has to be reset. */
   pilot_cargoCalc( pilot ); /* Calls pilot_updateMass. */

   /* Update GUI as necessary. */
   if ( ( pilot->id > 0 ) && pilot_isPlayer( pilot ) && ( pilot->ai != NULL ) )
      gui_setGeneric( pilot );

   /* Update weapon set range. */
   pilot_weapSetUpdateStats( pilot );

   /* In case the time_mod has changed. */
   if ( pilot_isPlayer( pilot ) && ( tm != s->time_mod ) &&
        !player_isFlag( PLAYER_AUTONAV ) )
      player_resetSpeed();
}

/**
 * @brief Cures the pilot as if he was landed.
 */
void pilot_healLanded( Pilot *pilot )
{
   pilot->armour = pilot->armour_max;
   pilot->shield = pilot->shield_max;
   pilot->energy = pilot->energy_max;

   pilot->stress = 0.;
   pilot->stimer = 0.;
   pilot->sbonus = 0.;

   pilot_fillAmmo( pilot );

   for ( int i = 0; i < array_size( pilot->escorts ); i++ ) {
      const Escort_t *e  = &pilot->escorts[i];
      Pilot          *pe = pilot_get( e->id );

      if ( pe != NULL )
         pilot_healLanded( pe );
   }
}

/**
 * @brief Gets the outfit slot by name.
 */
PilotOutfitSlot *pilot_getSlotByName( Pilot *pilot, const char *name )
{
   for ( int i = 0; i < array_size( pilot->outfits ); i++ ) {
      PilotOutfitSlot *s = pilot->outfits[i];
      if ( ( s->sslot->name != NULL ) &&
           ( strcmp( s->sslot->name, name ) == 0 ) )
         return s;
   }
   return NULL;
}

/**
 * @brief Gets the factor at which speed gets worse.
 */
double pilot_massFactor( const Pilot *pilot )
{
   double mass = pilot->solid.mass;
   if ( mass > pilot->stats.engine_limit ) {
      if ( pilot->stats.engine_limit > 0. ) {
         double f =
            ( mass - pilot->stats.engine_limit ) / pilot->stats.engine_limit;
         return 1. / ( 1. + f + f + 4. * pow( f, 3. ) );
      } else
         return 0.;
   } else
      return 1.;
}

/**
 * @brief Updates the pilot stats after mass change.
 *
 *    @param pilot Pilot to update his mass.
 */
void pilot_updateMass( Pilot *pilot )
{
   double factor;

   /* Recompute effective mass if something changed. */
   pilot->solid.mass =
      MAX( MAX( pilot->ship->mass * CTS.SHIP_MIN_MASS,
                pilot->stats.mass_mod *
                   ( pilot->ship->mass + pilot->mass_outfit ) ) +
              pilot->stats.cargo_inertia * pilot->mass_cargo,
           0. );

   /* Set and apply limit. */
   factor       = pilot_massFactor( pilot );
   pilot->accel = factor * pilot->accel_base;
   pilot->turn  = factor * pilot->turn_base;
   pilot->speed = factor * pilot->speed_base;

   /* limit the maximum speed if limiter is active */
   if ( pilot_isFlag( pilot, PILOT_HASSPEEDLIMIT ) ) {
      pilot->speed = pilot->speed_limit - pilot->accel / CTS.PHYSICS_SPEED_DAMP;
      /* Speed must never go negative. */
      if ( pilot->speed < 0. ) {
         /* If speed DOES go negative, we have to lower accel. */
         pilot->accel = CTS.PHYSICS_SPEED_DAMP * pilot->speed_limit;
         pilot->speed = 0.;
      }
   }
   /* Need to recalculate electronic warfare mass change. */
   pilot_ewUpdateStatic( pilot );

   /* Update ship stuff. */
   if ( ( pilot->id > 0 ) && pilot_isPlayer( pilot ) && ( pilot->ai != NULL ) )
      gui_setShip();
}

/**
 * @brief Checks to see if a slot has an active outfit that can be toggleable.
 *
 *    @param o Outfit slot to check.
 *    @return 1 if can toggle, 0 otherwise.
 */
int pilot_slotIsToggleable( const PilotOutfitSlot *o )
{
   return ( o->flags & PILOTOUTFIT_TOGGLEABLE );
}

typedef struct PilotTemp {
   int     noid;
   Pilot  *ptemp;
   Pilot **ps;
} PilotTemp;

static PilotTemp temp_setup( Pilot *p )
{
   PilotTemp tmp;
   if ( p == NULL ) {
      tmp.noid  = 0;
      tmp.ptemp = NULL;
      tmp.ps    = NULL;
      return tmp;
   }

   tmp.noid  = ( p->id == 0 );
   tmp.ptemp = NULL;
   if ( tmp.noid ) {
      tmp.ps = (Pilot **)pilot_getAll();
      /* If array is empty we have to add a new pilot. */
      if ( array_size( tmp.ps ) <= 0 ) {
         array_push_back( &tmp.ps, p );
      } else {
         tmp.ptemp = tmp.ps[0];
         tmp.ps[0] = p;
      }
      if ( pilot_isPlayer( p ) )
         p->id = PLAYER_ID;
      else
         p->id = PILOT_TEMP_ID;
   }
   return tmp;
}

static void temp_cleanup( PilotTemp tmp, Pilot *p )
{
   if ( p == NULL )
      return;
   if ( tmp.noid ) {
      p->id = 0;
      if ( tmp.ptemp == NULL )
         array_erase( &tmp.ps, &tmp.ps[0], &tmp.ps[1] );
      else
         tmp.ps[0] = tmp.ptemp;
   }
}

/**
 * @brief Wrapper that does all the work for us.
 */
static void pilot_outfitLRun( Pilot *p,
                              void ( *const func )( const Pilot     *p,
                                                    PilotOutfitSlot *po,
                                                    const void      *data ),
                              const void *data )
{
   /* If no ID, we'll hackily add a temporary pilot and undo the changes. */
   PilotTemp tmp = temp_setup( p );

   pilotoutfit_modified = 0;
   for ( int i = 0; i < array_size( p->outfits ); i++ ) {
      PilotOutfitSlot *po = p->outfits[i];
      if ( po->outfit == NULL )
         continue;
      func( p, po, data );
   }
   for ( int i = 0; i < array_size( p->outfit_intrinsic ); i++ ) {
      PilotOutfitSlot *po = &p->outfit_intrinsic[i];
      if ( po->outfit == NULL )
         continue;
      func( p, po, data );
   }

   /* Some clean up. */
   temp_cleanup( tmp, p );

   /* Recalculate if anything changed. */
   if ( pilotoutfit_modified ) {
      /* TODO pilot_calcStats can be called twice here. */
      pilot_weapSetUpdateOutfitState( p );
      pilot_calcStats( p );
   }
}
static void outfitLRunWarning( const Pilot *p, const Outfit *o,
                               const char *name, const char *error )
{
   WARN( _( "Pilot '%s''s outfit '%s' -> '%s':\n%s" ),
         ( p == NULL ) ? "(NULL)" : p->name, outfit_name( o ), name, error );
}

/**
 * @brief Sets up the outfit memory for a slot.
 */
static int pilot_outfitLmem( PilotOutfitSlot *po, nlua_env *env )
{
   int oldmem;
   /* Create the memory if necessary and initialize stats. */
   if ( po->lua_mem == LUA_NOREF ) {
      lua_newtable( naevL );                              /* mem */
      po->lua_mem = luaL_ref( naevL, LUA_REGISTRYINDEX ); /* */
   }
   /* Get old memory. */
   nlua_getenv( naevL, env, "mem" );              /* oldmem */
   oldmem = luaL_ref( naevL, LUA_REGISTRYINDEX ); /* */
   /* Set the memory. */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, po->lua_mem ); /* mem */
   nlua_setenv( naevL, env, "mem" );                     /* */
   return oldmem;
}

/**
 * @brief Cleans up the outfit memory for a slot.
 */
static void pilot_outfitLunmem( nlua_env *env, int oldmem )
{
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, oldmem );
   nlua_setenv( naevL, env, "mem" ); /* pm */
   luaL_unref( naevL, LUA_REGISTRYINDEX, oldmem );
}

static const char *pilot_outfitLDescExtra( const Pilot *p, const Outfit *o,
                                           PilotOutfitSlot *pos )
{
   static char descextra[STRMAX];
   const char *de;
   if ( outfit_luaDescextra( o ) == LUA_NOREF )
      return ( outfit_descExtra( o ) != NULL ) ? _( outfit_descExtra( o ) )
                                               : NULL;

   PilotTemp tmp = temp_setup( (Pilot *)p );

   /* Set up the function: init( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaDescextra( o ) ); /* f */
   if ( ( p != NULL ) && ( p->id > 0 ) ) /* Needs valid ID. */
      lua_pushpilot( naevL, p->id );     /* f, p */
   else
      lua_pushnil( naevL );    /* f, p */
   lua_pushoutfit( naevL, o ); /* f, p, o */
   if ( pos == NULL )
      lua_pushnil( naevL ); /* f, p */
   else
      lua_pushpilotoutfit( naevL, pos );           /* f, p, o, pos */
   if ( nlua_pcall( outfit_luaEnv( o ), 3, 1 ) ) { /* */
      outfitLRunWarning( p, o, "descextra", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      descextra[0] = '\0';
      temp_cleanup( tmp, (Pilot *)p );
      return descextra;
   }
   /* Case no return we just pass nothing. */
   if ( lua_isnoneornil( naevL, -1 ) ) {
      lua_pop( naevL, 1 );
      temp_cleanup( tmp, (Pilot *)p );
      return NULL;
   }
   de = luaL_checkstring( naevL, -1 );
   strncpy( descextra, de, sizeof( descextra ) - 1 );
   lua_pop( naevL, 1 );
   temp_cleanup( tmp, (Pilot *)p );
   return descextra;
}

/**
 * @brief Gets the description of an outfit for a given pilot.
 *
 * Note: the returned string can get overwritten by subsequent calls.
 *
 *    @param p Pilot to get the outfit description of (or NULL for no pilot).
 *    @param o Outfit to get description of.
 *    @param pos Pilot outfit slot or NULL if not applicable.
 *    @return The description of the outfit.
 */
const char *pilot_outfitDescription( const Pilot *p, const Outfit *o,
                                     PilotOutfitSlot *pos )
{
   static char o_description[STRMAX];
   const char *de = pilot_outfitLDescExtra( p, o, pos );
   if ( de == NULL )
      return _( outfit_descRaw( o ) );
   snprintf( o_description, sizeof( o_description ), "%s\n%s",
             _( outfit_descRaw( o ) ), de );
   return o_description;
}

/**
 * @brief Gets the summary of an outfit for a give pilot.
 *
 * Note: the returned string can get overwritten by subsequent calls.
 *
 *    @param p Pilot to get the outfit summary of (or NULL for no pilot).
 *    @param o Outfit to get summary of.
 *    @param withname Whether or not to show the name too.
 *    @param pos Pilot outfit slot or NULL if not applicable.
 *    @return The summary of the outfit.
 */
const char *pilot_outfitSummary( const Pilot *p, const Outfit *o, int withname,
                                 PilotOutfitSlot *pos )
{
   static char o_summary[STRMAX];
   const char *de = pilot_outfitLDescExtra( p, o, pos );
   size_t      n  = 0;
   if ( withname ) {
      n += outfit_getNameWithClass( o, &o_summary[n], sizeof( o_summary ) - n );
      o_summary[n++] = '\n';
   }
   n += snprintf( &o_summary[n], sizeof( o_summary ) - n, "%s",
                  outfit_summaryRaw( o ) );
   if ( de != NULL )
      snprintf( &o_summary[n], sizeof( o_summary ) - n, "\n%s", de );
   return o_summary;
}

/**
 * @brief Gets the speed of an outfit given a pilot.
 */
double pilot_outfitSpeed( const Pilot *p, const Outfit *o )
{
   if ( outfit_isBolt( o ) )
      return outfit_boltSpeed( o );
   else if ( outfit_isLauncher( o ) ) {
      double t;
      double accel     = outfit_launcherAccel( o );
      double speed     = outfit_launcherSpeed( o );
      double speed_max = outfit_launcherSpeedMax( o );
      double duration  = outfit_launcherDuration( o );
      if ( p != NULL ) {
         speed *= p->stats.launch_speed;
         accel *= p->stats.launch_accel;
         speed_max *= p->stats.launch_speed;
         duration *= p->stats.launch_range * p->stats.weapon_range;
      }

      if ( accel == 0. )
         return speed;

      if ( speed >
           0. ) /* Ammo that don't start stopped don't have max speed. */
         t = INFINITY;
      else
         t = ( speed_max - speed ) / accel; /* Time to reach max speed */

      /* Reaches max speed. */
      if ( t < duration )
         return ( accel * t * t / 2. +
                  ( speed_max - speed ) * ( duration - t ) ) /
                   duration +
                speed;
      /* Doesn't reach max speed. */
      else
         return accel * duration / 2. + speed;
   }
   return -1.;
}

static void outfitLInit( const Pilot *pilot, PilotOutfitSlot *po,
                         const void *data )
{
   (void)data;
   int           lua_oinit, oldmem;
   nlua_env     *lua_env;
   const Outfit *o = po->outfit;

   if ( o == NULL )
      return;

   lua_env = outfit_luaEnv( o );
   if ( lua_env == NULL )
      return;

   lua_oinit = outfit_luaInit( o );

   /* Create the memory if necessary and initialize stats. */
   oldmem = pilot_outfitLmem( po, lua_env );

   if ( lua_oinit == LUA_NOREF ) {
      pilot_outfitLunmem( outfit_luaEnv( o ), oldmem );
      return;
   }

   /* Set up the function: init( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, lua_oinit ); /* f */
   lua_pushpilot( naevL, pilot->id );                  /* f, p */
   lua_pushpilotoutfit( naevL, po );                   /* f, p, po */
   if ( nlua_pcall( lua_env, 2, 0 ) ) {                /* */
      outfitLRunWarning( pilot, o, "init", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      pilot_outfitLunmem( outfit_luaEnv( o ), oldmem );
      return;
   }
   pilot_outfitLunmem( outfit_luaEnv( o ), oldmem );
}

/**
 * @brief Runs the pilot's Lua outfits init script.
 *
 *    @param pilot Pilot to run Lua outfits for.
 */
void pilot_outfitLInitAll( Pilot *pilot )
{
   NTracingZone( _ctx, 1 );
   pilot_outfitLRun( pilot, outfitLInit, NULL );
   NTracingZoneEnd( _ctx );
}

/**
 * @brief Outfit is added to a ship.
 */
int pilot_outfitLAdd( Pilot *pilot, PilotOutfitSlot *po )
{
   int           oldmem;
   const Outfit *o = po->outfit;

   if ( o == NULL )
      return 0;
   if ( outfit_luaOnadd( o ) == LUA_NOREF )
      return 0;

   PilotTemp tmp = temp_setup( pilot );

   nlua_env *env = outfit_luaEnv( o );

   /* Create the memory if necessary and initialize stats. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: init( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnadd( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                             /* f, p */
   lua_pushpilotoutfit( naevL, po );                              /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {                               /* */
      outfitLRunWarning( pilot, o, "onadd", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      pilot_outfitLunmem( env, oldmem );
      return -1;
   }
   pilot_outfitLunmem( env, oldmem );

   temp_cleanup( tmp, pilot );
   return 1;
}

/**
 * @brief Outfit is removed froma ship.
 */
int pilot_outfitLRemove( Pilot *pilot, PilotOutfitSlot *po )
{
   int           oldmem;
   const Outfit *o = po->outfit;

   if ( o == NULL )
      return 0;
   if ( outfit_luaOnremove( po->outfit ) == LUA_NOREF )
      return 0;

   PilotTemp tmp = temp_setup( pilot );

   nlua_env *env = outfit_luaEnv( po->outfit );

   /* Create the memory if necessary and initialize stats. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: init( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnremove( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                                /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {  /* */
      outfitLRunWarning( pilot, o, "onremove", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      pilot_outfitLunmem( env, oldmem );
      return -1;
   }
   pilot_outfitLunmem( env, oldmem );

   temp_cleanup( tmp, pilot );
   return 1;
}

static void outfitLOutfitChange( const Pilot *pilot, PilotOutfitSlot *po,
                                 const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;

   if ( outfit_luaOnoutfitchange( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: outofenergy( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnoutfitchange( o ) );
   lua_pushpilot( naevL, pilot->id ); /* f, p */
   lua_pushpilotoutfit( naevL, po );  /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {   /* */
      outfitLRunWarning( pilot, o, "onoutfitchange",
                         lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}

void pilot_outfitLOutfitChange( Pilot *pilot )
{
   static int changing_outfit =
      0; /* No need for atomic, since lua state is shared. */

   if ( changing_outfit > 3 ) {
      WARN( "Too many nested 'onoutfitchange', skipping." );
      return;
   }
   changing_outfit++;

   NTracingZone( _ctx, 1 );
   pilot_outfitLRun( pilot, outfitLOutfitChange, NULL );
   NTracingZoneEnd( _ctx );

   changing_outfit--;
}

/**
 * @brief Runs the pilot's Lua outfits init script for an outfit.
 *
 *    @param pilot Pilot to run Lua outfits for.
 *    @param po Pilot outfit to check.
 */
void pilot_outfitLInit( Pilot *pilot, PilotOutfitSlot *po )
{
   /* Have to make sure the pilot is valid in the single case. */
   PilotTemp tmp = temp_setup( pilot );
   outfitLInit( pilot, po, NULL );
   temp_cleanup( tmp, pilot );
}

static void outfitLUpdate( const Pilot *pilot, PilotOutfitSlot *po,
                           const void *data )
{
   double        dt;
   int           oldmem;
   const Outfit *o = po->outfit;

   if ( outfit_luaUpdate( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* The data. */
   dt = *(double *)data;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: update( p, po, dt ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaUpdate( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                              /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   lua_pushnumber( naevL, dt );      /* f, p, po, dt */
   if ( nlua_pcall( env, 3, 0 ) ) {  /* */
      outfitLRunWarning( pilot, o, "update", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs the pilot's Lua outfits update script.
 *
 *    @param pilot Pilot to run Lua outfits for.
 *    @param dt Delta-tick from last time it was run.
 */
void pilot_outfitLUpdate( Pilot *pilot, double dt )
{
   if ( !pilot->outfitlupdate )
      return;

   NTracingZone( _ctx, 1 );
   pilot_outfitLRun( pilot, outfitLUpdate, &dt );
   NTracingZoneEnd( _ctx );
}

static void outfitLOutofenergy( const Pilot *pilot, PilotOutfitSlot *po,
                                const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;

   if ( outfit_luaOutofenergy( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: outofenergy( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOutofenergy( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id ); /* f, p */
   lua_pushpilotoutfit( naevL, po );  /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {   /* */
      outfitLRunWarning( pilot, o, "outofenergy", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Handles when the pilot runs out of energy.
 *
 *    @param pilot Pilot that ran out of energy.
 */
void pilot_outfitLOutfofenergy( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOutofenergy, NULL );
}

struct OnhitData {
   double       armour;
   double       shield;
   unsigned int attacker;
};
static void outfitLOnhit( const Pilot *pilot, PilotOutfitSlot *po,
                          const void *data )
{
   double                  armour, shield;
   unsigned int            attacker;
   const struct OnhitData *odat;
   int                     oldmem;
   const Outfit           *o = po->outfit;

   if ( outfit_luaOnhit( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Data. */
   odat     = (const struct OnhitData *)data;
   armour   = odat->armour;
   shield   = odat->shield;
   attacker = odat->attacker;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: onhit( p, po, armour, shield ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnhit( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                             /* f, p */
   lua_pushpilotoutfit( naevL, po );                              /* f, p, po */
   lua_pushnumber( naevL, armour );  /* f, p, po, a */
   lua_pushnumber( naevL, shield );  /* f, p, po, a, s */
   lua_pushpilot( naevL, attacker ); /* f, p, po, a, s, attacker */
   if ( nlua_pcall( env, 5, 0 ) ) {  /* */
      outfitLRunWarning( pilot, o, "onhit", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs the pilot's Lua outfits onhit script.
 *
 *    @param pilot Pilot to run Lua outfits for.
 *    @param armour Armour amage taken by pilot.
 *    @param shield Shield amage taken by pilot.
 *    @param attacker The attacker that hit the pilot.
 */
void pilot_outfitLOnhit( Pilot *pilot, double armour, double shield,
                         unsigned int attacker )
{
   const struct OnhitData data = {
      .armour = armour, .shield = shield, .attacker = attacker };
   pilot_outfitLRun( pilot, outfitLOnhit, &data );
}

/**
 * @brief Handle the manual toggle of an outfit.
 *
 *    @param pilot Pilot to toggle outfit of.
 *    @param po Outfit to be toggling.
 *    @param on Whether to toggle on or off.
 *    @param natural Whether it's a result of the pilot input, or something
 * automatic (such as turning off outfits when landing or hyperspace).
 *    @return 1 if was able to toggle it, 0 otherwise.
 */
int pilot_outfitLOntoggle( const Pilot *pilot, PilotOutfitSlot *po, int on,
                           int natural )
{
   nlua_env *env = outfit_luaEnv( po->outfit );
   int       ret, oldmem;
   pilotoutfit_modified = 0;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: ontoggle( p, po, armour, shield ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX,
                outfit_luaOntoggle( po->outfit ) ); /* f */
   lua_pushpilot( naevL, pilot->id );               /* f, p */
   lua_pushpilotoutfit( naevL, po );                /* f, p, po */
   lua_pushboolean( naevL, on );                    /* f, p, po, on */
   lua_pushboolean( naevL, natural );               /* f, p, po, on, natural */
   if ( nlua_pcall( env, 4, 1 ) ) {                 /* */
      outfitLRunWarning( pilot, po->outfit, "ontoggle",
                         lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      pilot_outfitLunmem( env, oldmem );
      return 0;
   }

   /* Handle return boolean. */
   ret = lua_toboolean( naevL, -1 );
   lua_pop( naevL, 1 );
   pilot_outfitLunmem( env, oldmem );
   return ret;
}

/**
 * @brief Handle the manual shoot of an outfit.
 *
 *    @param pilot Pilot to shoot outfit of.
 *    @param po Outfit to be toggling.
 *    @return 1 if was able to shoot it, 0 otherwise.
 */
int pilot_outfitLOnshoot( const Pilot *pilot, PilotOutfitSlot *po )
{
   const Outfit *o   = po->outfit;
   nlua_env     *env = outfit_luaEnv( o );
   int           ret, oldmem;
   pilotoutfit_modified = 0;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: onshoot( p, po, armour, shield ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnshoot( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                               /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   if ( nlua_pcall( env, 2, 1 ) ) {  /* */
      outfitLRunWarning( pilot, o, "onshoot", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      pilot_outfitLunmem( env, oldmem );
      return 0;
   }

   /* Handle return boolean. */
   ret = lua_toboolean( naevL, -1 );
   lua_pop( naevL, 1 );
   pilot_outfitLunmem( env, oldmem );
   return ret || pilotoutfit_modified; /* Even if the script says it didn't
                                          change, it may have been modified. */
}

struct CooldownData {
   int    done;
   int    success;
   double timer;
};
static void outfitLCooldown( const Pilot *pilot, PilotOutfitSlot *po,
                             const void *data )
{
   int                        done, success, oldmem;
   double                     timer;
   const struct CooldownData *cdat;
   const Outfit              *o = po->outfit;

   if ( outfit_luaCooldown( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   cdat    = (const struct CooldownData *)data;
   done    = cdat->done;
   success = cdat->success;
   timer   = cdat->timer;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: cooldown( p, po, done, success/timer ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaCooldown( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                                /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   lua_pushboolean( naevL, done );   /* f, p, po, done */
   if ( done )
      lua_pushboolean( naevL, success ); /* f, p, po, done, success */
   else
      lua_pushnumber( naevL, timer ); /* f, p, po, done, timer */
   if ( nlua_pcall( env, 4, 0 ) ) {   /* */
      outfitLRunWarning( pilot, o, "cooldown", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Handle cooldown hooks for outfits.
 *
 *    @param pilot Pilot being handled.
 *    @param done Whether or not cooldown is starting or done.
 *    @param success Whether or not it completed successfully.
 *    @param timer How much time is necessary to cooldown. Only used if done is
 * false.
 */
void pilot_outfitLCooldown( Pilot *pilot, int done, int success, double timer )
{
   const struct CooldownData data = {
      .done = done, .success = success, .timer = timer };
   pilot_outfitLRun( pilot, outfitLCooldown, &data );
}

static void outfitLOnshootany( const Pilot *pilot, PilotOutfitSlot *po,
                               const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaOnshootany( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: onshootany( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnshootany( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id ); /* f, p */
   lua_pushpilotoutfit( naevL, po );  /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {   /* */
      outfitLRunWarning( pilot, o, "onshootany", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs the pilot's Lua outfits onshootany script.
 *
 *    @param pilot Pilot to run Lua outfits for.
 */
void pilot_outfitLOnshootany( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOnshootany, NULL );
}

static void outfitLOnstealth( const Pilot *pilot, PilotOutfitSlot *po,
                              const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaOnstealth( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: onstealth( p, po, stealthed ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnstealth( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                                 /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   lua_pushboolean(
      naevL, pilot_isFlag( pilot, PILOT_STEALTH ) ); /* f, p, po, steathed */
   if ( nlua_pcall( env, 3, 0 ) ) {                  /* */
      outfitLRunWarning( pilot, o, "onstealth", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs the pilot's Lua outfits onhit script.
 *
 *    @param pilot Pilot to run Lua outfits for.
 *    @return 1 if ship stats were recalculated.
 */
int pilot_outfitLOnstealth( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOnstealth, NULL );
   return pilotoutfit_modified;
}

static void outfitLOnscan( const Pilot *pilot, PilotOutfitSlot *po,
                           const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaOnscan( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: onscan( p, po, target ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnscan( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                              /* f, p */
   lua_pushpilotoutfit( naevL, po );      /* f, p, po */
   lua_pushpilot( naevL, pilot->target ); /* f, p, po, t */
   if ( nlua_pcall( env, 3, 0 ) ) {       /* */
      outfitLRunWarning( pilot, o, "onscan", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs Lua outfits when pilot scanned their target.
 *
 *    @param pilot Pilot being handled.
 */
void pilot_outfitLOnscan( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOnscan, NULL );
}

static void outfitLOnscanned( const Pilot *pilot, PilotOutfitSlot *po,
                              const void *data )
{
   const Pilot  *scanner;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaOnscanned( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );
   scanner       = (const Pilot *)data;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: onscanned( p, po, stealthed ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnscanned( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                                 /* f, p */
   lua_pushpilotoutfit( naevL, po );    /* f, p, po */
   lua_pushpilot( naevL, scanner->id ); /* f, p, po, scanner */
   if ( nlua_pcall( env, 3, 0 ) ) {     /* */
      outfitLRunWarning( pilot, o, "onscanned", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs Lua outfits when pilot was scanned by scanner.
 *
 *    @param pilot Pilot being handled.
 *    @param scanner Pilot that scanned the pilot.
 */
void pilot_outfitLOnscanned( Pilot *pilot, const Pilot *scanner )
{
   pilot_outfitLRun( pilot, outfitLOnscanned, scanner );
}

static void outfitLOnland( const Pilot *pilot, PilotOutfitSlot *po,
                           const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaLand( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: land( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaLand( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                            /* f, p */
   lua_pushpilotoutfit( naevL, po );                             /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {                              /* */
      outfitLRunWarning( pilot, o, "land", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs Lua outfits when pilot lands on a spob.
 *
 *    @param pilot Pilot being handled.
 */
void pilot_outfitLOnland( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOnland, NULL );
}

static void outfitLOntakeoff( const Pilot *pilot, PilotOutfitSlot *po,
                              const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaTakeoff( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaTakeoff( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                               /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {  /* */
      outfitLRunWarning( pilot, o, "takeoff", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs Lua outfits when pilot takes off from a spob.
 *
 *    @param pilot Pilot being handled.
 */
void pilot_outfitLOntakeoff( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOntakeoff, NULL );
}

static void outfitLOnjumpin( const Pilot *pilot, PilotOutfitSlot *po,
                             const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaJumpin( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaJumpin( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                              /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {  /* */
      outfitLRunWarning( pilot, o, "jumpin", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs Lua outfits when pilot jumps into a system.
 *
 *    @param pilot Pilot being handled.
 */
void pilot_outfitLOnjumpin( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOnjumpin, NULL );
}

static void outfitLOnboard( const Pilot *pilot, PilotOutfitSlot *po,
                            const void *data )
{
   const Pilot  *target;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaBoard( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );
   target        = (const Pilot *)data;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: board( p, po, stealthed ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaBoard( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                             /* f, p */
   lua_pushpilotoutfit( naevL, po );                              /* f, p, po */
   lua_pushpilot( naevL, target->id ); /* f, p, po, target */
   if ( nlua_pcall( env, 3, 0 ) ) {    /* */
      outfitLRunWarning( pilot, o, "board", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
/**
 * @brief Runs Lua outfits when pilot boards a target.
 *
 *    @param pilot Pilot being handled.
 *    @param target Pilot being boarded.
 */
void pilot_outfitLOnboard( Pilot *pilot, const Pilot *target )
{
   pilot_outfitLRun( pilot, outfitLOnboard, target );
}

static const char *outfitkeytostr( OutfitKey key )
{
   switch ( key ) {
   case OUTFIT_KEY_ACCEL:
      return "accel";
   case OUTFIT_KEY_LEFT:
      return "left";
   case OUTFIT_KEY_RIGHT:
      return "right";
   }
   return NULL;
}

static void outfitLOnkeydoubletap( const Pilot *pilot, PilotOutfitSlot *po,
                                   const void *data )
{
   int           oldmem;
   OutfitKey     key;
   const Outfit *o = po->outfit;
   if ( outfit_luaKeydoubletap( o ) == LUA_NOREF )
      return;
   key = *( (const OutfitKey *)data );

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaKeydoubletap( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id ); /* f, p */
   lua_pushpilotoutfit( naevL, po );  /* f, p, po */
   lua_pushstring( naevL, outfitkeytostr( key ) );
   if ( nlua_pcall( env, 3, 1 ) ) { /* */
      outfitLRunWarning( pilot, o, "keydoubletap", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );

   /* Broke stealth. */
   if ( ( po->state == PILOT_OUTFIT_ON ) || lua_toboolean( naevL, -1 ) )
      stealth_break = 1;
   lua_pop( naevL, 1 );
}
void pilot_outfitLOnkeydoubletap( Pilot *pilot, OutfitKey key )
{
   if ( pilot_isDisabled( pilot ) )
      return;
   stealth_break = 0;
   pilot_outfitLRun( pilot, outfitLOnkeydoubletap, &key );
   if ( stealth_break && pilot_isFlag( pilot, PILOT_STEALTH ) )
      pilot_destealth( pilot );
}

static void outfitLOnkeyrelease( const Pilot *pilot, PilotOutfitSlot *po,
                                 const void *data )
{
   int           oldmem;
   OutfitKey     key;
   const Outfit *o = po->outfit;
   if ( outfit_luaKeyrelease( o ) == LUA_NOREF )
      return;
   key = *( (const OutfitKey *)data );

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaKeyrelease( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id ); /* f, p */
   lua_pushpilotoutfit( naevL, po );  /* f, p, po */
   lua_pushstring( naevL, outfitkeytostr( key ) );
   if ( nlua_pcall( env, 3, 0 ) ) { /* */
      outfitLRunWarning( pilot, o, "keyrelease", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
void pilot_outfitLOnkeyrelease( Pilot *pilot, OutfitKey key )
{
   pilot_outfitLRun( pilot, outfitLOnkeyrelease, &key );
}

/**
 * @brief Handle cleanup hooks for outfits.
 *
 *    @param pilot Pilot being handled.
 */
void pilot_outfitLCleanup( Pilot *pilot )
{
   /* TODO we might want to run this on intrinsic outfits too... */
   pilotoutfit_modified = 0;
   for ( int i = 0; i < array_size( pilot->outfits ); i++ ) {
      int              oldmem;
      PilotOutfitSlot *po = pilot->outfits[i];
      const Outfit    *o  = po->outfit;
      if ( o == NULL )
         continue;
      if ( outfit_luaCleanup( o ) == LUA_NOREF )
         continue;
      /* Pilot could be created and then erased without getting properly
       * initialized. */
      if ( po->lua_mem == LUA_NOREF )
         continue;

      nlua_env *env = outfit_luaEnv( o );

      /* Set the memory. */
      oldmem = pilot_outfitLmem( po, env );

      /* Set up the function: cleanup( p, po ) */
      lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaCleanup( o ) ); /* f */
      lua_pushpilot( naevL, pilot->id ); /* f, p */
      lua_pushpilotoutfit( naevL, po );  /* f, p, po */
      if ( nlua_pcall( env, 2, 0 ) ) {   /* */
         outfitLRunWarning( pilot, o, "cleanup", lua_tostring( naevL, -1 ) );
         lua_pop( naevL, 1 );
      }
      pilot_outfitLunmem( env, oldmem );
   }
   /* Pilot gets cleaned up so no need to recalculate stats. */
}

int pilot_outfitLMessage( Pilot *pilot, PilotOutfitSlot *po, const char *msg,
                          int data )
{
   int           oldmem, modified;
   const Outfit *o = po->outfit;
   if ( outfit_luaMessage( o ) == LUA_NOREF )
      return 0;

   nlua_env *env        = outfit_luaEnv( o );
   modified             = pilotoutfit_modified;
   pilotoutfit_modified = 0;

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaMessage( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                               /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   lua_pushstring( naevL, msg );
   if ( data == LUA_NOREF )
      lua_pushnil( naevL );
   else
      lua_pushvalue( naevL, data );
   int ret = 1;
   if ( nlua_pcall( env, 4, 1 ) ) { /* */
      outfitLRunWarning( pilot, o, "message", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
      ret = 0;
   }
   pilot_outfitLunmem( env, oldmem );

   /* Recalculate if anything changed. */
   if ( pilotoutfit_modified ) {
      /* TODO pilot_calcStats can be called twice here. */
      pilot_weapSetUpdateOutfitState( pilot );
      pilot_calcStats( pilot );
   }
   pilotoutfit_modified = modified;
   return ret;
}

static void outfitLOndeath( const Pilot *pilot, PilotOutfitSlot *po,
                            const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaOndeath( o ) == LUA_NOREF )
      return;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOndeath( o ) ); /* f */
   lua_pushpilot( naevL, pilot->id );                               /* f, p */
   lua_pushpilotoutfit( naevL, po ); /* f, p, po */
   if ( nlua_pcall( env, 2, 0 ) ) {  /* */
      outfitLRunWarning( pilot, o, "ondeath", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
void pilot_outfitLOndeath( Pilot *pilot )
{
   pilot_outfitLRun( pilot, outfitLOndeath, NULL );
}

typedef struct OnanyimpactData {
   const Pilot  *t;
   const Solid  *w;
   const Outfit *o;
} OnanyimpactData;
static void outfitLOnanyimpact( const Pilot *pilot, PilotOutfitSlot *po,
                                const void *data )
{
   (void)data;
   int           oldmem;
   const Outfit *o = po->outfit;
   if ( outfit_luaOnanyimpact( o ) == LUA_NOREF )
      return;
   const OnanyimpactData *dat = data;

   nlua_env *env = outfit_luaEnv( o );

   /* Set the memory. */
   oldmem = pilot_outfitLmem( po, env );

   /* Set up the function: takeoff( p, po ) */
   lua_rawgeti( naevL, LUA_REGISTRYINDEX, outfit_luaOnanyimpact( o ) );
   lua_pushpilot( naevL, pilot->id );
   lua_pushpilotoutfit( naevL, po );
   lua_pushpilot( naevL, dat->t->id );
   lua_pushvector( naevL, dat->w->pos ); /* f, p, p, x */
   lua_pushvector( naevL, dat->w->vel ); /* f, p, p, x, v */
   lua_pushoutfit( naevL, dat->o );      /* f, p, p, x, v, o */
   if ( nlua_pcall( env, 6, 0 ) ) {      /* */
      outfitLRunWarning( pilot, o, "ondeath", lua_tostring( naevL, -1 ) );
      lua_pop( naevL, 1 );
   }
   pilot_outfitLunmem( env, oldmem );
}
void pilot_outfitLOnanyimpact( Pilot *pilot, Pilot *target, const Solid *w,
                               const Outfit *o )
{
   const OnanyimpactData data = {
      .t = target,
      .w = w,
      .o = o,
   };
   pilot_outfitLRun( pilot, outfitLOnanyimpact, &data );
}
