local bhelp    = require "events.priority_bounty.helpers"
local bounty   = require "common.bounty"
local equipopt = require "equipopt"
return {
   var            = "bounty_dvaered_flf_1",
   title          = _("Ghost in the Mist"),
   desc           = _("A Frontier Liberation Front terrorist going by the moniker 'Ghost' is rumored to have recovered some kind of strange ship from within the Nebula. It is reported to be difficult to track on sensors. Find and deal with it."),
   msg_subdue     = { _("Disabled, the strange ship lies eerily silent. You and your boarding team make your way through spartan cooridors to the bridge, encountering no resistance. You find your target, Ghost, and a handful of injured crew sheltering on the bridge. They drop their weapons and are quickly subdued."), },
   escorts        = _("with some other terrorists"),
   reward         = 1.7e6,
   system         = system.get("Zacron"),
   name           = _("Ghost"),
   payingfaction  = faction.get("Dvaered"),
   reputation     = 200,
   targetfaction  = faction.get("FLF"),
   alive_only     = false,
   ships          = { ship.get("Thurion Apprehension") },
   spawnfunc      = function( b, params )
      local fct = bounty.get_faction()
      local p = pilot.add( b.targetship[1], fct, params, b.targetname, {ai="baddie_norun", naked = true } )
      equipopt.thurion( p )
      p:outfitAddIntrinsic("Escape Pod")
      local m = p:memory()
      if not m.lootables then
         m.lootables = {}
      end
      m.lootables["encrypted_data_matrix"] = 1
      m.capturable = true
      local saying = _("Freedom is worth dying for!")
      m.taunt = saying
      m.comm_greet = saying
      local enemies = {p}
      for k,s in ipairs(bhelp.choose_ships_from_points_and_capship( p:ship(), bhelp.ships.flf, 150 )) do
         local e = pilot.add( s, fct, params )
         e:memory().capturable = true
         e:setLeader(p)
         table.insert( enemies, e )
      end
      return enemies
   end,
   cond = bhelp.cond_bounty_done( 5 ),
}
