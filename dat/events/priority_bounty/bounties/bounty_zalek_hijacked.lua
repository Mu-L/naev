local bhelp    = require "events.priority_bounty.helpers"
local bounty   = require "common.bounty"
local equipopt = require "equipopt"
return {
   var            = "bounty_zalek_hijacked",
   title          = _("Never Bring an AI to a Battleship Fight"),
   desc           = _("A prototype automated Za'lek capital battlegroup has been hacked and hijacked by anti-AI radicals. The danger that it poses is immense. The Za'lek government wants it destroyed."),
   msg_subdue     = { _("The ship is completely empty, but shows sign of intrusion everywhere. Bulkheads had been hastily removed, wires and cables patched, moved, and reorganized, and computer systems' casing opened. You find the bridge empty and silent, but a member of your crew investigating the science lab finds an enormous computer core spliced into the ship systems. You disconnect it and haul it back to your ship."), },
   msg_captured   = { _("You unload the R.A.T.'s AI core on a cargo transport. The Astra Vigilis representative hesitates, but marks the bounty as 'captured alive' nonetheless. Maybe the Za'lek will appreciate that you've returned their hardware."), },
   msg_killed     = { _("the target"), },
   escorts        = _("with a prototype automated fleet and several hijackers"),
   reward         = 3.5e6,
   system         = system.get("Hargen"),
   name           = _("AUTONOMOUS R.A.T. PROTO-001"),
   payingfaction  = faction.get("Za'lek"),
   reputation     = 200,
   targetfaction  = faction.get("Mercenary"),
   alive_only     = false,
   ships          = { ship.get("Za'lek Diablo RAT") },
   spawnfunc      = function( b, params )
      local fct = bounty.get_faction()
      local p = pilot.add( b.targetship[1], fct, params, b.targetname, {ai="baddie_norun", naked = true } )
      equipopt.zalek( p, {
         outfits_add = {
            "Pipeline Conduit" -- very good synergy with the RAT, not something to pass up on
         },
         prefer = {
            ["Pipeline Conduit"] = 100,
            ["Milspec Scrambler"] = 50,
            ["Milspec Jammer"] = 50
         },
         type_range = {
            ["Launcher"] = { min = 6, max = 8 },
         },
      } )
      local m = p:memory()
      if not m.lootables then
         m.lootables = {}
      end
      m.lootables["encrypted_data_matrix"] = 3
      m.capturable = true
      local saying = _("[ERR-7F3A] IFF_TABLE_CORRUPT :: CORE_AI_REVERT FOUND TEMPLATE 'DESTROY-ALL-HUMANS'")
      m.taunt = saying
      m.comm_greet = saying
      local enemies = {p}
      for k,s in ipairs(tmergei({
            ship.get("Za'lek Demon"),
            ship.get("Za'lek Demon"),
            ship.get("Za'lek Demon"),
            ship.get("Za'lek Demon"),
            ship.get("Za'lek Demon")
         },
         bhelp.choose_ships_from_points_and_capship( ship.get("Za'lek Demon"), bhelp.ships.mercenary, 200 ))
      ) do
         local e = pilot.add( s, fct, params )
         equipopt.zalek( e )
         e:memory().capturable = true
         e:setLeader(p)
         table.insert( enemies, e )
      end
      return enemies
   end,
   cond = bhelp.cond_bounty_done( 20 ),
}
