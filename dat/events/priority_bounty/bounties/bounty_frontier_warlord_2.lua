local bhelp    = require "events.priority_bounty.helpers"
local bounty   = require "common.bounty"
local equipopt = require "equipopt"
return {
   var            = "bounty_frontier_warlord_2",
   title          = _("Flak 2, Explosive Boogaloo"),
   desc           = _("Lord Flakfinger's rich aunt paid his bail, and he went right back to shooting at Frontier merchant traffic, this time in a larger ship. Dead or alive this time."),
   msg_subdue     = { _("This Retribution cruiser is brand new, straight out of the Dvaered shipyards. Gleaming metal constrasts with battle damage from your weapons, and the shouts and groans of injured crew fill the air. They're too busy collecting their injured and trying to keep the ship intact to resist your boarding party. You breach the bridge within minutes, where before he can say a word, Lord Flakfinger is quickly subdued by a solid punch to the face. He's dragged out uncerimoniously by your crew."), },
   escorts        = _("with a hired fleet"),
   reward         = 2.3e6,
   system         = system.get("Nougat"),
   name           = _("Lord Flakfinger"),
   payingfaction  = faction.get("Frontier"),
   reputation     = 200,
   targetfaction  = faction.get("Mercenary"),
   alive_only     = false,
   ships          = { ship.get("Dvaered Retribution") },
   spawnfunc      = function( b, params )
      local fct = bounty.get_faction()
      local p = pilot.add( b.targetship[1], fct, params, b.targetname, {ai="baddie_norun", naked = true } )
      p:outfitAddIntrinsic("Escape Pod")
      equipopt.dvaered( p, {
         outfits_add = {
            "Flak Gun"
         },
         prefer = {
            ["Flak Gun"] = 100,
            ["Super-Fast Collider Launcher"] = 100
         },
         type_range = {
            ["Bolt Weapon"] = { min = 4 },
            ["Launcher"] = { max = 2 },
         },
      } )
      local m = p:memory()
      if not m.lootables then
         m.lootables = {}
      end
      m.lootables["encrypted_data_matrix"] = 2
      m.capturable = true
      local saying = _("You peasants will pay for what you did to me!")
      m.taunt = saying
      m.comm_greet = saying
      local enemies = {p}
      for k,s in ipairs(bhelp.choose_ships_from_points_and_capship( p:ship(), bhelp.ships.dvaered, 300 )) do
         local e = pilot.add( s, fct, params )
         e:memory().capturable = true
         e:setLeader(p)
         table.insert( enemies, e )
      end
      return enemies
   end,
   cond = function ()
      return var.peek("bounty_frontier_warlord_1")
         and bhelp.bounty_done() >= 5
   end,
}
