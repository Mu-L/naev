local bhelp    = require "events.priority_bounty.helpers"
local bounty   = require "common.bounty"
local equipopt = require "equipopt"
return {
   var            = "bounty_dvaered_flf_2",
   title          = _("Raising the Dead"),
   desc           = _("Dvaered intelligence reports another FLF operative has been sighted deeper into the nebula, in command of some kind of previously unseen battleship. We don't know where they found it, but the Dvaered can not allow FLF terrorists to possess this kind of technology."),
   msg_subdue     = { _("This vessel seems designed for an atypically small crew for a battleship. From an access terminal, a quick check of the ship's computer systems shows that most of the surviving crew is sheltering in the medbay or armoury. Dim emergency lighting guides your way through silent corridors to the bridge, where you find your target lying injured and unconscious in the captain's chair."), },
   escorts        = _("with a terrorist fleet"),
   reward         = 2.8e6,
   system         = system.get("Zylex"),
   name           = _("Wraith"),
   payingfaction  = faction.get("Dvaered"),
   reputation     = 200,
   targetfaction  = faction.get("FLF"),
   alive_only     = false,
   ships          = { ship.get("Thurion Certitude") },
   spawnfunc      = function( b, params )
      local fct = bounty.get_faction()
      local p = pilot.add( b.targetship[1], fct, params, b.targetname, {ai="baddie_norun", naked = true } )
      equipopt.thurion( p )
      p:outfitAddIntrinsic("Escape Pod")
      local m = p:memory()
      if not m.lootables then
         m.lootables = {}
      end
      m.lootables["encrypted_data_matrix"] = 2
      m.capturable = true
      local saying = _("I'm so close to finding it! And once I do, we'll have the power to crush the Dvaered and Empire alike!")
      m.taunt = saying
      m.comm_greet = saying
      local enemies = {p}
      for k,s in ipairs(bhelp.choose_ships_from_points_and_capship( p:ship(), bhelp.ships.flf, 50 )) do
         local e = pilot.add( s, fct, params )
         e:memory().capturable = true
         e:setLeader(p)
         table.insert( enemies, e )
      end
      return enemies
   end,
   cond = function ()
      return var.peek("bounty_dvaered_flf_1")
         and bhelp.bounty_done() >= 10
   end,
}
