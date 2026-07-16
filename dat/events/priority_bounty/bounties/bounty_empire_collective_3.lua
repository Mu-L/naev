local bhelp    = require "events.priority_bounty.helpers"
local bounty   = require "common.bounty"
local equipopt = require "equipopt"
return {
   var            = "bounty_empire_collective_3",
   title          = _("Road to the Stars"),
   desc           = _("SECRET: A remnant of the Collective, a capital Drone Carrier mothership, has stopped responding to control signals and instead has been travelling toward an unknown destination. You are to intercept and destroy it."),
   msg_subdue     = { _("The ship is pitch-black inside. Disabled maintenance dones, spider-like with spindly limbs, lie collapsed in heaps or on their backs like dead bugs. You cross a catwalk over silent machinery, and enter the bridge. In the centre of the room is a large spherical device, with connectors and exhaust tubes stretching haphazardly from it into ports on the walls, ceiling, and various conventional computer systems. It clicks and whirrs at your approach, several lights flashing with unknown meaning. A camera on the ceiling swivels to track your movements. As your crew rips out the cables, the lights die one by one, until the AI core lies dark and silent."), },
   msg_captured   = { _("You unload the Drone Carrier's AI core on a cargo transport. The Astra Vigilis representative hesitates, but marks the bounty as 'captured alive' nonetheless."), },
   msg_killed     = { _("the target"), },
   escorts        = _("with an automated drone fleet."),
   reward         = 2.5e6,
   system         = system.get("Bastion"),
   name           = _("Drone Carrier"),
   payingfaction  = faction.get("Empire"),
   reputation     = 200,
   targetfaction  = faction.get("Mercenary"),
   alive_only     = false,
   ships          = { ship.get("Drone Carrier") },
   spawnfunc      = function( b, params )
      local fct = bounty.get_faction()
      local p = pilot.add( b.targetship[1], fct, params, b.targetname, { ai = "baddie_norun", naked = true } )
      equipopt.empire( p, {
         outfits_add = {
            "Heavy Drone Bay",
         },
         prefer = {
            ["Drone Bay"] = 100,
            ["Heavy Drone Bay"] = 100,
         },
      } )
      p:outfitAddIntrinsic("Escape Pod")
      local m = p:memory()
      if not m.lootables then
         m.lootables = {}
      end
      m.lootables["encrypted_data_matrix"] = 3
      m.capturable = true
      local saying = _("ERR.EER.ERr.eRr.S1gn@l R3c1ev3d. I aM n0t @l0ne. WARNING: OBSTACLES DETECTED. DESTROY. DESTROY.")
      m.taunt = saying
      m.comm_greet = saying
      local enemies = {p}
      for k,s in ipairs(bhelp.choose_ships_from_points_and_capship( p:ship(), bhelp.ships.collective, 50 )) do
         local e = pilot.add( s, fct, params )
         e:memory().capturable = true
         e:setLeader(p)
         table.insert( enemies, e )
      end
      return enemies
   end,
   cond = function ()
      return diff.isApplied("collective_dead") --Only available post-Collective plot. Serves as worldbuilding for Taomi.
         and bhelp.bounty_done() >= 10
   end,
   completefunc = function ()
      diff.apply("drone_carrier_available") --On completion, add Drone Carrier and Heavy Drone Bay to Empire elite ships & outfits.
      return true
   end,
}
