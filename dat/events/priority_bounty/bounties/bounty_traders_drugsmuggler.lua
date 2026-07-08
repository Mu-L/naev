local bhelp    = require "events.priority_bounty.helpers"
local bounty   = require "common.bounty"
local equipopt = require "equipopt"
return {
   var            = "bounty_traders_drugsmuggler",
   title          = _("Blazing in the Nebula"),
   desc           = _("A smuggler is operating around the edge of the Nebula near the border between Empire and Soromid space. Normally this wouldn't be an issue, as a healthy grey market keeps commerce moving. But in this case, they've been dealing a new highly addictive and dangerous drug, and it's attracting too much attention. Put a stop to it before the Empire is forced to crack down on regional commerce."),
   msg_subdue     = { _("Your boarding party breaches into the cargo bay of the Clydesdale, finding it full of pallet after pallet of narcotics. Making your way carefully down the length of the disabled ship, a few of the uninjured crew take potshots at you, but are quickly overwhelmed and disarmed. The door to the bridge is sealed and bolted, but a small demolition charge swiftly takes care of that problem, and within moments of entry, the notorious drug runner Eisenhorn is cuffed and on his way to your brig."), },
   escorts        = _("with a moderate escort gang."),
   reward         = 1.6e6,
   system         = system.get("Eisenhorn"),
   name           = _("Eisenburg"),
   payingfaction  = faction.get("Traders Society"),
   reputation     = 150,
   targetfaction  = faction.get("Mercenary"),
   alive_only     = false,
   ships          = { ship.get("Clydesdale Blockade Buster") },
   spawnfunc      = function( b, params )
      local fct = bounty.get_faction()
      local p = pilot.add( b.targetship[1], fct, params, b.targetname, {ai="baddie_norun", naked = true } )
      p:outfitAddIntrinsic("Escape Pod")
      equipopt.pirate( p, {
         outfits_add={"Biometal Armour"},
         prefer={["Biometal Armour"] = 100},
         type_range = {["Armour Modifier"] = { max = 2 } }, } )
      p:cargoAdd( "Neblaze", p:cargoFree() )
      local m = p:memory()
      if not m.lootables then
         m.lootables = {}
      end
      m.lootables["encrypted_data_matrix"] = 2
      m.capturable = true
      local saying = _("You're too close to the source, soul. Can't have you ruining a good thing here.")
      m.taunt = saying
      m.comm_greet = saying
      local enemies = {p}
      for k,s in ipairs(bhelp.choose_ships_from_points_and_capship( p:ship(), bhelp.ships.mercenary, 150 )) do
         local e = pilot.add( s, fct, params )
         e:memory().capturable = true
         e:setLeader(p)
         table.insert( enemies, e )
      end
      return enemies
   end,
   cond = bhelp.cond_bounty_done( 5 ),
}
