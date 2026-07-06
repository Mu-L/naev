local lf = require "love.filesystem"

local bhelp = {}

bhelp.ships = {
   pirate = {
      ship.get("Pirate Hyena"),
      ship.get("Pirate Shark"),
      ship.get("Pirate Vendetta"),
      ship.get("Pirate Ancestor"),
      ship.get("Pirate Admonisher"),
      --ship.get("Pirate Revenant"),
      ship.get("Pirate Phalanx"),
      ship.get("Pirate Starbridge"),
      ship.get("Pirate Rhino"),
      ship.get("Pirate Kestrel"),
      --ship.get("Pirate Zebra"),
      --ship.get("Dealbreaker"),
   },
   mercenary = {
      ship.get("Hyena"),
      ship.get("Shark"),
      ship.get("Lancelot"),
      ship.get("Ancestor"),
      ship.get("Admonisher"),
      ship.get("Bedivere"),
      ship.get("Goddard"),
      ship.get("Kestrel"),
      ship.get("Phalanx"),
      ship.get("Pacifier"),
      ship.get("Starbridge"),
      ship.get("Tristan"),
      ship.get("Vendetta"),
      ship.get("Vigilance"),
      ship.get("Goddard"),
   },
   trader = {
      ship.get("Gawain"),
      ship.get("Llama"),
      ship.get("Koala"),
      ship.get("Quicksilver"),
      ship.get("Mule"),
      ship.get("Rhino"),
      ship.get("Clydesdale"),
      ship.get("Zebra"),
   },
   empire = {
      ship.get("Empire Shark"),
      ship.get("Empire Lancelot"),
      ship.get("Empire Admonisher"),
      ship.get("Empire Pacifier"),
      ship.get("Empire Hawking"),
      ship.get("Empire Peacemaker"),
   },
   flf = {
      ship.get("Tristan"),
      ship.get("Vendetta"),
      ship.get("Bedivere"),
      ship.get("Pacifier"),
      ship.get("Clydesdale"), -- Specially added here
   },
   frontier = { -- Slightly different from the spawn scripts
      ship.get("Hyena"),
      ship.get("Tristan"),
      ship.get("Lancelot"),
      ship.get("Ancestor"),
      ship.get("Phalanx"),
      ship.get("Bedivere"),
      ship.get("Clydesdale"), -- Specially added here
   },
   dvaered = {
      ship.get("Dvaered Vendetta"),
      ship.get("Dvaered Ancestor"),
      ship.get("Dvaered Phalanx"),
      ship.get("Dvaered Vigilance"),
      ship.get("Dvaered Retribution"),
      ship.get("Dvaered Goddard"),
   },
   sirius = {
      ship.get("Sirius Fidelity"),
      ship.get("Sirius Preacher"),
      ship.get("Sirius Shaman"),
      ship.get("Sirius Dogma"),
      ship.get("Sirius Divinity"),
   },
   zalek = {
      ship.get("Za'lek Light Drone"),
      ship.get("Za'lek Heavy Drone"),
      ship.get("Za'lek Bomber Drone"),
      ship.get("Za'lek Sting"),
      ship.get("Za'lek Demon"),
      ship.get("Za'lek Diablo"),
      ship.get("Za'lek Mephisto"),
   },
   soromid = {
      ship.get("Soromid Brigand"),
      ship.get("Soromid Reaver"),
      ship.get("Soromid Marauder"),
      ship.get("Soromid Odium"),
      ship.get("Soromid Nyx"),
      ship.get("Soromid Ira"),
      ship.get("Soromid Arx"),
   },
   proteron = {
      ship.get("Proteron Euler"),
      ship.get("Proteron Dalton"),
      ship.get("Proteron Hippocrates"),
      ship.get("Proteron Gauss"),
      ship.get("Proteron Pythagoras"),
      ship.get("Proteron Archimedes"),
      ship.get("Proteron Watson"),
   },
   collective = {
      ship.get("Drone"),
      ship.get("Heavy Drone"),
   },
}

function bhelp.choose_ships_from_points_and_capship( capship, shiplist, points )
   local maybeship
   if capship then
      local cappoints = capship:points()
      maybeship = {}
      for k,v in ipairs(shiplist) do
         local p = v:points()
         if p < cappoints then
            table.insert( maybeship, v )
         end
      end
   else
      maybeship = shiplist
   end
   -- If no ships found, return empty
   if #maybeship <= 0 then return {} end
   table.sort( maybeship, function( a, b ) return a:points() > b:points() end )
   local smallest = maybeship[ #maybeship ]:points()

   local ships = {}
   while points >= smallest do
      local candidates = rnd.permutation( maybeship )
      local s
      local id = 1
      repeat
         s = candidates[id]
         if not s then return ships end
         id = id+1
      until s:points() < points
      table.insert( ships, s )
      points = points - s:points()
   end

   return ships
end

-- Coounts how many priority bounties were done by the player
function bhelp.bounty_done()
   local nc = naev.cache()
   if not nc._priority_bounty_done then
      nc._priority_bounty_done = {}
      for k,v in ipairs(lf.getDirectoryItems("events/priority_bounty/bounties")) do
         local filename = "events.priority_bounty.bounties."..string.gsub(v,".lua","")
         local b     = require( filename )
         table.insert( nc._priority_bounty_done, b.var or v )
      end
   end

   local done = 0
   for k,v in ipairs(nc._priority_bounty_done) do
      if var.peek( v ) then
         done = done+1
      end
   end
   return done
end

-- Checks to see if the player has done a certain amount of bounties
function bhelp.cond_bounty_done( done )
   return function ()
      return bhelp.bounty_done() >= done
   end
end

-- Checks to see if the player meets a required amount of points
function bhelp.cond_bounty_points( points )
   return function ()
      return (var.peek( "astra_vigilis_points" ) or 0) >= points
   end
end

-- Combines cond_priority_bounty_done and cond_bounty_points
function bhelp.cond_bounty_points_done( points, done )
   return function ()
      return (bhelp.bounty_done() >= done) and
            ((var.peek( "astra_vigilis_points" ) or 0) >= points)
   end
end

return bhelp
