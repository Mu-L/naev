local scom = require "factions.spawn.lib.common"

local sfidelity   = ship.get("Sirius Fidelity")
local sshaman     = ship.get("Sirius Shaman")
local spreacher   = ship.get("Sirius Preacher")
local sdogma      = ship.get("Sirius Dogma")
local sdivinity   = ship.get("Sirius Divinity")

-- @brief Spawns a small patrol fleet.
local function spawn_patrol ()
   return scom.doTable( { __doscans=true }, {
      { w=0.5, sfidelity },
      { w=0.8, sfidelity, sfidelity },
      { sshaman, sfidelity },
   } )
end

-- @brief Spawns a medium sized squadron.
local function spawn_squad ()
   return scom.doTable( { __doscans=(rnd.rnd() < 0.5) }, {
      { w=0.5, spreacher, sshaman, sfidelity },
      { w=0.8, spreacher, spreacher },
      { spreacher, sshaman, sfidelity, sfidelity },
   } )
end

-- @brief Spawns a capship with escorts.
local function spawn_capship ()
   -- Generate the capship
   local pilots = scom.doTable( {}, {
      { w=0.5, sdogma },
      { sdivinity },
   } )

   -- Generate the escorts
   return scom.doTable( pilots, {
      { w=0.5, sshaman, sfidelity, sfidelity },
      { spreacher, sshaman, sfidelity },
   } )
end

return function ( t, max )
   t.patrol  = { f = spawn_patrol,  w = 300 }
   t.squad   = { f = spawn_squad,   w = math.max(1, -80 + 0.80 * max) }
   t.capship = { f = spawn_capship, w = math.max(1, -500 + 1.70 * max) }
end, 10
