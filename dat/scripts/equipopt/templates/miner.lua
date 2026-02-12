
local optimize = require 'equipopt.optimize'
local eparams = require 'equipopt.params'
local ecores = require 'equipopt.cores'
local eoutfits = require 'equipopt.outfits'
local miner_outfits = eoutfits.merge{
   {
      -- Heavy Weapons
      -- Medium Weapons
      "Mining Lance MK2",
      "Laser Turret MK2", "Turreted Vulcan Gun", "Plasma Turret MK2",
      "Orion Beam",
      -- Small Weapons
      "Mining Lance MK1",
      "Laser Turret MK1", "Turreted Gauss Gun", "Plasma Turret MK1",
      "Laser Cannon MK1", "Gauss Gun", "Plasma Blaster MK1",
      -- Other mining-related stuff
      "S&K Heavy Plasma Drill",
      "S&K Plasma Drill",
      "Asteroid Scanner",
      "Cargo Damper",
   },
   eoutfits.standard.set,
}

--[[
-- @brief Does Miner pilot equipping
--
--    @param p Pilot to equip
--]]
local function equip_miner( p, opt_params, cores )
   opt_params  = opt_params or {}
   cores       = cores or "standard"

   local miner_params = {
      turret   = 0.5,
      launcher = 0.5,
      bolt     = 1.5,
      beam     = 1.5,
      disable  = 0, -- Don't want disable weapons
      prefer   = {
         ["Mining Lance MK1"] = 2,
         ["Mining Lance MK2"] = 2,
      },
   }
   if rnd.rnd() < 0.2 then
      miner_params.prefer["S&K Heavy Plasma Drill"] = 2
      miner_params.prefer["S&K Plasma Drill"] = 2
   end
   local params = eparams.choose( p, miner_params )
   params = tmerge_r( params, opt_params )

   -- See cores
   local ocores = cores or ecores.get( p, { all="standard" } )

   -- Try to equip
   return optimize.optimize( p, ocores, miner_outfits, params )
end

return equip_miner
