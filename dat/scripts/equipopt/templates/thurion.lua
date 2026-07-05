local optimize = require 'equipopt.optimize'
local ecores = require 'equipopt.cores'
local eoutfits = require 'equipopt.outfits'
local eparams = require 'equipopt.params'

local thurion_outfits = eoutfits.merge{{
   -- Heavy Weapons
   "Heavy Laser Turret", "Heavy Ripper Turret",
   "Heavy Muon Disruptor", "Supercharged Muon Disruptor",
   "Thurion Perspicacity Bay", "Thurion Scintillation Bay",
   -- Medium Weapons
   "Muon Disruptor", "Laser Turret MK2",
   "Convulsion Launcher", "Turreted Convulsion Launcher",
   "Unicorp Caesar IV Launcher", "TeraCom Medusa Launcher",
   "Thurion Perspicacity Dock",
   -- Small Weapons
   "Ripper Cannon", "Laser Turret MK1", "Light Muon Disruptor",
   -- Point Defence
   "Guardian Overseer System",
   "Guardian Interception System",
   -- Utility
   "Droid Repair Crew", "Milspec Scrambler", "Auto-Modulating Scrambler",
   "Targeting Array", "Agility Combat AI", "Unicorp Scrambler", "Hyperbolic Blink Engine",
   "Milspec Jammer", "Emergency Shield Booster", "Unicorp Jammer",
   "Sensor Array", "Agility Combat AI", "Flicker Drive",
   "Nebula Resistant Coating", "Efficiency Combat AI", "Weakness Harmonizer AI",
   -- Heavy Structural
   "Battery III", "Shield Capacitor III", "Shield Capacitor IV",
   "Reactor Class III", "Battery IV", "Auxiliary Processing Unit IV",
   "Large Shield Booster", "Auxiliary Processing Unit III",
   -- Medium Structural
   "Battery II", "Shield Capacitor II", "Reactor Class II",
   "Medium Shield Booster", "Auxiliary Processing Unit II",
   -- Small Structural
   "Improved Stabilizer", "Engine Reroute",
   "Battery I", "Shield Capacitor I", "Reactor Class I",
   "Adaptive Camouflage Plating",
   "Small Shield Booster", "Auxiliary Processing Unit I",
}}

local thurion_params = {
   ["Thurion Apprehension"] = function () return {
         type_range = {
            ["Launcher"] = { max = 1 },
         },
      } end,
   ["Thurion Certitude"] = function  () return {
         type_range = {
            ["Launcher"] = { max = 0 },
         },
      } end,
}
local thurion_cores = {
}

local thurion_params_overwrite = {
   -- Prefer to use the Thurion utilities
   prefer = {
      ["Nebula Resistant Coating"] = 100,
   },
   max_same_util = 3,
   max_same_stru = 3,
   max_same_weap = 3,
}

--[[
-- @brief Does Thurion pilot equipping
--
--    @param p Pilot to equip
--]]
local function equip_thurion( p, opt_params )
   opt_params = opt_params or {}
   local ps    = p:ship()
   local sname = ps:nameRaw()

   -- Choose parameters and make Thurion-ish
   local params = eparams.choose( p, thurion_params_overwrite )
   params.max_mass = 0.95 + 0.1*rnd.rnd()
   -- Per ship tweaks
   local sp = thurion_params[ sname ]
   if sp then
      params = tmerge_r( params, sp() )
   end
   params = tmerge_r( params, opt_params )

   -- Outfits
   local outfits = thurion_outfits
   if opt_params.outfits_add then
      outfits = eoutfits.merge{ outfits, opt_params.outfits_add }
   end

   -- See cores
   local cores = opt_params.cores
   if not cores then
      local thucor = thurion_cores[ sname ]
      cores = ecores.get( p, { all="elite" } )
      if thucor then
         cores = tmerge( cores, thucor() )
      end
   end

   -- Set some meta-data
   local mem = p:memory()
   mem.equip = { type="thurion", level="elite" }

   -- Try to equip
   return optimize.optimize( p, cores, outfits, params )
end

return equip_thurion
