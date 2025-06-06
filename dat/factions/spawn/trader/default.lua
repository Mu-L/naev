local scom = require "factions.spawn.lib.common"
local var = require "shipvariants"

local szebra      = ship.get("Zebra")
local srhino      = ship.get("Rhino")
local splowshare  = ship.get("Plowshare")

local tradelane

local function add_llama( pilots )
   scom.addPilot( pilots, var.llama, {name=_("Trader Llama")})
end
local function add_koala( pilots )
   scom.addPilot( pilots, var.koala, {name=_("Trader Koala")})
end
local function add_quicksilver( pilots )
   scom.addPilot( pilots, var.quicksilver, {name=_("Trader Quicksilver")})
end
local function add_mule( pilots )
   scom.addPilot( pilots, var.mule, {name=_("Trader Mule")})
end
local function add_zebra( pilots )
   scom.addPilot( pilots, szebra, {name=_("Trader Zebra")})
end
local function add_rhino( pilots )
   scom.addPilot( pilots, srhino, {name=_("Trader Rhino")})
end
local function add_shark( pilots )
   scom.addPilot( pilots, var.shark, {name=_("Trader Shark"), ai="mercenary"})
end
local function add_plowshare( pilots )
   scom.addPilot( pilots, splowshare, {name=_("Trader Plowshare")})
end

-- Doubles the credits of the pilot
local function double_credits( p )
   p:credits( p:credits() )
end

-- @brief Spawns a small trade fleet.
local function spawn_loner ()
   local pilots = {}
   local r = rnd.rnd()

   -- Plowshare only appear in tradelanes
   if r < 0.5 and tradelane then
      add_plowshare( pilots )
      return pilots
   else
      r = rnd.rnd() -- New random number
   end

   -- Regular loners
   if r < 0.3 then
      add_llama( pilots )
   elseif r < 0.5 then
      add_koala( pilots )
   elseif r < 0.7 then
      add_quicksilver( pilots )
   elseif r < 0.79 then
      add_mule( pilots )
   elseif r < 0.85 then
      add_zebra( pilots )
   else
      add_rhino( pilots )
   end

   return pilots
end

local function spawn_fleet_small ()
   local pilots = {}

   for i=1,rnd.rnd(2,5) do
      local r = rnd.rnd()
      if r < 0.5 then
         add_llama( pilots )
      elseif r < 0.8 then
         add_koala( pilots )
      else
         add_quicksilver( pilots )
      end
   end

   return pilots
end

local function spawn_fleet_small_guarded ()
   local pilots = {}

   -- Base Fleet
   for i=1,rnd.rnd(2,4) do
      local r = rnd.rnd()
      if r < 0.5 then
         add_llama( pilots )
      elseif r < 0.8 then
         add_koala( pilots )
      else
         add_quicksilver( pilots )
      end
   end

   -- Give more money
   for k,p in ipairs(pilots) do
      p.postprocess = double_credits
   end

   -- Some Guards
   for i=1,rnd.rnd(1,3) do
      add_shark( pilots )
   end

   return pilots
end


local function spawn_fleet_med ()
   local pilots = {}

   -- Leader
   local big_leader = false
   local r = rnd.rnd()
   if r < 0.2 then
      add_zebra( pilots )
      big_leader = true
   elseif r < 0.6 then
      add_mule( pilots )
      big_leader = true
   else
      add_rhino( pilots )
   end

   -- Determine type of fleet (small or large ships)
   if rnd.rnd() < 0.5 then
      for i=2,4 do
         r = rnd.rnd()
         if r < 0.3 then
            add_llama( pilots )
         elseif r < 0.8 then
            add_koala( pilots )
         else
            add_quicksilver( pilots )
         end
      end
   else
      for i=1,2 do
         if big_leader or rnd.rnd() < 0.6 then
            add_mule( pilots )
         else
            add_rhino( pilots )
         end
      end
   end

   -- Some Guards
   for i=1,rnd.rnd(3,5) do
      add_shark( pilots )
   end

   return pilots
end

local function spawn_fleet_med_guarded ()
   local pilots = spawn_fleet_med ()

   -- Give more money
   for k,p in ipairs(pilots) do
      p.postprocess = double_credits
   end

   -- Some Guards
   for i=1,rnd.rnd(3,5) do
      add_shark( pilots )
   end

   return pilots
end

local ftrader = faction.get("Trader")
return function ( t, max )
   -- Hostiles (namely pirates atm)
   local host = 0
   local total = 0
   local csys = system.cur()
   for f,v in pairs(csys:presences()) do
      if ftrader:areEnemies(f) then
         host = host + v
      end
      total = total + v
   end
   local hostnorm = host / total

   -- Hermite interpolation
   hostnorm = math.pow(hostnorm,2) * (3-hostnorm)

   -- Whether or not system is a tradelane
   tradelane = csys:tags().tradelane

   -- Create weights for spawn table
   t.loner = { f=spawn_loner, w=400 } -- codespell:ignore loner
   t.fleet_small = { f=spawn_fleet_small, w=math.max(1, -150, max ) * (1-hostnorm) }
   t.fleet_small_guarded = { f=spawn_fleet_small_guarded, w=math.max(1, -200, max ) * hostnorm }
   t.fleet_med = { f=spawn_fleet_med, w=math.max(1, -300 + max) * (1-hostnorm) }
   t.fleet_med_guarded = { f=spawn_fleet_med_guarded, w=math.max(1, -300 + max) * (1-hostnorm) }
end, 10
