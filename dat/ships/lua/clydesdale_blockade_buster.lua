local fmt = require "format"

local RANGE       = 3000
local BONUS       = 5
local BONUSMAX    = 25
local POINT_STEP  = 50

function descextra( _p, _s )
   return "#y"..fmt.f(_([[For each {step} fleet capacity points worth of hostile ships within {range} distance, gain {bonus}% additional turn, speed, and signature reduction up to a maximum of {bonusmax}%. Range is affected by detection bonus.]]), {
      bonus = BONUS,
      bonusmax = BONUSMAX,
      step  = POINT_STEP,
      range = RANGE
   })
end

-- Init function run on creation
function init( p )
   mem.nearbyPoints = 0
   p:shippropReset( p )
end

function update( p, _dt )
   -- Get points of nearby enemies
   local mod = p:shipstat("ew_detect", true)
   local totalPoints = 0
   for k,h in ipairs(p:getEnemies( RANGE * mod )) do
      totalPoints = totalPoints + h:ship():points()
   end

   -- Update when enemies in range change
   if totalPoints ~= mem.nearbyPoints then
      if totalPoints >= POINT_STEP then
         local bonusMultiplier = math.floor(totalPoints / POINT_STEP)
         local totalBonus = math.min(bonusMultiplier * BONUS, BONUSMAX)

         p:shippropSet{
            turn_mod       =  totalBonus,
            speed_mod      =  totalBonus,
            ew_signature   = -totalBonus,
         }
      else
         p:shippropReset( p )
      end

      mem.nearbyPoints = totalPoints
   end
end
