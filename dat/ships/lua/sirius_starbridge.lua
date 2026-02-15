local flow = require "ships.lua.lib.flow"
local fmt = require "format"
require "ships.lua.sirius"

local MAXBONUS = 20  -- Maximum value of the bonus
local MINFLOW  = 100  -- Flow required to start getting a bonus
local OVERFLOW = 10  -- How much bonus is needed for an increment
local INCBONUS = 1   -- Increment per OVERFLOW
local BONUS    = INCBONUS / OVERFLOW

function descextra( _p, _s )
   return "#y"..fmt.f(_("For each {over} flow above {min} flow, increases max speed, turn, and acceleration by {bonus}% up to a maximum of {max}%."),
      {over=OVERFLOW, min=MINFLOW, bonus=INCBONUS, max=MAXBONUS}).."#0"
end

function update( p, _dt )
   local f = flow.get( p, mem )
   local mod = math.min( MAXBONUS, math.max( (f-MINFLOW)*BONUS, 0 ) )
   p:shippropSet{
      speed_mod = mod,
      turn_mod = mod,
      accel_mod = mod,
   }
end
