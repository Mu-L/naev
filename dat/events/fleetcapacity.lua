--[[
<?xml version='1.0' encoding='utf8'?>
<event name="Fleet Capacity">
 <location>load</location>
 <chance>100</chance>
</event>
--]]
--[[
   Handles the player's fleet capacity.
--]]
local fmt = require "format"

local cap_tags_list = {
   ["fleetcap_10"] = 25,
}

local function compute_capacity( tags )
   local cap = 0
   for t,j in pairs(tags) do
      cap = cap + (cap_tags_list[t] or 0)
   end
   return cap
end

local function recalculate( domsg )
   local cap = 0

   if not player.evtDone("Chapter 1") then
      player.fleetCapacitySet( cap )
      return
   end

   local cur = player.fleetCapacity()
   cap = 150

   for i,e in ipairs(player.evtDoneList()) do
      cap = cap + compute_capacity( e.tags )
   end
   for i,m in ipairs(player.misnDoneList()) do
      cap = cap + compute_capacity( m.tags )
   end
   for i,o in pairs(player.outfits()) do
      local q = player.outfitNum(o)
      cap = cap + q*compute_capacity( o:tags() )
   end

   player.fleetCapacitySet( cap )

   local inc = cap-cur
   if domsg and inc > 0 then
      player.msg(fmt.f("#g".._("Fleet capacity increased to {val}!").."#0",{val=cap}))
   end
end

function eventmission_done( data )
   -- Only update if there's a tag we care about
   for t, b in pairs( data.tags ) do
      local c = cap_tags_list[t]
      if c then
         recalculate( true )
         return
      end
   end
end

function tagsbuy( o, _q )
   -- Only update if there's a tag we care about
   for t, b in pairs( o:tags() ) do
      local c = cap_tags_list[t]
      if c then
         recalculate( true )
         return
      end
   end
end

function create ()
   recalculate( false )

   hook.mission_done( "eventmission_done" )
   hook.event_done( "eventmission_done" )
   hook.outfit_buy( "tagsbuy" )
end
