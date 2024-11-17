--[[
<?xml version='1.0' encoding='utf8'?>
<mission name="Assault on Unicorn">
 <priority>3</priority>
 <cond>
   if spob.cur():reputation("Dvaered") &lt; 5 then
      return false
   end
   if system.get("Unicorn"):jumpDist() &gt; 5 then
      return false
   end
   if player.misnActive("Assault on Unicorn") then
      return false
   end
   local misn_test = require "misn_test"
   if not misn_test.mercenary() then
      return false
   end
   return true
 </cond>
 <faction>Dvaered</faction>
 <chance>36</chance>
 <location>Computer</location>
 <done>Empire Shipping 3</done>
 <notes>
  <tier>3</tier>
 </notes>
</mission>
 --]]
--[[

   MISSION: Assault on Unicorn
   DESCRIPTION: Kill some pirates!

--]]
local pir = require 'common.pirate'
local fmt = require "format"
local dv  = require "common.dvaered"
local vn = require "vn"

-- Mission constants
local misn_target_sys = system.get("Unicorn")
-- local misn_return_sys = system.get("Amaroq")

local function update_osd()
   local osd_msg = {}
   osd_msg[1] = _("Fly to the Unicorn system.")
   if mem.bounty_earned == mem.max_payment then
      osd_msg[2] = fmt.f(_("You have reached your maximum payment. Return to {pnt} ({sys} system)."),
         {pnt=mem.planet_start, sys=mem.system_start})
   else
      osd_msg[2] = fmt.f(_("Destroy some pirates! You have killed {n} and have earned {credits}. If finished, return to {pnt} ({sys} system)."),
         {n=mem.pirates_killed, credits=fmt.credits(mem.bounty_earned), pnt=mem.planet_start, sys=mem.system_start})
   end
   misn.osdCreate(_("Assault on Unicorn"), osd_msg)
end

function create ()
   local rep = spob.cur():reputation("Dvaered")
   -- Round the payment to the nearest thousand.
   mem.max_payment = rep * 50e3
   misn.setTitle(dv.prefix.._("Assault on Unicorn"))
   misn.setReward(_("Variable"))
   misn.setDesc(fmt.f(_("It is time to put a dent in the pirates' forces. We have detected a strong pirate presence in the system of Unicorn. We are offering a small sum for each pirate killed. The maximum we will pay you is {credits}."),
      {credits=fmt.credits(mem.max_payment)} ))

   mem.marker = misn.markerAdd( misn_target_sys, "computer" )
end

function accept ()
   -- This mission makes no system claims.
   if misn.accept() then
      mem.pirates_killed = 0
      mem.planet_start, mem.system_start = spob.cur()
      mem.marker2 = misn.markerAdd( mem.planet_start, "low" )
      mem.pirates_killed = 0
      mem.bounty_earned = 0
      mem.misn_stage = 0
      update_osd()
      misn.osdActive(1)

      hook.enter("jumpin")
      hook.land("land")
   end
end

function jumpin()
   if system.cur() == misn_target_sys then
      misn.osdActive(2)
      hook.pilot(nil, "death", "death")
   end
end

function death( pilot, killer )
   if pir.factionIsPirate(pilot:faction()) and killer and killer:withPlayer() then
      local reward_earned = pilot:ship():price()/10
      mem.pirates_killed = mem.pirates_killed + 1
      mem.bounty_earned = math.min( mem.max_payment, mem.bounty_earned + reward_earned )
      update_osd()
      misn.osdActive(2)
   end
end

function land()
   if spob.cur() == mem.planet_start and mem.pirates_killed > 0 then

      vn.clear()
      vn.scene()
      vn.transition()
      vn.na(fmt.f(_("As you land, you see a Dvaered military official approaching. Thanking you for your hard and diligent work, he hands you the bounty you've earned, a number of chips worth {credits}."),
         {credits=fmt.credits(mem.bounty_earned)}))
      vn.sfxMoney()
      vn.func( function ()
         player.pay(mem.bounty_earned)
         faction.hit( "Dvaered", math.pow( mem.bounty_earned, 0.5 ) / 100 )
      end )
      vn.na(fmt.reward(mem.bounty_earned))
      vn.run()

      misn.finish(true)
   end
end
