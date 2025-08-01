require 'ai.core.core'
require 'ai.core.idle.miner'
require 'ai.core.misc.distress'

mem.lanes_useneutral = true

function create ()
   create_pre()

   local price = ai.pilot():ship():price()
   ai.setcredits( rnd.rnd(price/150, price/40) )
   mem.atk_skill = 0.3 + 0.3*rnd.sigma()

   create_post()
end

function hail ()
   -- Remove randomness from future calls
   if not mem.hailsetup then
      mem.refuel_base = rnd.rnd( 1000, 3000 )
      mem.hailsetup = true
   end

   -- Clean up
   mem.refuel        = 0
   mem.refuel_msg    = nil
   mem.bribe         = 0
   mem.bribe_prompt  = nil
   mem.bribe_prompt_nearby = nil
   mem.bribe_paid    = nil
   mem.bribe_no      = nil

   -- Refuel
   mem.refuel = mem.refuel_base
   mem.refuel_msg = _([["I'll supply your ship with fuel for {credits}."]])

   -- Communication stuff
   mem.bribe_no = _([["I don't want any problems."]])
end
