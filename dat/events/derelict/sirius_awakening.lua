local der = require 'common.derelict'
local vn = require 'vn'
local vne = require "vnextras"
local fmt = require 'format'
local srs = require "common.sirius"
local tut = require "common.tutorial"

return function ()
   -- Must not be psychic
   if var.peek("sirius_awakening") or srs.playerIsPsychic() then
      return
   end

   -- Must have sirius presence
   if system.cur():presence("Sirius") <= 0 then
      return
   end

   -- We want to prioritize "Eye of Night Mystery" start, so wait until the player knows sirius space a lot
   if not system.get("Eye of Night"):known() then
      return
   end
   local known_sirian_sys = 0
   local fsirius = faction.get("Sirius")
   for k,s in ipairs(system.getAll()) do
      if s:known() and s:faction() == fsirius then
         known_sirian_sys = known_sirian_sys+1
      end
   end
   if known_sirian_sys < 10 then
      return
   end

   return {
      ship = ship.get("Sirius Preacher"),
      weight = 3,
      func = function ()
         vn.clear()
         vn.scene()
         vn.sfx( der.sfx.board )
         vn.music( der.sfx.ambient )
         vn.transition()

         vn.na(_("You carefully board the derelict and begin to explore the ship. Although it has no power, the ship still has a breathable atmosphere once you make it through the airlock. You start looking through the cargo hold, but it seems to be picked clean."))
         vn.na(_("Eventually you make your way towards the cockpit, but something smells off. When you finally open the door, a pungent odour assaults you, forcing you to activate your external oxygen while you gag. It's a close call, but you manage to keep yourself together."))
         vn.na(_("Regaining your senses, you make out some bodies in the room. A quick inspection reveals that they are starting to decompose. There's nothing of value, but you have to decide what to do next."))

         vn.menu( function ()
            local opts = {
               {_("Return to your ship"),"leave"},
               {_("Eject the bodies to space"), "eject"},
            }
            if system.cur():reputation(faction.get("Sirius")) >= 0 then
               table.insert( opts, {_("Notify local Sirius authorities"), "notify"} )
            end
            return opts
         end )

         vn.label("notify")
         vn.sfx( der.sfx.unboard )
         vn.na(_("You head back to your ship and notify the local Sirius authorities about the issue. They thank you for the information."))
         vn.func( function ()
            faction.hit( "Sirius", 2 )
         end )
         vn.jump("cont01")

         vn.label("eject")
         vn.na(_("You decide to give the bodies a space funeral and eject them out of the airlock, letting them float among the stars. With the deed done, you do a second pass over the ship's interior and find a small credit chip that you stash before heading back to your ship."))
         local creditchip = 50e3
         vn.func( function ()
            player.pay( creditchip )
         end )
         vn.na( fmt.reward( creditchip ) )
         vn.sfx( der.sfx.unboard )
         vn.jump("cont01")

         vn.label("leave")
         vn.na(_("You decide that it's best to forget about the whole derelict and get back to your ship. Nothing good can come out of this."))
         vn.sfx( der.sfx.unboard )
         vn.jump("cont01")

         vn.label("cont01")
         vn.na(_("With everything settled, you sit in your captain's chair and are about to get back to the controls when you are suddenly overcome by a horrible headache."))
         vn.func( function ()
            var.push("sirius_awakening",true)
         end )

         vn.music("snd/sounds/loops/kalimba_atmosphere.ogg")
         vn.scene()
         local memory = vne.flashbackTextStart( _("Memory"), {transition="blinkin"})
         local function m( txt ) memory("\n"..txt,true) end
         memory(_("You feel a light breeze caressing you."))
         m(_("Is that..."))
         m(_("Is that the sound of the ocean?"))
         m(_("..."))
         m(_("It feels good."))
         m(_("..."))
         m(_("Wait, someone is sitting next to you."))
         m(_("Who could they be?"))
         vn.func( function ()
            srs.sfxGong()
         end )
         vne.flashbackTextEnd{ notransition=true }

         --vn.scene() -- vn.scene() is done in vne.flashbackTextEnd
         local sai = vn.newCharacter( tut.vn_shipai() )
         vn.transition( "blinkout" )

         vn.na(fmt.f(_("You awake to your ship AI {shipai} looming over you."),
            {shipai=tut.ainame()}))
         sai(fmt.f(_([["Are you alright, {pilotname}? You usually wake up after a single electric shock, but this time you did not respond and had me worried."]]),
            {pilotname=player.name()}))

         vn.menu{
            {_([["Electric shock?"]]), "cont02"},
            {_([["My head is killing me."]]), "cont02"},
         }

         vn.label("cont02")
         sai(_([["It's great to have you back! Maybe you should get a doctor to look at your head this time."]]))
         vn.disappear( sai, tut.shipai.transition )
         vn.na(fmt.f(_("{shipai} quickly dematerializes, leaving you alone with your throbbing headache. It'll probably go away soon..."),
            {shipai=tut.ainame()}))

         vn.run()
         player.unboard()
         return true
      end
   }
end
