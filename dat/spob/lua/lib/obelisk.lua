local vn = require 'vn'
local fmt = require "format"
local ccomm = require "common.comm"
local srs = require "common.sirius"
local tut = require "common.tutorial"

local obelisk = {}

function obelisk.init( spb, target, description, criteria )
   mem.spob = spb
   mem.target = system.get(target)
   mem.description = description
   mem.criteria = criteria
end

function obelisk.can_land ()
   -- No psychic powers
   if not srs.playerIsPsychic() then
      return false, _("The obelisk seems to be inert.")
   end

   return false, fmt.f(_("It seems like you may be able to establish psychic communication with the obelisk by hailing it with {key}."), {key=tut.getKey("hail")})
end

function obelisk.comm ()
   -- No psychic powers
   if not srs.playerIsPsychic() then
      player.msg(_("The obelisk seems to be inert."), true)
      return true
   end
   if player.pos():dist( mem.spob:pos() ) > 1000 then
      player.msg(_("You need to get closer to connect to the Obelisk."))
      return true
   end
   local activated = false
   local canactivate, requirement = mem.criteria()
   local desc = mem.description
   if type(desc)=="function" then
      desc = desc()
   end

   vn.clear()
   vn.scene()
   --local spb = ccomm.newCharacterSpob( vn, mem.spob )
   ccomm.newCharacterSpob( vn, mem.spob )
   vn.transition()
   vn.na(fmt.f(_("You tune your psychic energy to the {spb}. {description}"),
      {spb=mem.spob, description=desc}))

   vn.label("menu")
   vn.menu( function ()
      local opts = {
         { _("Close"), "leave" }
      }
      if canactivate then
         table.insert( opts, 1, {_("Activate"),"activate"} )
      else
         table.insert( opts, 1, {"#r".._("Activate").."#0","cant_activate"} )
      end
      return opts
   end )

   vn.label("cant_activate")
   vn.na(_("You muster your psychic powers to attempt to activate the obelisk, however, it seems like you do not have the powers to be able to activate this obelisk.").."\n"..requirement)
   vn.jump("menu")

   vn.label("activate")
   vn.na(_("You activate the obelisk."))
   vn.func( function () activated = true end )
   vn.done()

   vn.label("leave")
   vn.run()

   -- Player activated the obelisk
   if activated then
      var.push( "obelisk_target", mem.target:nameRaw() )
      naev.eventStart("Obelisk")
   end
   return true
end

return obelisk
