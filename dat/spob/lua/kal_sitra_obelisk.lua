local obelisk = require "spob.lua.lib.obelisk"
local fmt = require "format"

local reward = outfit.get("Avatar of the Sirichana")
local reqs = {
   outfit.get("Cleansing Flames"),
   outfit.get("Astral Projection"),
   outfit.get("Feather Drive"),
}

function init( spb )
   local description = function ()
      if player.outfitNum( reward ) > 0 then
         return _("You already have acquired a flow ability from the Obelisk, however, you can activate it to retry the test if you wish.")
      else
         return fmt.f(_("You will be able to acquire the {reward} ability by passing the Obelisk's Test."), {reward=reward} )
      end
   end
   return obelisk.init( spb, "Test of Devotion", description, function ()
      for k,o in ipairs(reqs) do
         if player.outfitNum( o ) <= 0 then
            local desc = _("You need the following flow abilities to be able to activate the Obelisk:")
            for i,r in ipairs(reqs) do
               if player.outfitNum( r ) <= 0 then
                  desc = desc.."\n#r"..r:name().."#0"
               else
                  desc = desc.."\n"..r:name()
               end
            end
            return false, desc
         end
      end
      local fct = faction.get("Sirius")
      local minstanding = 45
      local curstanding = fct:reputationGlobal()
      if curstanding < minstanding then
         return false, fmt.f(_("You need at least {standing} with {fct} (you have {current})."),
            {standing=minstanding,fct=fct:longname(),current=fmt.number(curstanding)})
      end
      return true, ""
   end )
end

-- Set up globals
can_land = obelisk.can_land
comm = obelisk.comm
