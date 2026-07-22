notactive = true -- Doesn't become active

local flow = require "ships.lua.lib.flow"
local fmt = require "format"

function descextra( p, o )
   local desc = fmt.f("#y".._("Provides {regen} flow regeneration per second up to 50% maximum capacity. Requires a flow amplifier.").."#0",
      { regen=flow.list_regen[o:nameRaw()] })
   if p and flow.is_dreamer( p ) and not o:tags().dreamer then
      desc = desc.."\n\n#r".._("Requires a flow amplifier to use.").."#0"
   end
   return desc
end
