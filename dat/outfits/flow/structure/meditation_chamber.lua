notactive = true -- Doesn't become active

local flow = require "ships.lua.lib.flow"
local fmt = require "format"

function descextra( p, o )
   local desc = fmt.f("#y".._("Provides {flow} maximum flow capacity. Requires a flow amplifier.").."#0",
      { flow=flow.list_base[o:nameRaw()] })
   if p and flow.is_dreamer( p ) and not o:tags().dreamer then
      desc = desc.."\n\n#r".._("Requires a flow amplifier to use.").."#0"
   end
   return desc
end
