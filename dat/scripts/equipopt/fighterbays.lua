local fmt = require "format"

local fb = {}

function fb.spawnerOutfit( p )
   local m = p:mothership()
   if not m then return end
   local ds = p:dockslot()
   if not ds then return end
   return m:outfitGet( ds )
end

function fb.equip( p, olist )
   for k,v in pairs(olist) do
      if type(k)=="string" then
         if not p:outfitAddSlot( v, k, true ) then
            warn(fmt.f("Failed to equip '{outfit}' on ship '{pilot}' using named slot '{slot}'",
            {outfit=v, pilot=p, slot=k}))
         end
      else
         if p:outfitAdd( v, 1, true ) ~= 1 then
            warn(fmt.f("Failed to equip '{outfit}' on ship '{pilot}'",
            {outfit=v, pilot=p}))
         end
      end
   end
   return true
end

return fb
