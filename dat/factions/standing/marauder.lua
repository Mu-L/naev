-- Marauder faction standing script
local spir = require "factions.standing.lib.pirate"
spir.init{
   fct            = faction.get("Marauder"),
   text = {
      [0]  = _("Ignored"),
      [-1] = _("Potential Victim"),
   },
}
--friendly_at    = 50
function hit()
   return 0 -- Doesn't change through hits
end
