local drill = require "outfits.lib.mining_drill"
hidestats = true

function init( p, po )
   -- Since this outfit is usually off, we use shipstats to forcibly set the
   -- base stats
   po:set( "asteroid_scan", 500 )
   po:set( "mining_bonus", 100 )
   mem.isp = (p == player.pilot())
   drill.setup( p, po, {
      speed = 1.5*math.pi,
      shots_max = 2,
   })
end

ontoggle = drill.ontoggle
