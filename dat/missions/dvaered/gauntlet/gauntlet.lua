--[[
<?xml version='1.0' encoding='utf8'?>
<mission name="Crimson Gauntlet">
 <priority>3</priority>
 <chance>100</chance>
 <location>Bar</location>
 <spob>Totoran</spob>
</mission>
--]]

local vn = require 'vn'
local gauntlet = require 'common.gauntlet'
local gauntlet_gui = require 'missions.dvaered.gauntlet.gui'
local tables = require 'missions.dvaered.gauntlet.tables'
local fmt = require "format"
local equipopt = require 'equipopt'
local flow = require "ships.lua.lib.flow"

local logidstr = "log_gauntlet"
local enemies, enemy_faction, gmods, wave_enemies, wave_killed -- Non-persistent state
local wave_end -- Forward-declared functions

local npc_portrait   = "/gfx/misc/crimson_gauntlet.webp"
local npc_description= _("A terminal to access the Crimson Gauntlet Virtual Reality environment. This directly allows you to enter the different challenges and tournaments available.")

local gauntletsys = system.get("Crimson Gauntlet")

local sfx_clear = audio.new( 'snd/sounds/jingles/victory.ogg' )

function create ()
   misn.npcAdd( "approach_gauntlet", _("Crimson Gauntlet Terminal"), npc_portrait, npc_description )
end

-- Land is unified for all types of combat
function land ()
   local rewardcredits = mem.total_score
   -- TODO only give emblems from bosses or special clears?
   local rewardemblems = mem.total_score/100

   vn.clear()
   vn.scene()
   vn.transition()
   vn.sfxMoney()
   vn.func( function ()
      player.pay( rewardcredits, "noescorts" )
      gauntlet.emblems_pay( rewardemblems )
   end )
   vn.na(fmt.reward( rewardcredits ) .. "\n" .. fmt.reward( gauntlet.emblems_str( rewardemblems ) ))
   vn.run()

   misn.finish(true)
end

function approach_gauntlet ()
   mem.gtype, mem.gopt, mem.gsubtype, gmods = gauntlet_gui.run()
   if mem.gtype == nil then
      return
   end

   mem.wave_category = string.lower( mem.gopt )
   mem.wave_subcategory = string.lower( mem.gsubtype )

   -- See if we start the event
   if mem.wave_category==nil or mem.wave_subcategory==nil then
      return
   end

   -- Accept mission
   misn.accept()

   -- We try to claim if possible, but it's not a failure if we can't
   -- This allows it to work with kex03
   if naev.claimTest( gauntletsys ) then
      misn.claim( gauntletsys )
   end

   -- Set details
   misn.setDesc( _("Annihilate all enemies in the Crimson Gauntlet.") )
   misn.setReward( _("Great riches!") )
   misn.setTitle( _("Crimson Gauntlet Challenge") )

   -- Add to log
   shiplog.create( logidstr, _("Totoran Tournament"), _("Totoran Tournament") )

   -- Create the OSD
   misn.osdCreate( _("Crimson Gauntlet"),
         { _("Defeat all the other adversaries!") } )

   hook.load( "loaded" )
   hook.safe("enter_the_ring")
   player.takeoff() -- take off and enter the ring!

   -- Wave meta-information
   mem.gauntlet_enter = "enter_wave"
   mem.wave_round = 1
end
function loaded ()
   misn.finish(false)
end
function abort ()
   if system.cur() == gauntletsys then
      leave_the_ring()
   end
end

-- Clears pilots include the player's escorts
function clear_pilots ()
   gauntlet.clear_pilots()
end

--[[
   Common Teleporting Functions
--]]
-- Enters Crimson Gauntlet
function enter_the_ring ()
   -- Failed to take off
   if player.isLanded() then
      -- Have to add a new mission because we accepted this one
      naev.missionStart("Crimson Gauntlet")
      misn.finish(false)
   end

   -- Teleport the player to the Crimson Gauntlet and hide the rest of the universe
   hook.safe( mem.gauntlet_enter ) -- Will defer one frame, hook.enter triggers in the middle of enter_the_ring which is undesirable
   gauntlet.enter_the_ring()

   -- Player lost info
   local pp = player.pilot()
   mem.pp_hook_disable = hook.pilot( pp, "disable", "player_lost_disable" )
   mem.pp_hook_dead = hook.pilot( pp, "exploded", "player_lost" )
end
-- Goes back to Totoran (landed)
function leave_the_ring ()
   gauntlet.leave_the_ring()
   hook.land("land")
   player.land( spob.get("Totoran") )
end

--[[
   Countdown stuff
--]]
local function countdown_start ()
   mem.omsg_id = player.omsgAdd( _("5…"), 1.1 )
   hook.timer( 1.0, "countdown", _("4…") )
   hook.timer( 2.0, "countdown", _("3…") )
   hook.timer( 3.0, "countdown", _("2…") )
   hook.timer( 4.0, "countdown", _("1…") )
   hook.timer( 5.0, "countdown_done" )
end
function countdown( str )
   -- TODO play countdown sound
   player.omsgChange( mem.omsg_id, str, 1.1 )
end
function countdown_done ()
   -- TODO play sound and cooler text
   player.omsgChange( mem.omsg_id, _("FIGHT!"), 3 )
   mem.wave_started = true
   mem.wave_started_time = naev.ticksGame()

   for k,p in ipairs(enemies) do
      p:setInvincible(false)
      p:control(false)
      -- We let them do their thing
   end
end


--[[
   Common functions
--]]
local function enemy_out( p )
   local idx = nil
   for k,v in ipairs(enemies) do
      if v==p then
         idx=k
         break
      end
   end
   if idx then
      table.remove( enemies, idx )
   end
   if mem.wave_started and #enemies == 0 then
      mem.wave_started = false
      mem.all_enemies_dead()
   end
end
function p_disabled( p )
   p:setDisable() -- don't let them come back
   p:setInvisible( true ) -- can't target
   p:setInvincible( true ) -- just stays there
   enemy_out( p )
end
function p_death( p )
   enemy_out( p )
end
function player_lost_disable ()
   local pp = player.pilot()
   if not pp:disabled() then
      return
   end
   player.cinematics( true )
   pp:setInvincible( true )
   pp:setInvisible( true )
   -- omsg does not display when dead so we will need a custom solution
   --player.omsgAdd( _("YOU LOST!"), 4.5 )
   if not mem.leave_hook then
      mem.leave_hook = hook.timer( 5.0, "leave_the_ring" )
   end
end
function player_lost ()
   local pp = player.pilot()
   if pp:armour() > 0 then
      -- Something revived the player, ZD-5 Guardian?
      return
   end
   hook.rm( mem.pp_hook_disable )
   pp:setHealth( 100, 100 ) -- Heal up to avoid game over if necessary
   pp:setHide( true )
   pp:setInvincible( true )
   player.cinematics( true )

   -- omsg does not display when dead so we will need a custom solution
   --player.omsgAdd( _("YOU LOST!"), 5 )
   --shiplog.append( logidstr, string.format(_("You defeated a %s in one-on-one combat."), enemy_ship) )
   if not mem.leave_hook then
      mem.leave_hook = hook.timer( 3.0, "leave_the_ring")
   end
end


--[[
   Wave stuff
--]]
function enter_wave () -- luacheck: globals enter_wave (passed by reference)
   if system.cur() ~= system.get("Crimson Gauntlet") then
      return
   end

   -- Get rid of pilots
   clear_pilots()
   pilot.toggleSpawn(false)

   -- Metafactions
   enemy_faction = faction.dynAdd( "Mercenary", "Combatant", _("Combatant") )

   -- Start round
   mem.total_score = 0
   wave_round_setup()
end
function wave_round_setup ()
   clear_pilots() -- clear remaining pilots if necessary

   -- Heal up and be nice to the player
   local pp = player.pilot()
   if not gmods.nohealing then
      pp:setHealth( 100, 100, 0 )
      pp:setEnergy( 100 )
   end
   pp:fillAmmo() -- Have to fill ammo or deployed fighters get "lost"
   pp:outfitsReset()
   flow.reset( pp )
   pp:setPos( vec2.new( 0, 0 ) ) -- teleport to middle
   pp:setVel( vec2.new( 0, 0 ) )

   local function addenemies( ships, equipfunc )
      local e = {}
      local posbase = gauntletsys:waypoints("gauntlet_pos_base")
      local boss = nil
      local layout = ships.layout or "cluster"
      local pos = posbase
      for k,v in ipairs(ships) do
         -- Determine position
         if layout=="circle" then
            local d,a = posbase:polar()
            a = a +(k-1) * 2*math.pi / #ships
            pos = vec2.newP( d, a )
         elseif layout=="pincer" then
            local offset = vec2.newP( 300+200*rnd.rnd(), rnd.angle() )
            if math.fmod(k,2)==1 then
               pos = posbase + offset
            else
               local x, y = posbase:get()
               pos = vec2.new(-x,-y) + offset
            end
         elseif layout=="cluster" then
            pos = posbase + vec2.newP( 300+200*rnd.rnd(), rnd.angle() )
         else
            warn(string.format("unknown layout '%s'",layout))
         end

         -- Add ship
         local shipname = v
         local p
         if ships.func then
            p = ships.func( shipname, enemy_faction, pos, k )
         else
            p = pilot.add( shipname, enemy_faction, pos, nil, {ai="baddie_norun", naked=true} )
            if equipfunc then
               equipfunc( p )
            else
               equipopt.generic( p, nil, "elite" )
            end
         end
         p:setInvincible(true)
         p:control(true)
         p:setHostile(true)
         p:brake()
         p:face( pp )
         if gmods.doubledmgtaken then
            p:intrinsicSet("fwd_damage",   100)
            p:intrinsicSet("tur_damage",   100)
            p:intrinsicSet("launch_damage",100)
            p:intrinsicSet("fbay_damage",  100)
         end
         local aimem = p:memory()
         aimem.comm_no = _("No response.") -- Don't allow talking
         hook.pilot( p, "disable", "p_disabled" )
         hook.pilot( p, "death", "p_death" )
         if boss then
            p:setLeader( boss )
         else
            boss = p
         end
         table.insert( e, p )
      end
      return e
   end

   local round_enemies = tables.wave_round_enemies[mem.wave_subcategory][mem.wave_category]
   local enemies_list = round_enemies[mem.wave_round]
   if gmods.doubleenemy then
      local doublelist = {}
      for k,v in ipairs(enemies_list) do
         table.insert( doublelist, v )
         table.insert( doublelist, v )
      end
      enemies_list = doublelist
   end
   enemies = addenemies( enemies_list, round_enemies.equip )
   wave_enemies = enemies_list

   -- Count down
   player.omsgAdd( string.format( "#p".._("WAVE %d").."#0", mem.wave_round ), 8 )
   countdown_start()

   mem.all_enemies_dead = wave_end
end
local function wave_compute_score ()
   local pp = player.pilot()
   local score = 0
   -- Positive bonuses are addeded, negative are multiplied
   local bonus = 1
   local bonus_mul = 1
   local str = {}

   local elapsed = (naev.ticksGame()-mem.wave_started_time)
   table.insert( str, string.format(_("%.1f seconds"), elapsed) )

   wave_killed = wave_killed or {}
   for k,n in ipairs(wave_enemies) do
      local s = tables.wave_score[n]
      table.insert( str, string.format("#o%s %d", _(n), s ) )
      score = score + s
      -- Store all the stuff the pilot killed
      local killed = wave_killed[n] or 0
      wave_killed[n] = killed+1
   end

   local function newbonus( s, b )
      local h
      if b > 0 then
         h = "#g"
         bonus = bonus + b
      else
         h = "#r"
         bonus_mul = bonus_mul * (1 + b)
      end
      table.insert( str, h .. string.format(s,b*100) )
   end
   local s = pp:ship()
   local c = _(s:class())
   local civ = s:tags().civilian
   if mem.wave_category == "skirmisher" then
      local SIZES = {
         [1] = 0.10,
       --[2] = 0.00,
         [3] = -0.20,
         [4] = -0.40,
         [5] = -0.80,
         [6] = -0.90,
      }
      local b = SIZES[pp:ship():size()]
      if civ then
         b = (b or 0) + 0.10
      end
      if b then
         newbonus( fmt.f(_("{class} %d%%"), {class=c} ), b )
      end
      if elapsed < 15 then
         newbonus( _("Fast Clear (<15s) %d%%"), 0.25 )
      elseif elapsed > 90 then
         newbonus( _("Slow Clear (>90s) %d%%"), -0.25 )
      end
   elseif mem.wave_category == "warrior" then
      local SIZES = {
         [1] = 0.15,
         [2] = 0.10,
         [3] = 0.25,
       --[4] = 0.00,
         [5] = -0.20,
         [6] = -0.30,
      }
      local b = SIZES[pp:ship():size()]
      if civ then
         b = (b or 0) + 0.25
      end
      if b then
         newbonus( fmt.f(_("{class} %d%%"), {class=c} ), b )
      end
      if elapsed < 25 then
         newbonus( _("Fast Clear (<25s) %d%%"), 0.25 )
      elseif elapsed > 120 then
         newbonus( _("Slow Clear (>120s) %d%%"), -0.25 )
      end
   elseif mem.wave_category == "warlord" then
      local SIZES = {
         [1] = 7.00,
         [2] = 5.00,
         [3] = 1.00,
         [4] = 0.50,
         [5] = 0.25,
       --[6] = 0.00,
      }
      local b = SIZES[pp:ship():size()]
      if civ then
         b = (b or 0) + 0.50
      end
      if b then
         newbonus( fmt.f(_("{class} %d%%"), {class=c} ), b )
      end
      if elapsed < 40 then
         newbonus( _("Fast Clear (<40s) %d%%"), 0.25 )
      elseif elapsed > 180 then
         newbonus( _("Slow Clear (>180s) %d%%"), -0.25 )
      end
   end

   -- Implement global modifier bonuses here
   if gmods.doubledmgtaken then
      newbonus( _("Double Damage Enemies %d%%"), 0.50 )
   end
   if gmods.nohealing then
      newbonus( _("No Healing Between Waves %d%%"), 0.25 )
   end

   score = math.max( 0, math.floor(score * bonus * bonus_mul + 0.5) )

   mem.total_score = mem.total_score + score
   table.insert( str, string.format(_("TOTAL %d (#g+%d#0)"), mem.total_score, score ) )
   return str, score
end
function wave_end_msg( d )
   player.omsgAdd( d[1], d[2] )
   -- TODO add sound
end
function wave_end ()
   local round_enemies = tables.wave_round_enemies[mem.wave_subcategory][mem.wave_category]
   if mem.wave_round < #round_enemies then
      -- TODO Cooler animation or something
      local score_str = wave_compute_score()
      local n = #score_str
      local s = 1.2 -- time to display each message
      local f = (n+2)*s
      player.omsgAdd( string.format( "#p".._("WAVE %d CLEAR").."#0", mem.wave_round ), f )
      hook.safe( "clear_pilots" ) -- can't be done in the same frame as it is run on an enemy hook
      sfx_clear:play()
      for k,v in pairs(score_str) do
         local start = k*s
         hook.timer( 1.0*start, "wave_end_msg", {v, f-start} )
      end
      mem.wave_round = mem.wave_round + 1
      hook.timer( (f+1)*1.0, "wave_round_setup" )
      return
   end

   -- TODO play sound and cooler text
   player.omsgAdd( _("YOU ARE VICTORIOUS!"), 5 )
   sfx_clear:play()
   --shiplog.append( logidstr, string.format(_("You defeated a %s in one-on-one combat."), enemy_ship) )
   hook.timer( 5.0, "leave_the_ring")
end
