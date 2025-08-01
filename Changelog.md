## 0.13.0 (unreleased)

#### Gameplay
 - Removed heat mechanic (to be readded as a new mechanic in the future)
 - Made pirates less dangerous to new players
    - Marauders are now intrinsically weaker than other pirates, especially with low pirate presence
    - Made pirates less aggressive
    - Change pirate spawning to be mainly marauders in low presence areas
 - Fuel from outfits has been tweaked a bit
 - NPCs now have skill levels that can affect their targeting and fighting ability
 - Bioships have had their skills and modifiers slightly reworked to be better balanced
 - Light, medium-heavy and ultraheavy ship now have two core hull/systems/engine slots, one main and one secondary
    - Effects of heavier cores can be obtained by stacking smaller ones
 - Engines have been reworked to be more coherent and consistent
    - Tricon engines have much higher acceleration to max speed ratio
    - Melendez engines have higher speed, but low acceleration and turn
    - More consistency across lines
 - Gauntlet intrinsics are stronger and now mutually exclusive
 - Intrinsics from a certain trader are now stronger
 - Ships now disabled below a certain armour threshold regardless of stress
 - AI is less susceptible to stealth bomber cheese
 - Point defense now shoots at all ships prioritizing smaller ones after missiles
 - Cargo delivery missions have increased cargo amounts
 - Minimum stealth range also affects detection and signature
 - Ships can have a minimum cargo-less mass of 50% of the ship base mass
 - Damage over time effects now can cause faction loss and other effects
 - New harder difficulty
 - Gauntlet negative bonuses are now multiplicative
 - Stealthing will temporarily jam locked on munitions

#### Quality of Life
 - Autonav improvements
    - Can automatically land when following a ship
    - Improved autonav behaviour when losing a followed ship
    - Disables afterburner when getting ready to land or jump
    - Shooting *non-point defense* weapons resets autonav speed up
 - Combat and ambient music tries to play from where it was last stopped when applicable
 - Gave small untargetable time to player when using hypergates and wormholes
 - Ships that are scanning you use a different colour hilight
 - Landable but uninhabited space objects use a different icon
 - Deployed fighter equipment is no longer random
 - Added support for multiple automatic backups with 5 by default
 - Game will prompt if exiting the game when save data loss can occur
 - Escort mission revamp
    - Escorts now follow the player
    - Autonav will warn the player if jumping or landing will fail the mission
    - Autonav will wait for escorts when landing or taking off


#### Content
 - 13 new missions and events
    - Go treasure hunting
 - 8 new ship variants
 - 17 new outfits and reworked cores
    - Outfit sets that gain bonuses as more elements of the set are equipped
 - New sensor anomalies
 - Two new intertwined minor houses of the Empire with new systems and lore
 - More space objects with new things to explore
 - New portraits
 - Over 20 new songs
    - More variation in landing and situational music
 - Significant changes in jumps (hidden and otherwise)
 - Reworked the visuals for some weapons using shaders

#### Engine
 - Begin porting to rust
 - Using SDL3
 - Engine supports modifying some important constants such as those related to the physics model for use in plugins
 - Use instancing instead of geometry shaders for space dust
 - Ship Lua scripting supports "onshootany" now
 - Support for price_mod on tech group commodities
 - Mission and Event NPC's priority default to the priority of the parent
 - Local maps can have a range parameter now
 - Intrinsic outfits for ships are defined in \<intrinsics\> instead of \<slots\> now
 - Systems can have waypoints that are accessible from missions
 - Fixed some flickering at the end of ship trails
 - Diff mode in the editor remembers filenames and diff names
 - Slots can have tags
 - Outfits can fit an additional slot property
 - Support for hiding dynamic ship stats from outfits in the equipment support
 - Outfits support inline Lua definitions
 - Can override long class names of spobs
 - Changed how ship statistics are computed: those from same source will be additive, while different sources will be multiplicative
 - New ship stats and changes
    - Minimum Stealth Range
    - Absolute Mass
    - Absolute CPU
    - Temporary Invincibility
    - Detection Range only affects 'Detected at:' range
    - Ship Visibility that affects detection, signature, and stealth ranges
 - Engine exposes some constants that can be modified by plugins

#### Fixes
 - The Bite turns off afterburners
 - Hiding on screen display information for missions also hides system markers
 - Sensor anomalies now appear in the on-screen display
 - Fixed texture interpolation when alpha is 0 for pixels
 - Fixed adding commodities to space objects via tech groups
 - Engine glow decay is less ridiculous on ships with really weird acceleration / speed ratios
 - Only try to load missions and events with file names that end in .lua
 - Action speed can no longer be negative and disables space worthiness
 - Don't show OSD frame when not visible
 - Can't stack Sirius ferry pilgrim missions anymore
 - Add marker mission to help the player find wild space
 - Bioships will reset their locked slots when swapping ship and loading
 - jump_delay was being applied instead of jump_warmup to the engine warm up animation
 - Mass modifiers now affect outfit mass too
 - Ships will no longer appear tilted in the equipment view
 - dv_antiflf02: Make the vigilances less pathetic
 - Gauntlet properly resets outfit cooldowns
 - Saves try to save outfits by slot name to be more robust to changes in the future
 - Player can't stealth while disabled
 - Getting disabled turns off stealth
 - Carried fighters don't carry commodities
 - darkshodw: enemies shouldn't see through stealth
 - Cryogenic Nanobots no longer restore health to 100% on completing cooldown regardless of damage taken
 - Improved AI braking skills
 - Game will not save in certain cases like loading the game and immediately being forced to take off by a load hook
 - Improved handling of fullscreen toggling and resolution changes


## 0.12.6

 - Dvaered Negotiation 1: update the location of the target on the map
 - Dvaered Sabotage: no longer fail the mission if the target jumps out after boarding
 - Fixed point defense turning off all the time
 - Minor typo fixes and translation updates


## 0.12.5

 - Fixed bug causing lower diversity in ship spawning
 - Fixed autonav not respecting shield thresholds
 - Fixed AI spamming afterburners in an unhealthy way
 - Removed support for save game compression as libxml 2.14 disable compression by default
 - minerva/pirate3: fixed dialogue being weird if paying with tokens
 - Minor typo fixes and translation updates


## 0.12.4

 - More robust jump check to fix crash when creating new systems
 - Gave Pacifier +400 fuel bonus to match description
 - Made the baron comm event respect system claims
 - Shaky swan no longer gets killed by patrols
 - Fixed crash when shooting asteroids with explosive weapons
 - Disallow cooling down while jumping
 - Fixed crash with pilot.navJumpSet
 - Remove all pirate presence within 2 jumps of the starting system
 - Minor typo fixes and translation updates


## 0.12.3

 - Sirius preacher gives +4% shield regeneration, not +4 GW shield regeneration
 - Largus Gene Drive (Nyx) has a max speed of 175, not 275
 - Tweaked Melendez Mammoth XL and Eagle 6500 top speeds
 - Fixed not being able to stealth with active point defense
 - Fixed being able to complete the test of purification after failing
 - Fixed GUI getting weirdly offset in some cases
 - Cannibalize button doesn't overlap with capture anymore
 - Don't crash if loading a save where the player's current ship doesn't exist
 - Gauss has a reputation requirement
 - Derelicts are no longer space-worthy
 - Fixed Tartarus landing graphics
 - Fried now shows the stellar winds on the map
 - Reworked aggressivity on distress signals to be more consistent
 - Try to fix trails not appearing on variant ships
 - dv_goddard: The Siren of Halir no longer fails the mission after you get the commandos
 - dv_shopping: Fixed not giving the player the Fancy Key Chain
 - Misi will sell the Fancy Key Chain for those affected by the dv_shopping bug
 - oldwoman: The old woman complains again
 - shadowvigil: seiryuu doesn't disappear if you land
 - darkshadow: four wind fighters can hurt the player
 - kex3: made it completable by making it much easier to dodge and have a fairer fight
 - Fixed stealth circles not appearing on hostile pilots if their faction is friendly
 - Fixed some date-related mission information not being updated
 - Fixed landing music playing with VN music in some situations
 - Significantly buffed point defense damage
 - Story mode gives an additional -50% fuel usage modifier
 - Minor typo fixes and translation updates


## 0.12.2

 - Made plasma drill much easier to apply to asteroids
 - Don't reset cargo if not space worthy when reloading
 - Truly fix the negative mass exploit
 - Made miners less bad at mining, their only passion
 - Slim GUI set formation button is no longer clickable when invisible
 - Removed outdated tip from NPC messages
 - Round faction reputation at hypergates
 - Hypergates no longer spoil system names for you
 - Fixed description of the Squadron Synchronizer Module
 - Deployed fighters are always aggressive (for now)
 - Player can't make own escorts hostile with AoE weapons
 - Fuel usage is a negative is better stat
 - Added faction.areNeutral to get true neutral state of factions
 - Ammo can not become negative anymore
 - Made Za'lek drone weapons builtin
 - Be more restrictive on which factions give waste dump missions
 - Added the Admonisher ΩIIa for sale
 - Reputation can't go above +100 or below -100
 - Made ship buying reputation requirements more lax, using maximum of local and global reputation
 - Fix overlay map not updating after obtaining map while open
 - Stealth Discovered Speed should be Stealth Discovered Rate
 - Fixed mass being displayed twice for outfits
 - Don't have player.teleport break the game while the player is landing
 - Modified condition of many neutral missions so they don't appear in the wrong places
 - Dvaered ships are now also sold in more accessible locations
 - FLF should not cause reputation chains making other factions hostile
 - Fixed the ship_buy hook referring to the ship sold when trading ships
 - Fixed captured ships having inconsistent load outs and not spawning near the pilot
 - Fixed zebra losing cylinders when thrusting
 - Don't remove builtin weapons when spawning pilots naked
 - Fixed pilot.distress() not sending signals to spobs
 - lovebiz01: fixed condition allowing it to appear anywhere
 - dv_goddard: fixed spawning of Silent Death
 - harjas_vengeance: avoid certain system to make the mission easier to do
 - minerva missions: fixed a ship not spawning correctly
 - Large cargo hulls now match the cargo difference of the small and medium hulls
 - Minor typo fixes and translation updates


## 0.12.1

 - Fixed psychic orbs having trails
 - Crop selected spob name if it goes out of bounds in the GUI
 - Fix obelisk tests not setting properly the primary weapons of the player
 - Fleet members don't try to investigate, only leaders do
 - More lenient update checks when updating saves
 - Pilots should no longer see through stealth when attacked from stealth
 - Fixed sometimes full map being partially displayed
 - Don't show the "didn't play in a while" message every time a save is loaded
 - Difficulty modifiers extend to the player's escorts as well
 - Boarding bonus affects outfit / ship capture costs
 - Lowered the cost of capturing ships
 - Minor typo fixes and translation updates


## 0.12.0

 - Don't play wormhole sound on main menu
 - Don't allow the player to sell unique ships
 - Fixed corner cases where destroyed player ships would not properly be updated
 - Fixed Sirius Providence flow absorption bonus
 - Fixed positioning of a certain blockade
 - Fleshed out API for player.missions() to be as documentation indicates
 - Fixed some visual aberrations when forcing the player to land or jump
 - Prioritize pilots and other objects over uninhabited planets when clicking
 - Fixed bug in rehab missions causing them to get stuck
 - Trading in a ship triggers both ship_sell and ship_buy hooks
 - Disabled Nasin campaign and Defend the System missions
 - Fixed weird commodity global average values
 - Fixed how Lua environments were being stored and referenced
 - Can always bribe your way to pirate's clansworlds
 - Fixed weapon sets getting reset on game load
 - Don't remove hooks on failure, which should make rehab missions a tad more robust
 - Fixed exploit allowing the player to ignore cargo limits
 - Consistently colour the ShipAI messages in the tutorial
 - Made max_fps actually get respected
 - Fixed nebula trails on the Virtuosity
 - Bumped tracy wrap to 0.11.1
 - fw03_sirius: fixed Lua error
 - fw05_triathlon: fixed acquired information of ships the player can obtain
 - nelly02: don't have nelly tell you about disable weapons after disabling her ship
 - Carried fighters don't investigate
 - Minor typo fixes and translation updates


## 0.12.0-beta.3

 - Fixed collisions for some ships at certain angles
 - Changed mouse click selection priority, it prefers spobs/jumps and will ignore clicks reselecting the same object
 - Fixed map decorators not shown in editor
 - Centered buttons at the top of the holo-archives
 - Show if a system has a bar in the map
 - Fixed conf.mouse_doubleclick not disabling when set to 0
 - Fixed crashes when a faction is assigned a non-existent colour
 - Update the overlay scaling given the player's position
 - Fixed population not being updated by universe diffs
 - Fixed date acquired being wrong on some platforms
 - Round reputation values when displaying standing text for consistency
 - Display local standing in the information window
 - Fixed certain combination of opening menus locking up
 - Fixed fmt.number displaying wrong values when negative
 - Fixed spoiling of system names on the map
 - Improved pilot facing corrections when jumping
 - Can no longer abort missions during landing animation
 - Fixed editor crash with empty descriptions
 - Fixed editor crash when making links to systems with no presence
 - Fixed rehabilitation missions
 - Fixed escorts not using special outfits and abilities
 - Fixed escort fighters existing after undeploying escort
 - Fixed Certitude trails
 - Pirate ambush derelict events break stealth
 - Mining minigame result affects yield more
 - Absorption is no longer limited to the 0% to 100% range
 - derelict_rescue: only allow generic target space objects
 - onion04: don't fail if something happens to the gawain after taking the cargo
 - traffic_01: don't use hardcoded systems
 - bounty: fixed error with deadlines in certain cases
 - neburesearch04: don't let the drones see through stealth and disable the player from relanding to skip stage
 - minerva/judugement: fixed error in VN flow
 - Minor typo fixes and translation updates
 - Increased maxmimum reputation for a certain hidden faction to 70
 - Restored portable installs with the Windows installer
 - Workaround MESA driver bug that causes dark or invisible ships on AMD hardware
 - Documentation fixes
 - Minor typo fixes and translation updates


## 0.12.0-beta.2

 - Fallback for missing holo-archives ship entries
 - Reset proteron reputation more aggressively
 - Fixed overlap in VN logs
 - Show intrinsic outfits in the "Other" tab
 - Shooting weapons no longer resets autonav speed up
 - Fixed plural form of 0 items in many cases
 - Made pilots in formation less prone to spinning
 - Ships significantly past the mass limit are no longer space worthy
 - Lowered the price of a certain map
 - Lowered presence of a certain faction
 - Prevent changing autonav target via Lua when jumping
 - Disable jettisoning cargo when landing
 - Removed overlap in the options menu
 - Don't allow ship shield/energy to go negative
 - Don't show stealth circles during cinematics
 - Jumping through a one-way normal / hidden jump reveals the hidden side
 - Escorts will try to respect player's autonav rules by default
 - Increased nebula visibility by 400 km
 - Fixed text overlap in the holo-archives for certain languages
 - Fixed reputation changes not working on dynamic factions
 - Fixed text overlap if ship name is too long
 - Fixed automatic weapon sets behaving oddly with multiple ships
 - Fixed rare crash when setting hook on non-existent pilot
 - Fixed map thresholds for interference
 - Fixed some reputation values not being correctly rounded
 - Fixed OSD not advancing in a certain mission
 - Fixed Sirius Divinity collisions
 - Fixed beam weapons in "in range" mode not respecting range bonus
 - Fixed hiding mission OSDs and changing priority being reset on take off or jump
 - Fixed asking your ship AI for advice
 - Fixed point defense requiring toggling thrice to turn off in some cases
 - sensor anomaly: Nelly will no longer appear in too dangerous or risky systems
 - mephisto type v: lowered heat generation
 - seek_n_destroy: fixed hints breaking in certain cases
 - pir_hit_intro: fixed OSD being displayed wrong
 - taiomi04: update OSD after boarding ship
 - bounty: fixed OSD getting reset after killing/capturing target
 - ant_supplies: fixed hostility after jumping into system again
 - Editor now shows dialogues on errors and allows user to choose directory to save data to
 - Minor typo fixes and translation updates


## 0.12.0-beta.1

#### Gameplay
 - Ability to capture disabled ships
 - Faction reputation is no longer universal, but varies on systems
    - Direct faction hit changes are shown as messages
    - Your actions will more directly affect ships in the system
 - Significantly increase maximum potential fleet capacity
 - Excess energy to battery recharge efficiency is now a flat instead of varying based on charge level
 - Gave the Za'lek sting a slight detection buff
 - Can only have a single patrol mission active at a given time
 - Pirates are less numerous during Chapter 0
 - Dvaered Arsenal has no fighter bays and fewer slots
 - Lowered CPU cost of some fighter bays
 - Decreased CPU and energy provided by large core slots
 - Pillaging ships affects reputation
 - Significantly increased the amount of fuel provided by outfits
 - Camouflage burster gives a speed bonus while active and disables when out of stealth
 - Can scan ships with 'u' key
 - Gave the Quicksilver another medium structural slot
 - Beam weapons have minimum delays instead of minimum durations
 - Can stealth when missiles are locked on
 - Deployed fighters take stress and damage over time when their mother ship is destroyed
 - Electron Burst Cannons and Za'lek Heavy Drones have had their damage per second lowered
 - Improved point defense
    - Spittle Tubuloid Cluster is now a utility and does more damage
    - Guardian Interception System does double damage
    - Missiles explode when intercepted
 - Bounties have explicit time limits
 - Derelicts should appear in remote systems

#### Quality of Life
 - Added colourblind correction mode
 - Exposed more colourblind options to the user
 - Can modify game speed directly instead of using a slow mode difficulty for accessibility
 - Messages get folded instead of repeating
 - Asteroids no longer fade out if close to or targeted by the player
 - Display fuel consumption for ships in the equipment overlay
 - Added option to match speed with the slowest ship in the fleet (on by default)
 - Made it so ships in the equipment screen do not change order based on value
 - Mark spaceport bar tab when there is an important NPC
 - Weapon set keybindings change window tabs
 - Use short outfit names when displaying groups of outfits
 - Shown more useful things you have when buying outfits with things other than credits
 - Can sort and filter the mission computer
 - Added small optional bounce to NPCs when they start talking
 - Inform the player when they don't meet mission requirements for some important missions
 - Can add new plugins directly from the options menu
 - Can toggle whether autonav stops when missile lock-ons are detected
 - Added colour coded factional backgrounds in the shipyard
 - Can rotate shipyard image for ships with 3D graphics
 - Prompt when deleting notes
 - Added accessibility option that allows skipping story minigames
 - Can view and change all your ships in the equipment tab when there is refuelling
 - Reduced space dust size by 50%

#### Content
 - New in-game wiki with explanation on gameplay mechanics and lots of lore
 - 8 new missions
    - The Onions call
 - 4 new ships
 - 13 new ship variants
    - Give a new twist to existing ships
 - 6 new outfits
 - New area with unique challenges
 - New events
    - Get more Sirius abilities
    - Find the dark side of the Nebula
 - New NPC portraits
 - New generic NPC and News messages
 - Reworked trails to make ships feel more speedy

#### Engine
 - Support for 3D models
    - Lighting is based on system stars
    - Fancy effects for special systems
    - Ships tilt slightly when rotating
    - Simple animations are supported and used when applicable
 - Support for an in-game wiki
    - Can process YAML, Lua, and markdown
    - Cross-link support
    - Support for custom widgets
 - Stats (except inverted stats) are now additive instead of multiplicative
 - Try to merge saves if multiple directories correspond to the same player
 - More robust weapon set support that can handle multiple overlapping weapon sets
 - Faster handling of asteroids with large exclusion areas
 - Added fuel_usage_mod ship stat
 - Fixed player losing navigation targets when unidiff is applied
 - Fixed ai.idir giving wrong answer by M_PI_2 in some cases
 - Threaded more loading components
 - Lua require now caches chunks
 - Dropped SDL_image fallback
 - Use higher internal timer for all platforms
 - Support for Tracy
 - Spobs that are not landable yet generate presence will be shown on the map
 - Warn the player if they try to use a name with all space characters
 - Decoupled the collision system from the rendering system
 - Added a low memory mode to use fewer and smaller textures with 3D
 - Lazy load ship and outfit graphics to reduce memory usage and decrease load time
 - Use clang-format to format all the C code
 - Use enums internally to represent keybindings instead of strings
 - Always use replaygain information when available when loading vorbis files
 - Stop repeating the same warning after a fixed amount of times
 - Support for outfits that don't break stealth
 - Editor has partial support for universe diffs
 - Editor has separate configuration and file dialogues
 - New ship stats
    - Global weapon range, damage, fire-rate and energy usage
    - Range ship stat for all specific weapon types
    - Turn, speed, and accel for launchers
    - Shield downtime modifier
 - Deprecated old faction API in favour of the new one for local / global standings
 - Added support for temporarily overriding faction standings

#### Fixes
 - PSO and Mizar nebulas have trails again
 - Removed fancy background mode
 - Can remove fighter bays with deployed fighters
 - Sol is less friendly
 - Fixed rare cases where fighters would not be able to dock
 - Fixed hypergate and wormhole effects getting stuck
 - Obelisks tell if you have completed them already
 - Fixed space dust disappearing with certain jump abilities
 - Boarded pilots become permanently disabled if they lose space-worthiness
 - Double tap will activate feather drive like blink engines
 - Fixed camouflage burster not activating stealth
 - Fixed bug where beams were counted as giving energy in the equipment optimizer
 - shadowvigil: claim entire route
 - `sh01_corvette`: should be completable on easy difficulties now
 - patrol: fixed escorts and fighters becoming hostile if hostile to the mission giving faction
 - Fixed debugging paranoid builds under Windows
 - Fixed autonav not respecting shield thresholds
 - zbh10: Godheart and friends get at most 1 fighter bay per ship now
 - Fixed ESS Trinity being bribeable
 - Cargo on fleets with be displayed to make room for mission cargo when necessary
 - Made it so Shadow Vigil doesn't try to go through Surano system for reasons
 - `blackhole/zbh09`: fixed softlock during a cutscene under certain conditions


## 0.11.5

 - Fixed FLF-Pirate map not providing jump information
 - Fixed the combat hologram projector making escorts hostile
 - Build system no longer libdl on platforms such as BSD


## 0.11.4

 - More guards against divides by zero in autonav
 - Fixed warnings on certain OpenGL drivers
 - Fixed toolkit not rerendering when right-clicking on tabbed windows
 - Fixed Marius enclave description
 - Tweaked Za'lek Diablo and Mephisto stats so that they are better suited to their respective roles
 - Updated flicker drive, blink drive, and hyperbolic blink engine descriptions to be more complete and correct
 - Initialize outfit Lua scripts when added in the equipment view so that flicker drive signature gets properly computed
 - Flicker drive no longer displays -10% signature range bonus twice when equipped
 - Fixed crash when selling deployed ship
 - Gave the Dvaered warlord event better criteria so they don't attack the hypergate
 - `shark/sh01_corvette`: have the pilot jump in from the same system as the player
 - `minerva/kex03`: have mission claim the crimson gauntlet to prevent unwanted spawns
 - `dvaered/gauntlet`: have mission claim the system if it can, but not fail if it can't
 - `trader/trader_escort`: player can only escort one group of traders at a time
 - `neburesearch/neburesearch_01`: use the correct faction when complaining
 - Fixed some typos and revised writing
 - Translation updates


## 0.11.3

 - Don't run discovery event when in cinematic mode
 - Do a better job of updating old save autonav settings
 - Added more checks so tutorial messages don't appear in claimed systems
 - Fixed blinking not breaking stealth
 - Fixed auto-hail message colouring
 - Only do updates with positive delta ticks
 - Fixed cargo disappearing when buying a ship when over the cargo limit
 - Don't let the player trade ships when they have mission cargo
 - Fixed not being able to turn off point defense weapons
 - Fixed Za'lek drone bay being cheaper than the mini-bay
 - Flicker drives can not be stacked and are mutually exclusive with other blink drives
 - Decreased Nexus Drill Lance mining malus from -80% to -15%
 - Increased all mining yields by roughly 5 times
 - `sciencegonewrong/02_sciwrong`: make the drones not hostile to other factions
 - `neutral/baron_comm`: fixed trivial warning when trying to clean up baron comm event
 - `neutral/wastedump`: fixed getting rid of cargo while landed
 - Translation updates


## 0.11.2

 - Stopped autonav from preventing wobble and overshooting by crashing the entire game


## 0.11.1

 - Give all ammo back to player after doing obelisk
 - Fixed player being invincible after completing an obelisk
 - Fixed a crash when loading games while a landing hook was running
 - Fixed race condition in threadpool
 - Fixed asteroids spawning in not proper shapes
 - Fixed autonav wobble and overshooting
 - Point defense won't shoot at disabled pilots anymore
 - Fixed battery descriptions
 - Discovering a hidden jump will make both directions known
 - Fixed AI trying to scan hostile targets they lost track of
 - Properly save and load reward_value to and from saves
 - Fixed events and missions being able to trigger in obelisk tests
 - Fixed beam weapons not hitting asteroids
 - Properly compute weapon range with launch_range modifiers
 - Fixed some range checks with inrange weapon sets
 - Fixed Empire Pacifier mass being too low
 - Fixed warning when looking at internal flow amplifier descriptions
 - Removed Soromid Ira turret and forward weapon stats
 - Fixed reality rip and avatar of sirichana abilities giving errors when the AI tries to use them
 - Fixed issue on some systems with indexed images
 - Fixed pirates and pilots that don't care being able to disable the fake transponder
 - Can now see and target allies that are stealthed
 - Pheromone emitter won't do anything in exclusively claimed systems
 - Fixed some weapons such as beams not hitting targets other than selected one
 - Fixed rendering order making it so most effects were below the player
 - Fixed fallback switch weapon sets not being found properly
 - Fixed bioship "Wanderer" perk giving absolute accel bonus instead of relative
 - Increased Neural Accelerator Interface energy regen malus to -100%
 - `zalek/blackhole/zbh09`: don't error out when a bioship ceases to exist
 - `kidnapped/traffic_00`: fixed formatting string telling the player what system to go to
 - `kidnapped/traffic_01`: ship should spawn if taking off in the same system, not just jumping in
 - `tutorial/nelly01`: fixed derelict message not appearing
 - `minerva/pirate4`: fixed save me spam only being print once
 - `neutral/seek_n_destroy`: fixed warnings when taking off in the same system
 - `sirius/achack/achack01`: fixed not being able to accept mission
 - `dvaered/dv_diversion`: fixed not being able to accept mission
 - `dvaered/dv_bikers`: don't allow the player to use escorts and properly update mission TODO
 - `sirius/achack03`: fixed not being able to complete the mission
 - `neutral/kidnapped`: fixed inconsistency in the name of the system to go to
 - `shadow/shadowrun`: fixed VN issues not allowing mission completion
 - `shadow/shadowrun`: changed locations to make it possible to do in the allotted time-frame
 - `shadow/shadowvigil`: fixed Seiryuu not appearing
 - `shiplover`: don't ask the player about ships they can't obtain
 - Fixed many typos
 - Translation updates


## 0.11.0

 - Fixed beams only showing hit explosions on one target being hit instead of all
 - Consider beam width when computing collisions
 - Made beam effects a bit thinner
 - Point defense weapons should track fighters much better now
 - Fixed point defense weapons not firing properly
 - Approach no longer instantly starts autonav unless a space object or derelict is already targeted
 - Tweaked the approach logic to prefer planets over pilots when only planet is selected
 - Approach should no longer play extraneous target sounds
 - Stores now show amount for unique outfits, while equipment windows shall not
 - Fixed POI missions having markers say 'Point of Interest' instead of 'Sensor Anomaly'
 - Most consistent texture interpolation when using mipmaps
 - Pilots will do their first distress somewhat sooner
 - Can only start to afterburn with enough energy for 0.5 seconds of afterburning
 - Updated meson.build to require SDL 2.0.26 which seems to be minimum supported version now
 - Fixed a certain faction being enemies with wild ones when they shouldn't necessarily be
 - Soromid Arx now has one non-exclusive heavy non-bio slot
 - Buffed Scanning Combat AI to have an additional +15% tracking
 - Pirates should no longer have presence in Sol under certain conditions
 - Buffed weapon ionizer to only -50% damage from -70% damage
 - Made weapon sets more robust to changes
 - Fixed some pirate checks in dominated systems
 - Clear weapon sets when doing active cooldown
 - Run ship / outfit code Lua code when disabled or cooling down
 - Increased maximum standing cap with pirates
 - Allow marauders to become friendly
 - Fixed gamma correction / colourblind shaders not being run on top of everything
 - Fixed damage sometimes causing pilots to instantly become undisabled
 - Fixed Sirius ships acquired before 0.11.0 not being able to use flow
 - Fixed Cleansing Flames shader
 - Gave some flow abilities custom sound effects
 - `srs_ferry`: fixed marker not updating for alternative delivery locations
 - `nelly01`: fixed giving the player twice as much money as needed for an ion cannon
 - `nelly03`: guard against the player changing systems in the middle of mining
 - `zbh06`: made mission easier for the player
 - `seek_n_destroy`: fixed mission being failable after the player takes out the target
 - `preach`: don't try to claim the system twice
 - Fixed many typos
 - Translation updates


## 0.11.0-beta.3

 - Differentiated more significantly the energy / kinetic / plasma weapon types
 - Fixed hard crash when loading when approaching certain NPCs
 - Fixed crash when change tab triggers a takeoff
 - Fixed Naev not starting on Mac OS
 - Fixed beam collisions (again)
 - Fixed rare hard crash when beams are being fired
 - 'in range' option for weapon sets now takes into account weapon arcs
 - Buffed Targeting Conduit
 - Renamed 'Point of Interest' to 'Sensor Anomaly'
 - Double-tap activating outfits breaks stealth
 - Nerfed Sirius Fidelity so it can no longer get ludicrous levels of action speed
 - Map's discovery mode shows system features
 - Removed enemies from a certain hidden faction so the player won't have standing issues with them
 - Non-weapon outfits no longer show switch groups they are in as hot keys
 - Turn off weapon sets when changing type
 - Fixed toggle weapon sets not triggering outfits over and over as expected
 - Turn off all weapon sets when entering stealth
 - Fixed resizing not working on the background and toolkit in some cases
 - Added pilot.weapsetAddType and pilot.weapsetSetActive
 - Gave unicorp storm launchers a rarity of 2 and made it more available
 - Have ships be a little less spammy distressing
 - Centered tracking icon in slim GUI
 - Show tracking icons for non-turret bolt weapons too
 - More short names for weapon outfits
 - Fixed pilot.setSpeedLimit not working
 - Added tutorial message when player acquires first point defense weapon
 - `test_of_renewal`: fixed weapon set defaults being incorrect and increased enemy damage
 - `seek_n_destroy`: missions should work properly when boarding the target
 - `flf_diversion`, `flf_rogue`: Missions should no longer be able to have ridiculously low credit rewards
 - `ec00`: changed it so you can't hail the collective drone
 - `escort`: fixed pilots not flying in formation
 - Fixed many typos
 - Translation updates


## 0.11.0-beta.2

 - Slim GUI now shows activated outfits and all weapons all the time
 - Collision system reworked (again) to take into account fast moving particles
 - Fixed collision polygons not properly being used
 - Added short names to some outfits such that they are easier to distinguish in the GUI
 - Allied factions won't help out the player against neutral targets
 - Added missing graphics for meditation chambers
 - Don't allow giving the player names that can't be saved
 - Updated love.filesystem to 0.11 spec renaming mkdir to createDirectory, enumerate to getDirectoryItems, and adding remove
 - Fixed toggle weapon sets not turning off with only bolt weapons
 - Hardened physics engine a bit to overflow that happens in 49 days of straight game time
 - Fixed trivial memory leak in vpool
 - Allow buying local maps where they are not sold
 - Buffed Hunting Combat AI to 15% bonus
 - Made Weakness Harmonizer AI not appear as an active outfit
 - Outfit Lua function onshoot has been renamed onshootany
 - Can toggle point defense weapons on and off
 - AI will turn off weapons ionizer when going for a kill
 - Player's new ships should start will all the default outfits, which fixes the some Sirius psychic tests
 - pilot.weapsetAdd follows the same logic as the normal equipping functions
 - slim GUI uses primary/secondary colours like the info menu
 - AI should be less prone to jump before their leader
 - Fixed cargo being lost when swapping ships in missions and events
 - `chapter1`: event claims all the systems just in case
 - `poi`: renamed 'Pristine Derelict' to 'Unusual Derelict'
 - `poi_intro`: fixed typo
 - `achack03`: fixed mission not being acceptable and harja spawning forever
 - `achack04`: fixed missing formatting string
 - `dv_bikers`: made missiles significantly more dodgeable and changed location
 - Translation updates


## 0.11.0 (beta)

#### Gameplay Changes
 - Universe significantly overworked to be more consistent with lore
    - More landable uninhabited spobs, unique locations, and things to discover
    - Reviewed and corrected many descriptions and placement of spobs and systems
    - More in-depth and fleshed out tag system for locations
 - Ship slots and mass limits reworked such that smaller ships get more utility/structural slots to work with
 - Point defense systems that can shoot down missiles and torpedoes
 - Space object (planets, stations, etc.) properties affect quantity of missions available
 - Moved many missions and events to the vn system
 - Can sometimes find POI events with a pulse scanner equipped when entering systems
 - Changed the visuals for some of the nebulas (PSO, Mizar)
 - Pirate bribes cost more depending on your fleet and are based on points not mass
 - Missions are introduced less all at once to the player
 - Blink drives are more flexible but use energy and generate heat
 - Razor class weapons have been completely reworked
 - Can steal more than one outfit from a ship with high boarding bonus
 - diy-nerds: improved reward

#### Quality of Life
 - Significantly improved autonav
    - Configurable and can use lanes
    - More efficient at reaching target locations
    - New option like follow pilots through jumps or brake when going to positions
 - Make it explicit when all the escorts have jumped or landed
 - Escorts will keep their same loadout until the game restarts
 - Player ship is no longer translucent when in stealth as it is redundant with the stealth icon
 - Show enemy/ally factions in the faction standing info window
 - Space dust is properly anti-aliased
 - Minor speed ups to patrol lane computations
 - Try to enforce minimum number of articles in the news
 - Independent patrol and bounty missions can be completed on more planets and stations
 - Can hide or prioritize missions from the info menu
 - Manual aiming model aims at the mouse location when the mouse is visible
 - Travelling merchant tells you when new wares are available and should be easier to find
 - Lua Love API should be better at handling input and not apply keys held before started
 - Increase time compression when disabled
 - Inverted how hide, evasion, and stealth value percent bonuses work. Now lower is better
 - Hide locked slots without outfits as the player can't do anything with them
 - Added volley mode to weapon sets that makes weapons fire as fast as possible, instead of staggered
 - Autonav options are now player-specific and accessible via the info menu settings button
 - Travelling merchant gives full details of the intrinsic outfits they provide
 - Selected slots in the equipment window will only show outfits that fit
 - Autonav routes consider distance travelled in-system
 - Made stress more visible in the slim GUI
 - Reworked how stats are displayed to be more visible and intuitive
 - Can show all known outfits or ships in with the map find functionality
 - Weapon sets much more flexible, verbose, and easy to use
 - Weapon sets remember slots, not outfits
 - Route is visible on all map modes now
 - Blink and flicker drives can use double tap arrow keys to move around
 - Toolkit is cached in a framebuffer for much faster rendering
 - Intrinsic outfit details now visible from the equipment menu
 - Visually indicate which pilots are scanning the player on the overlay and radars
 - Can sell all outfits on any spob with an outfitter
 - Enemies in patrol missions should not run away
 - Changed the faction standing caps to allow the player to get all ships when maxed out. Will be decreased in the future as missions are added
 - Slot icons to make it more clear what special slots a ship has
 - System markers and autonav TARGET marker will try to not overlap with jumps and spob names

#### New Content
 - New mechanic for House Sirius called flow to unlock psychic powers
    - Gives passive bonuses to Sirius ships
    - Outfits allow use on non-Sirius ships
 - 8 new missions
    - Finish the Minerva campaign
    - Nebula refugees
 - Many new events
    - Abandoned stations with secrets
    - Greedy pirates looking for domination
    - Challenges of the mind
    - Mysterious signals
    - More points of interest
 - Many, many new systems and space objects
    - 67 new star systems with 123 new space objects
    - New graphics for space objects
 - Tons of new outfits
    - Sirius flow outfits
    - Completely reworked Sirius weapons too
    - Point defense systems
    - New accessories
    - Intrinsic outfits
    - and more!
 - Many new outfit graphics
 - New NPC graphics
 - More NPC and news messages
 - Added the Space Trader Society faction
 - Custom death animations for many ships
 - More ways to increase fleet capacity
 - The pirate clans are now more differentiated in terms of AI behaviour and taunts
 - More factional landing messages
 - Rehabilitation missions have been made more factional and a new rehabilitation mission for the FLF is now available

#### Engine
 - Map system viewer is more compact
 - Added hook.hail_spob
 - Events support tags
 - Editor supports tags
 - Library to handle conditionals for mission computer missions
 - Added support for disabling specific patrol lanes from being generated
 - Support for Lua scripting for ships
 - Changed api of evt.claim and misn.claim
 - Missions/events load Lua as chunks instead of compiling each time
 - Significant speed-ups in collision detection with quadtrees
 - Hooks "outfit_buy", "outfit_sell", "ship_sell", and "ship_swap" pass Lua objects instead of strings
 - Ships can have extra descriptions that show up on mouse over
 - More Lua API added such as pilot.armour, pilot.shield, or naev.missionList
 - Soromid NPCs can have custom descriptions based on genetics
 - Improved VN API with vn.move, vn.musicVolume, etc.
 - Improved VN handling of non-ascii fonts
 - Support for generating munitions from outfits
 - Improved derelict script to handle custom derelicts better
 - Removed some custom environment and string handling functions for standard SDL ones (requires 2.0.18 now)
 - Better handling of user locales
 - Weaker effects shouldn't overwrite stronger ones anymore
 - Spobs can use communication graphics
 - Require OpenGL 3.2 for geometry shaders now
 - Support for advanced collisions such as weapon on weapon, allowing for point defense weapons
 - Renamed thrust to accel for more consistency and simplifications
 - Support for buying intrinsic outfits
 - Ship distress moved to the message framework
 - Outfits have support for double tapping accel/left/right triggers
 - Removed toolkit fading effects
 - Support for rendering images as SDFs
 - Significant loading time speed-up with multithreading

#### Fixes
 - Main menu more responsive when changing windows
 - Typo and wording fixes
 - Fixed many corner case crashes in the editor
 - Fixed cargo missions not being generated in some parts of the universe
 - Game no longer crashes when loading save with persisted Lua pointing to nonexistent systems/spobs
 - Minor improvements to many existing missions
 - Fixed crash when events trigger other events on creation
 - Fixed autonav sometimes having trouble landing with reverse thrusters
 - Qex races should be much less laggy now
 - Fixed some outfits using the wrong store images
 - Fixed system viewer not being consistent with map
 - Player's escort damage is counted towards players damage
 - Avoid having missions duplicates for cases where they can significantly stack such as patrol missions
 - Fire rate and action speed should affect damage and disable of beam weapons
 - Fixed some outfits not having "Activated Outfit" in their description
 - Fixed ships offering 100 fuel refuels twice
 - Fixed some ships having trouble equipping because of stacking engine reroutes and such
 - Improved AI's scanning behaviour to be more robust to stealth pulsing
 - Fixed POI generating in extremely volatile systems
 - Fixed tutorial running during cinematics
 - Fixed active outfits not showing "activated outfit" in their summary sometimes
 - FLF no longer become true allies of the Dreamer Clan to not limit the players actions
 - Made audio system more robust to running out of source errors


## 0.10.6

#### Fixed potential segfault with invalid semver strings
 - Fixed sign error when buying artifacts in the Baron Prince mission (sorry)
 - Fixed behaviour of naev.trigger with parameters
 - Fixed Lua spfx volume changing with game speed
 - Fixed memory leak in luaspfx trails
 - Fixed missions doing things in systems they should not
 - Fixed Adrenal Gland III's time speed up effect
 - Fixed Misi giving upgrades for free
 - Fixed minor planet check in frontier war missions
 - Fixed silent installs on windows
 - Fixed typos


## 0.10.5

 - Start counting effect stacks from 1 not 2
 - Fixed launcher weapons using outfit mass instead of ammo mass
 - taiomi: fixed claim check for last mission
 - Fixed some typos
 - Fixed equipment of Lancelot in "Sharkman Is Back" mission
 - Fixed clicking on jump points also selecting planets in some cases
 - nelly02: Nelly now stops recommending stuff that only does a little disable
 - shadowvigil: Fixed mission not spawning escorts
 - flf_patrol: Missions should no longer be able to have ridiculously low credit rewards
 - Fixed some events not claiming systems that could interfere with other missions
 - hypergate_construction: should actually claim the system
 - Fixed some formatting in the alt text when hovering over outfits in the equipment window
 - ec06: refuel tanker should provide as much fuel as possible
 - taiomi09: fixed smugglers becoming hostile in some cases
 - Changed music.stop() API to stop music from continuing by default
 - Fixed potential memory issues on some platforms
 - Fixed crash when changing to Japanese language on Windows using Japanese locale


## 0.10.4

 - Fixed crashes related to multiple effects being active at once
 - Fixed multiple mission_done hooks not passing parameter correctly
 - Fixed plugin strings not being initialized with mismatched saves
 - ec06: made the final battle work much better


## 0.10.3

 - Fixed crash when using fits currently selected ship outfit filter
 - Fixed escorts always being set to aggressive when loading a save
 - Fixed Dvaered standing cap increase being lost after loading game
 - Made space dust a bit less bright when it starts turning into lines
 - derelict_rescue: play money sound instead of victory sound on completion
 - rehab: fixed crash when aborting rehabilitation missions
 - foundation station, efferey: fixed not using pirate landing script


## 0.10.2

 - Fixed escorts sometimes not following their leader and landing
 - Fixed autonav always wanting to go to the edge of jump points
 - Fixed crash when causing multiple dialogues to run in the background
 - Fixed outfit lua being called before initialization
 - Fixed not giving the AI a name when updating a save breaking the updater script
 - Fixed deleting last snapshot of a save switching to another pilot's saves
 - Fixed saves and snapshots not displaying correct name with version mismatch
 - Fixed crash when deleting Lua-side fonts, should fix crash with POI
 - Fixed swapping ships with mission cargo sharing name with other cargo can lead to wrong cargo getting duplicated
 - Fixed original music at Research Post Sigma-13
 - Fixed music stopping after playing once in new games
 - Change music API to make it explicit you can temporarily disable the music engine
 - Set windows compatibility mode to Windows 7 when cross-compiling
 - legacy gui: fixed line artefacts near fuel / energy bars
 - pulse_scanner: fix potential error on init
 - patrol: don't have an invisible time limit to reach the system anymore
 - taiomi: fixed some claims
 - zbh03: landing when hostiles spawned will fail the mission
 - bounties: mention there is a time limit to reach the system, not made explicit though
 - zpp01, zbh01: bumped chance to 30%
 - poi_intro: can't board nelly again


## 0.10.1

 - Fixed many stations not marked as stations
 - FLF combatants only appear on FLF spobs
 - Fixed cases where the player could be forced to take off when not spaceworthy
 - Show engine volume option same as other volume options
 - Use nearest neighbour interpolation for small resolution vn images
 - Fix engine sound being played at high time compression values
 - Try to fix issue where music stops playing
 - Statically link libenet on steam versions
 - diy-nerds: fixed reward and description not matching
 - deliverlove: fixed credit exploit
 - reynir: don't add 0 tonnes of hotdogs


## 0.10.0

 - Made slim the default GUI instead of brushed
 - Contraband missions use vntk instead of tk
 - Sightseeing missions mention explicitly how much you get paid when completed
 - Removed outfit name duplication in alt texts
 - Made outfit descriptions fit in the landing window for all outfits
 - Added sound effects to starting race
 - Fixed non-Lua active outfits (jammers, etc.)
 - Sort ship stats in outfit/ship descriptions
 - Fixed jammers and jamming
 - Buffed jamers and scramblers
 - Buffed evasion bonus of Red Star hulls to compensate for detection malus
 - Fix "Dead or Alive" and "Alive" being swapped in bounty mission descriptions
 - Lowered error in physics approximation (shouldn't be noticeable though)
 - Added particle beam and particle lance to Za'lek outfitters
 - Fixed escort ai not being properly applied to newly spawned escorts
 - Improved rendering of the map system information window
 - Improved minor artefacts in slim and slimv2 GUIs with scaling enabled
 - Engine sounds smoothly transition on/off
 - Fixed other minor issues


## 0.10.0-beta.2

 - Pirates should avoid attacking near safeish areas
 - Fixed crash on load when player has more than one ship
 - Fixed player.setSpeed() not resetting speed as intended
 - Fixed pilot.comm not showing messages
 - Fixed typo/grammar in sightseeing and dvaered census introductory mission
 - Don't display health bars with no player alive
 - Autonav doesn't go only to the center of spobs
 - Mention escort AI settings when buying a fighter bay tutorial plays


## 0.10.0 (beta)

#### New Mechanics
 - Support for setting ships as escorts and thus player fleets
 - Hypergates that allow for long distance travel
 - Asteroid rework
    - Asteroids no longer randomly explode
    - Asteroid scanning is no longer binary, but distance-based
    - New mini-game based mining
    - More diversity in types with different rarity
 - Support for restrictions for outfits and ships, such as minimum faction standing
 - Significantly improved how faction reputation caps are handled
 - Pilots can have intrinsic outfits
 - Bioships go rawr (new skill system)
 - Weapons and outfits can cause effects on ships
 - More complex space objects (spob)
 - Support for different difficulty settings
 - New exploration mechanic with points of interest
 - Unique ships to be found throughout the universe (pers)
 - Manual aiming mode for weapon sets

#### Gameplay Changes
 - Weapon types are more differentiated
 - Removed nearly redundant launchers
 - Nerfed beams
 - Factional ships are more widely sold
 - Schroedinger uses less fuel instead of getting a large bonus
 - Plasma has a burning effect
 - Changed spawning and behaviour of pirates to be less dangerous in populated systems
 - Fighters only attack enemies visible by their carrier
 - AI is better at choosing targets
 - More dump targets for waste dump mission and tweaked rewards
 - Removed escorts for hire in lieu of player fleets

#### New Content
 - Added a gigantic black hole
 - Added new space anomalies such as plasma storms
 - Lots of new asteroid types and commodities
 - More interesting places to visit and explore
 - More engine sounds
 - 45 New missions
    - More terraforming
    - Continuation of the Za'lek story
    - Help the Dvaered do some tasks
    - Tutorial for new mechanics
    - Secret system!
 - 14 New ships
    - Certain secret faction completely revamped
 - New spob graphics
 - Lots of new outfits and reworked old outfits
    - Use energy to avoid death
    - Create scanning pulses
    - Blinking has animations
    - Advanced mining techniques
    - Space mines
 - News revamped to be more flexible and relate more with current events
 - NPCs revamped to be more flexible with many new messages
 - New commodities that are only available from mining
 - Fancy racing mini-game that replaces old race missions

#### Quality of Life
 - Support for save snapshots for each pilot
 - Can ask pilots to refuel you more than 100 units at a time
 - Engine sound volume is configurable
 - Revamped the star map to be large and more useful
 - Can add notes to the star map
 - Autoscroll is now an option in the VN
 - Spob communication window has been redone
 - Limit sound output volume when lots of sounds are playing jointly
 - Redid the music engine to be less prone to play combat music
 - Hide radar when overlay is open (with option to revert to old behaviour)
 - Duplicate effects get collapsed into stacks in the GUI
 - Allowing exiting and reloading while love framework is open (VN, etc.)
 - Autonav is more flexible with positioning on jump points
 - Can customize jump flash brightness
 - Player will not be scanned immediately on jumping in or taking off
 - Show health bars near pilots in combat (can be disabled in options)
 - Escort AI is customizable
 - Ship AI reminds player about things when they haven't played in a while

#### Engine changes
 - Work has begun on a development manual
 - Plugin support with explicit support for total conversions
 - All monolithic files have been split up
 - Simplified terminology with spob (space objects) replacing planet/asset
 - Support for tags in missions
 - Player ships can store their own variables now
 - Weapon outfits support some Lua scripting
 - Ammunition/fighters merged into launchers/fighter bays
 - Support for "shotgun"-type weapons
 - Backgrounds no longer use an orthographic projection
 - Minor transitions added to the toolkit
 - Asteroids redone to be more flexible and easy to add using groups
 - Asteroid field support in the editor
 - Allow for soft claims instead of only hard claims
 - Unified the event and mission headers
 - Implemented per-pilot variables
 - Lua scripting for spob
 - AI can use special outfits
 - Lots of engine rewriting and modernization that should bring some speed improvements and more flexibility when modding
 - Added lua-enet library to allow for networking in plugins (off by default, requires setting in configuration file)
 - Can animate loading screen

#### Bug Fixes
 - Too many to list, but we'll try
 - Fixed pilots not getting equipped at all in some cases
 - Fixed looting cargo when boarding giving less than expected
 - Fixed all asteroid graphics being used as debris
 - Fixed some hooks not properly passing arguments
 - Block certain inputs during cinematics
 - Fixed disabling saving and forcing the pilot to take off not working as expected
 - Fixed sounds getting stopped in many cases due to garbage collection
 - Many typo fixes


## 0.9.4

 - Fix "No error." log spam with certain video card drivers
 - Fix Lua errors with tiny nebula such as Sarcophagus
 - Fix fake transponder cheesing rehabilitation missions
 - Fix errors in "Anxious Merchant", "Dead Or Alive Bounty", "Harja's Vengeance", and "The Lost Brother" missions
 - Fix the in-game screenshot feature, in case of odd window dimensions
 - Fix at least "Dvaered Diplomacy" glitching when the game is saved/reloaded (thanks to "Duke" on the Steam forums)
 - Update translations, including a new Spanish translation


## 0.9.3

 - Bug fix: if the German translation was active, casino minigames' explanation (Erklärung) didn't work
 - Fix errors/slowdown in Diversion from (...) missions
 - Fix bug in "Waste Collector" mission
 - Fixed a bug that allowed the player to get infinite escorts
 - Work around bugs in at least one OpenGL driver
 - Fix crash when unidiff changes assets that the player has targetted
 - Player actually has to pay for stealing outfits
 - Fixed game hanging when entering some volatile nebula systems


## 0.9.2

 - Fix reward messages in the Particle Physics campaign
 - Can no longer steal a certain Soromid ship
 - Enhanced the logic for deciding whether it's safe to save the game after landing
 - Fix mission bugs: "Assault on Unicorn", "Emergency of Immediate Inspiration", "The Search for Cynthia"
 - Fix zombie autonav toward deselected targets


## 0.9.1

 - Minor countermeasures for long player ship names
 - Fix mission breakage in "Minerva Pirates 4", "Runaway Search", "Particle Physics 3", "Shadow Vigil", "Baron Prince", and "Dvaered Ballet"
 - Fix exploit in "Travelling Merchant" event (mission prize for sale that shouldn't have been)
 - Fix many missions that explicitly attack the player overriding stealth and visibility mechanics
 - Fix some text labels that couldn't be translated from English
 - Fix equipment slot information displaying over filter widget
 - Fix phantom acceleration after an auto-board and undock sequence
 - Darkened nebulas and lowered default background darkness
 - Improved upstream metainfo for packagers
 - Can no longer steal a certain Za'lek ship
 - Fix crash under certain conditions when using the console
 - Masochists and LTS distro packagers may build with Meson 0.54 (no subproject fallbacks) or 0.53 (also no "meson compile", only "ninja")
 - Slightly reduced rendered nebula quality to stop breakage on some intel GPUs
 - VN music uses logarithmic scale like internal music
 - Fixed some offset issues with the slim GUI


## 0.9.0

 - Fixed glitchy appearance of the map's mode menu
 - Map mode is remembered throughout the gaming session
 - Music transitions better for impatient players
 - Tighten up alt-text
 - Don't show aiming helper in cinematic mode
 - A busy volunteer proofreader kept editing almost as quickly as we could add errores
 - Some more outfit graphics
 - Pilots should be a bit less trigger happy when jumping in
 - Fixed minor visual artefacts with pirate trails
 - Centered the bottom bar (Brushed GUI)
 - FPS and Time Compression factors are monospaced when displayed
 - Added option for disabling resizing of window
 - Stealthed pilots don't affect autonav
 - Meow meow


## 0.9.0-beta.3

 - Fixed warning about cargo rush deliveries when you don't know the best route
 - Fixed another crash related to pilot removal
 - Fixed wonky backgrounds during death cutscenes, for the sake of *other* players of course
 - Fixed crash when techs are first patched to planets through unidiff
 - Fixed potential spurious warnings about incomplete translations, even when running in English
 - Fixed failure to resolve regional translations (like pt_BT or pt_PT) from the locale
 - Fixed VN log text overlap issues
 - Fixed commodities not being added through unidiff
 - Fixed safe lane rendering alpha being wrong
 - Fixed misbehaviours with Maikki, Nelly 2, Shark 3, Shipwreck, Travelling Merchant, Warlords Battle, and Particle Physics 2
 - Fixed backgrounds accumulating when messing with options
 - Fixed issues with board scripts getting deferred with respect to boarding script
 - Fixed some instances of background text interfering with how foreground text was drawn
 - Fixed some missions causing trouble when saved/reloaded (due to dynamic factions)
 - Fixed minor Ship AI issues (rename at game start)
 - Fixed autonav via the map during a landing sequence
 - Fixed autonav giving away autofollowed pilots and unknown destination systems
 - Improved speed and accuracy of autonav stopping
 - Improved mission marker behavior (show planets more, always clean up at end of mission)
 - Kicked Empire patrols out of the Arandon system
 - Gave pirate ships dodgier outfits
 - Proofread too many parts of the game to mention
 - AI should only try to jump to systems with their faction presence
 - Wrap OSD titles as necessary
 - Don't allow illegal characters in pilot name
 - Be kinder to old video drivers
 - More music
 - More meow


## 0.9.0-beta.2

 - Prevented Naev from losing the player's (pre-0.9.0-beta) licenses on first load
 - Fixed missing credits and translation coverage data
 - Improved phrasing
 - Prevented players from getting stranded without access to fuel
 - Mission script fixes for "Helping Nelly" and "The one with the Visit"
 - Outfit script fix for "Weapons Ionizer"
 - Fixed issues impacting at least some Windows / Intel graphics combinations
 - Hulls are more widely available
 - Improved some of the map outfits
 - Do not render systems with unknown assets as restricted
 - Added gamma correction to Options
 - Fixed reproducible crash when boarded pilots get removed
 - Added counterfeit licenses to pirate worlds
 - Remove minor energy malus from sensor array and jump detector
 - Electron burst cannon is no longer widely available


## 0.9.0 (beta)

#### New mechanics
 - Added new utility outfits with complex effects
 - Changed ship classification, removing rare classes while adding Interceptor and Battleship classes
 - Illegal cargo and ship-to-ship detection
 - Pilots can now go into stealth mode
 - Systems have "safe lanes" patrolled by the governing faction
 - Electronic warfare parameters are simplified and visible
 - Added escorts for hire
 - Some simple minigames have been added
 - Scramblers and jammers have fixed chance to mess up missiles depending on their resistance
 - Restricted systems where dominant faction will attack on sight
 - Some bulk freighter-class ships added
 - Systems can have different effects on all ships in them
 - Fake transponder replaces fake id

#### Visual improvements
 - New fancy system rendering effects
 - Ships and rockets now have engine trails
 - Beam visuals have been made more flexible and pretty
 - Jumping visuals improved
 - Redid the shake visuals and added a small damage visual
 - Most special effects implemented as shaders
 - Most small visuals redone to be more visible and clean
 - Similar presences are now merged in map
 - Overhauled all the backgrounds

#### Gameplay changes
 - Pirates split into multiple clans and marauders
 - Added discovery messages as you explore the universe
 - Overhauled NPC AI
 - Overhaul and rebalance of most outfits
 - Wanted ships no longer aggro defense forces (bounties)
 - Bribed pilots don't become hostile again unless attacked
 - Stress now decreases based on ship mass
 - Merged the Independent and Civilian factions
 - Game now tracks meta-data like ships destroyed and time played
 - Trade lane routes made explicit
 - More common and useful derelict ships
 - Missiles have lock-on reduced and in-flight calibration added
 - Tutorial redone with Ship AI that is also accessible from the info menu
 - New ships including the Starbridge

#### Quality of Life
 - Autonav supports landing and boarding
 - Comm window reworked and you can bribe multiple pilots at once
 - Possible to change or unequip ships with deployed fighters
 - More fine-grained autonav reset control by setting enemy distance
 - Added autoequip functionality
 - Able to filter equipable outfits
 - Minimal view mode for the map
 - More visible map markers
 - More in-game tutorial-ish explanations for new mechanics as you encounter them
 - You can now favourite your ships to help with sorting
 - Redid boarding window to be more intuitive and easier to loot what you want
 - Paste support for input stuff
 - Translation completion status is shown in the options

#### New locations
 - Added gambling resort "Minerva Station"
 - Revamped and improved some existing locations
 - Several new planets and systems

#### 40 New missions
 - Challenge adversaries in the Crimson Gauntlet
 - Follow happenings on "Minerva Station"
 - Invade the frontier with the Dvaered
 - Ship enthusiast quiz
 - Deliver fancy contraband all over the universe
 - Raid trader convoys
 - Rescue derelict crew
 - Small early game tutorial-ish campaign
 - Neutral campaign to transform the universe
 - Help the Za'lek do particle physics
 - Meow

#### New translation(s) in progress:
 - Czech
 - French
 - Korean
 - Portuguese
 - Japanese

#### Engine Changes
 - Added an optimizer to improve automatic outfitting choices
 - A ton of new ship stat attributes have been added
 - Support for Lua-based hooks in Outfits for complex behaviours
 - Support for post-processing shaders
 - Added rendering and update hooks in the Lua API
 - Added image format support beyond PNG (notably WebP)
 - Support for arbitrary ship display classes
 - Game data now handled by PhysicsFS, allowing for multiple sources and easier modding
 - Meson is now the only build system, and development builds can integrate all assets/translations without being installed
 - Fonts now use distance fields and much better in many cases
 - Improved how Lua was being loaded
 - Added library that supports lots of Love2D API in Naev
 - Added Visual Novel library
 - Added card games
 - Added dynamic factions
 - Added dynamic commodities
 - Lua support for advanced sound effects
 - Most markers and indicators use signed distance functions now
 - Internally using linear colourspace
 - Faction presence computed with base and bonus values
 - Virtual assets have been redone and are more flexible than before
 - Point value system for ships to help with presence and other things
 - Support for shipstats at a system level
 - Initial support for 3D models
 - Proper support for line breaks in most languages
 - Most objects (ships, planets, etc.) have tags that can be used from Lua
 - Lots of optimization


## 0.8.2

#### Gameplay
 - Fixed duplicate rewards from pirate ship-stealing missions. (Sorry.)
 - Fixed the Advanced Nebula Research mission's failure condition in case you don't stick with the transport ship. (Sorry.)
 - Fixed the "The one with the Runaway" mission so the captured drone doesn't appear back in space

#### Engine
 - Fixed a bug loading games with short (1-character) names
 - Tweaked chances of seeing Spaceport Bar missions
 - Updated German translation
 - Fixed "configure" script when the system has a "cxsparse" library and no "csparse"
 - Fixed source .tar.gz so ./configure is immediately usable again. (Note: 0.9.x will use Meson for builds.)


## 0.8.1

#### Gameplay
 - Lowered large ships' time constant (renamed from time dilation) by 50% of the deviation from 100%
 - Tweaked Za'lek ships' stats and outfit slot behavior to match expectations

#### Engine
 - Restored macOS support. (Catalina users will have to bypass Gatekeeper: See https://github.com/naev/naev/wiki/FAQ for details.)
 - Fixed a crash-loop when the "saves" folder gets populated by Steam data (or other files) and no Naev save files
 - Fixed intermittent error messages about the "Lua Spawn script for faction 'Trader'"
 - Fixed rare/potential bugs in font and save-file code
 - Fixed crash when navigating landing screens with the tab key
 - Updated German translation
 - Improved text in minor ways


## 0.8.0

#### Gameplay
 - Overhaul of the interface to be more sleek and functional
    - Interface is much more slick
    - Dark theme to be more consistent with space
    - Font uses outlines to be more readable
 - New map overlay with adjustable opacity
 - Added rarity indicator to ships and outfits
 - Changed fonts
 - Indicate non-common NPC with exclamation marks
 - Added accessory slot and unique accessory outfits as mission rewards
 - Simple economy model implemented with map visualizations
 - Added travelling merchant who sells unique items
 - Made missiles and fighter bays reload while in space
 - Modified the balancing of missiles
 - Added asteroids and mining
 - Improved player GUI
 - Brushed GUI is now the default
 - Improved and fixed escort system
 - Made Pirates and FLF spawn in a fairer way
 - Made time pass at different rates for different ships ("Time Dilation")
 - Made piracy missions available from any Independent or black market planet
 - Substantially increased pay for unique missions (10x in most cases)
 - Made references to the player gender-neutral
 - Made combat music vary from faction to faction
 - Made it so AI ships spawn with cargo
 - Improved AI behaviours
 - Nerfed Quicksilver
 - Added the ability to buy "fake IDs" from pirate strongholds
 - Made jammers into activated outfits that increase cloaking
 - Added Soromid organic ships that level up organs
 - Improved and expanded NPC portraits
 - Commodities can be sold/bought everywhere
 - Added a "slow mode", which runs the game at half speed (like an easy mode)
 - Added a ship log which records events
 - Added a "system map" which displays information about known remote planets
 - Added support for giving commands to individual escorts
 - New intro images replacing old placeholders
 - Increased pirate name variety for bounty missions
 - Ships now travel with you automatically for free, as with outfits
 - Added map decorators showing locations of factions and the Nebula
 - Added a dogfight aiming helper
 - More music
 - New and/or improved missions
    - New Za'lek mini-campaign
    - Completed the FLF campaign
    - Fixed up the Collective campaign
    - Improved the Shark (Nexus Shipyards) campaign
    - Improved the Dvaered anti-FLF campaign
    - Added and improved piracy missions
    - New minor Soromid campaign, "Coming Out"
    - New tutorial mission at the start of a new game
    - Various newly added and improved generic missions

#### Engine
 - Support for compilation with Meson
 - HiDPI-awareness
 - Support for translations
 - Added shaders to speed up and improve graphics
 - Added support for non-ascii direct character input
 - Added support for map decorators
 - Removed support for Lua 5.0
 - Removed support for SDL 1, only SDL 2 is supported
 - Added support for translating
 - Made the OSD compact itself to avoid showing redundant information
 - Made Autonav able to follow ships
 - Consolidated the effects of cloaking and jammers under cloaking
 - Added workaround for ALSOFT buggy version that crashes
 - Added a polygon-based collision algorithm
 - Added some symbols for partial colorblind accessibility
 - Support #include in shaders
 - Multiple font support
 - Many bugfixes
