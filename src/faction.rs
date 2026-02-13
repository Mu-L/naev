#![allow(dead_code, unused_variables, unused_imports)]
use crate::array;
use crate::array::ArrayCString;
use crate::nlua::LuaEnv;
use crate::nlua::{NLUA, NLua};
use anyhow::Context as AnyhowContext;
use anyhow::Result;
use gettext::gettext;
use helpers::{binary_search_by_key_ref, sort_by_key_ref};
use mlua::{BorrowedStr, Either, Function, UserData, UserDataMethods, UserDataRef};
use naev_core::{nxml, nxml_err_attr_missing, nxml_warn_node_unknown};
use nalgebra::{Vector3, Vector4};
use nlog::warn_err;
use nlog::{warn, warnx};
use rayon::prelude::*;
use renderer::{Context, ContextWrapper, colour, texture};
use slotmap::{Key, KeyData, SecondaryMap, SlotMap};
use std::collections::HashMap;
use std::ffi::{CStr, CString, OsStr};
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex, OnceLock, RwLock};

#[derive(Copy, Clone, Debug, PartialEq)]
enum GridEntry {
   None,
   Enemies,
   Allies,
   Neutrals,
}
#[derive(Default)]
struct Grid {
   data: Vec<GridEntry>,
   size: usize,
}
impl std::ops::Index<(FactionRef, FactionRef)> for Grid {
   type Output = GridEntry;
   fn index(&self, index: (FactionRef, FactionRef)) -> &Self::Output {
      &self.data[self.offset(index)]
   }
}
impl std::ops::IndexMut<(FactionRef, FactionRef)> for Grid {
   fn index_mut(&mut self, index: (FactionRef, FactionRef)) -> &mut Self::Output {
      let offset = self.offset(index);
      &mut self.data[offset]
   }
}
impl Grid {
   const fn new() -> Self {
      Self {
         data: vec![],
         size: 0,
      }
   }

   fn offset(&self, idx: (FactionRef, FactionRef)) -> usize {
      let a = idx.0.slot();
      let b = idx.0.slot();
      if a <= b {
         a * self.size + b
      } else {
         a * self.size + b
      }
   }

   fn recompute(&mut self) -> Result<()> {
      let factions = FACTIONS.read().unwrap();
      self.size = factions.capacity();
      self.data.clear();
      self.data.resize(self.size * self.size, GridEntry::None);

      for (id, fct) in factions.iter() {
         let dat = &fct.data;
         self[(dat.id, dat.id)] = GridEntry::Allies;
         for a in &dat.allies {
            #[cfg(debug_assertions)]
            {
               let ent = self[(dat.id, *a)];
               if ent != GridEntry::Allies && ent != GridEntry::None {
                  warn!(
                     "Incoherent faction grid! '{}' and '{}' already have contradictory relationships!",
                     &dat.name, &factions[*a].data.name
                  );
               }
            }
            self[(dat.id, *a)] = GridEntry::Allies;
         }
         for e in &dat.enemies {
            self[(dat.id, *e)] = GridEntry::Enemies;
            #[cfg(debug_assertions)]
            {
               let ent = self[(dat.id, *e)];
               if ent != GridEntry::Enemies && ent != GridEntry::None {
                  warn!(
                     "Incoherent faction grid! '{}' and '{}' already have contradictory relationships!",
                     &dat.name, &factions[*e].data.name
                  );
               }
            }
         }
         for n in &dat.neutrals {
            self[(dat.id, *n)] = GridEntry::Neutrals;
            #[cfg(debug_assertions)]
            {
               let ent = self[(dat.id, *n)];
               if ent != GridEntry::Neutrals && ent != GridEntry::None {
                  warn!(
                     "Incoherent faction grid! '{}' and '{}' already have contradictory relationships!",
                     &dat.name, &factions[*n].data.name
                  );
               }
            }
         }
      }
      Ok(())
   }
}
static GRID: RwLock<Grid> = RwLock::new(Grid::new());

/// Full faction data
pub static FACTIONS: LazyLock<RwLock<SlotMap<FactionRef, Faction>>> =
   LazyLock::new(|| RwLock::new(SlotMap::with_key()));
pub static PLAYER: OnceLock<FactionRef> = OnceLock::new();
const PLAYER_FACTION_NAME: &str = "Escort";

slotmap::new_key_type! {
pub struct FactionRef;
}

impl FactionRef {
   pub fn slot(&self) -> usize {
      // TODO this is not very safe and probably a bad idea
      (self.data().as_ffi() & 0xffff_ffff) as usize - 1
   }

   pub fn as_ffi(self) -> i64 {
      self.data().as_ffi() as i64
   }

   pub fn from_ffi(value: i64) -> Self {
      Self(KeyData::from_ffi(value as u64))
   }

   pub fn new(name: &str) -> Option<FactionRef> {
      let factions = FACTIONS.read().unwrap();
      for (id, fct) in factions.iter() {
         if fct.data.name == name {
            return Some(id);
         }
      }
      None
   }

   pub fn call<F, R>(&self, f: F) -> Result<R>
   where
      F: Fn(&Faction) -> R,
   {
      let factions = FACTIONS.read().unwrap();
      match factions.get(*self) {
         Some(fct) => Ok(f(fct)),
         None => anyhow::bail!("faction not found"),
      }
   }

   pub fn call2<F, R>(&self, other: &Self, f: F) -> Result<R>
   where
      F: Fn(&Faction, &Faction) -> R,
   {
      let factions = FACTIONS.read().unwrap();
      if let Some(fct1) = factions.get(*self)
         && let Some(fct2) = factions.get(*other)
      {
         Ok(f(fct1, fct2))
      } else {
         anyhow::bail!("faction not found")
      }
   }

   pub fn hit(&self, val: f32, system: &mlua::Value, source: &str, single: bool) -> Result<f32> {
      let factions = FACTIONS.read().unwrap();
      match factions.get(*self) {
         Some(fct) => {
            let ret = fct.hit_lua(val, system, source, false, None)?;
            if !single {
               for a in &fct.data.allies {
                  factions[*a].hit_lua(val, system, source, true, Some(fct))?;
               }
               for e in &fct.data.enemies {
                  factions[*e].hit_lua(-val, system, source, true, Some(fct))?;
               }
            }
            Ok(ret)
         }
         None => anyhow::bail!("faction not found"),
      }
   }

   /// Checks to see if two factions are allies
   pub fn are_allies(&self, other: &Self) -> bool {
      if self == other {
         true
      } else if other == PLAYER.get().unwrap() {
         self
            .call(|fct| {
               let std = fct.standing.read().unwrap();
               let api = fct.api.get().unwrap();
               std.player >= api.friendly_at
            })
            .unwrap_or_else(|err| {
               warn_err!(err);
               false
            })
      } else {
         GRID.read().unwrap()[(*self, *other)] == GridEntry::Enemies
      }
   }

   /// Checks to see if two factions are enemies
   pub fn are_enemies(&self, other: &Self) -> bool {
      if self == other {
         false
      } else if other == PLAYER.get().unwrap() {
         self
            .call(|fct| fct.standing.read().unwrap().player < 0.)
            .unwrap_or_else(|err| {
               warn_err!(err);
               false
            })
      } else {
         GRID.read().unwrap()[(*self, *other)] == GridEntry::Enemies
      }
   }

   /// Checks to see if two factions are true neutrals
   pub fn are_neutrals(&self, other: &Self) -> bool {
      if self == other || other == PLAYER.get().unwrap() {
         false
      } else {
         GRID.read().unwrap()[(*self, *other)] == GridEntry::Neutrals
      }
   }
}

#[derive(Debug)]
struct StandingAPI {
   friendly_at: f32,
   hit: Option<mlua::Function>,
   hit_test: Option<mlua::Function>,
   text_rank: Option<mlua::Function>,
   text_broad: Option<mlua::Function>,
   reputation_max: Option<mlua::Function>,
}

#[derive(Debug)]
struct Standing {
   player: f32,
   p_override: Option<f32>,
   f_known: bool,
   f_invisible: bool,
}

#[derive(Debug)]
pub struct Faction {
   api: OnceLock<StandingAPI>,
   standing: RwLock<Standing>,
   data: FactionData,
}
impl Faction {
   pub fn player(&self) -> f32 {
      let standing = self.standing.read().unwrap();
      match standing.p_override {
         Some(std) => std,
         None => standing.player,
      }
   }

   pub fn set_player(&self, std: f32) {
      let mut standing = self.standing.write().unwrap();
      if standing.p_override.is_none() {
         standing.player = std;
      }
   }

   pub fn r#override(&self) -> Option<f32> {
      let standing = self.standing.read().unwrap();
      standing.p_override
   }

   pub fn set_override(&self, std: Option<f32>) {
      let mut standing = self.standing.write().unwrap();
      standing.p_override = std;
   }

   pub fn known(&self) -> bool {
      self.standing.read().unwrap().f_known
   }

   pub fn set_known(&self, state: bool) {
      self.standing.write().unwrap().f_known = state;
   }

   pub fn invisible(&self) -> bool {
      self.standing.read().unwrap().f_invisible
   }

   pub fn set_invisible(&self, state: bool) {
      self.standing.write().unwrap().f_invisible = state;
   }

   pub fn fixed(&self) -> bool {
      self.data.f_static
   }

   pub fn dynamic(&self) -> bool {
      self.data.f_dynamic
   }

   fn init_lua(&self, lua: &NLua) -> Result<()> {
      self.data.init_lua(lua)?;
      if let Some(env) = &self.data.lua_env {
         fn load_func(env: &LuaEnv, name: &str) -> Option<mlua::Function> {
            match env.get(name) {
               Ok(f) => Some(f),
               Err(e) => {
                  warn_err!(e);
                  None
               }
            }
         }

         // Store important non-changing stuff here
         self
            .api
            .set(StandingAPI {
               friendly_at: env.get("friendly_at")?,
               hit: load_func(env, "hit"),
               hit_test: load_func(env, "hit_test"),
               text_rank: load_func(env, "text_rank"),
               text_broad: load_func(env, "text_broad"),
               reputation_max: load_func(env, "reputation_max"),
            })
            .map_err(|_| {
               anyhow::anyhow!(
                  "Failed to load StandingAPI for faction '{}'",
                  self.data.name
               )
            })?;
      }
      Ok(())
   }

   fn hit_lua(
      &self,
      val: f32,
      system: &mlua::Value,
      source: &str,
      secondary: bool,
      parent: Option<&Faction>,
   ) -> Result<f32> {
      if self.data.f_static {
         return Ok(0.);
      } else {
         let std = self.standing.read().unwrap();
         if std.p_override.is_some() {
            return Ok(0.);
         }
      }
      let ret: f32 = match &self.api.get().unwrap().hit {
         // (sys, mod, source, secondary, primary_fct)
         Some(hit) => hit.call((system, val, source, secondary, parent.map(|f| f.data.id)))?,
         None => anyhow::bail!("hit function not defined for faction '{}'", &self.data.name),
      };
      Ok(ret)
   }

   fn hit_test_lua(
      &self,
      val: f32,
      system: &mlua::Value,
      source: &str,
      secondary: bool,
   ) -> Result<f32> {
      if self.data.f_static {
         return Ok(0.);
      } else {
         let std = self.standing.read().unwrap();
         if std.p_override.is_some() {
            return Ok(0.);
         }
      }
      let ret: f32 = match &self.api.get().unwrap().hit_test {
         Some(hit) => hit.call((system, val, source, secondary))?,
         None => anyhow::bail!(
            "hit_test function not defined for faction '{}'",
            &self.data.name
         ),
      };
      Ok(ret)
   }
}

#[derive(Debug, Clone)]
pub struct Generator {
   /// ID of faction to generate
   id: FactionRef,
   /// Weight modifier
   weight: f32,
}

#[derive(Debug, Default)]
struct FactionLoad {
   // Generators
   generator: Vec<(String, f32)>,

   // Relationships
   enemies: Vec<String>,
   allies: Vec<String>,
   neutrals: Vec<String>,
}

#[derive(Debug, Default)]
pub struct FactionData {
   id: FactionRef,
   pub name: String,
   pub longname: Option<String>,
   pub displayname: Option<String>,
   pub mapname: Option<String>,
   ai: String,
   description: String,
   local_th: f32,

   // Scripts
   script_standing: String,
   script_spawn: String,
   script_equip: String,

   // Graphics
   pub logo: Option<texture::Texture>,
   pub colour: Vector4<f32>,

   // Relationships
   enemies: Vec<FactionRef>,
   allies: Vec<FactionRef>,
   neutrals: Vec<FactionRef>,

   // Player stuff
   pub player_def: f32,

   // Scheduler
   sched_env: Option<LuaEnv>,

   // Behaviour
   lua_env: Option<LuaEnv>,

   // Equipping
   equip_env: Option<LuaEnv>,

   // Safe lanes
   lane_length_per_presence: f32,
   lane_base_cost: f32,

   // Presence
   generators: Vec<Generator>,

   // Flags
   pub f_static: bool,
   pub f_invisible: bool,
   pub f_known: bool,
   pub f_dynamic: bool,
   pub f_useshiddenjumps: bool,

   // Tags
   pub tags: Vec<String>,

   // C stuff, TODO remove when unnecessary
   cname: CString,
   clongname: Option<CString>,
   cdisplayname: Option<CString>,
   cmapname: Option<CString>,
   cdescription: CString,
   cai: CString,
   ctags: ArrayCString,
}
impl FactionData {
   /// Loads the elementary faction stuff, does not fill out information dependent on other
   /// factions
   fn new<P: AsRef<Path>>(
      ctx: &ContextWrapper,
      lua: &NLua,
      filename: P,
   ) -> Result<(Self, FactionLoad)> {
      let mut fctload = FactionLoad::default();
      let mut fct = FactionData::default();

      // TODO use default_field_values when stabilized
      // https://github.com/rust-lang/rust/issues/132162
      fct.local_th = 10.;

      let data = ndata::read(filename)?;
      let doc = roxmltree::Document::parse(std::str::from_utf8(&data)?)?;
      let root = doc.root_element();
      fct.name = String::from(match root.attribute("name") {
         Some(n) => n,
         None => {
            return nxml_err_attr_missing!("Damage Type", "name");
         }
      });
      fct.cname = CString::new(fct.name.as_str())?;

      for node in root.children() {
         if !node.is_element() {
            continue;
         }
         match node.tag_name().name().to_lowercase().as_str() {
            "player" => fct.player_def = nxml::node_f32(node)?,
            "longname" => {
               fct.longname = Some(nxml::node_string(node)?);
               fct.clongname = Some(nxml::node_cstring(node)?);
            }
            "display" => {
               fct.displayname = Some(nxml::node_string(node)?);
               fct.cdisplayname = Some(nxml::node_cstring(node)?);
            }
            "mapname" => {
               fct.mapname = Some(nxml::node_string(node)?);
               fct.cmapname = Some(nxml::node_cstring(node)?);
            }
            "description" => {
               fct.description = nxml::node_string(node)?;
               fct.cdescription = nxml::node_cstring(node)?;
            }
            "ai" => {
               fct.ai = nxml::node_string(node)?;
               fct.cai = nxml::node_cstring(node)?;
            }
            "local_th" => fct.local_th = nxml::node_f32(node)?,
            "lane_length_per_presence" => fct.lane_length_per_presence = nxml::node_f32(node)?,
            "lane_base_cost" => fct.lane_base_cost = nxml::node_f32(node)?,
            // TODO COLOUR
            "colour" => continue,
            "logo" => {
               let gfxname = nxml::node_texturepath(node, "gfx/logo/")?;
               fct.logo = Some(
                  texture::TextureBuilder::new()
                     .path(&gfxname)
                     .build_wrap(ctx)?,
               );
            }
            "known" => fct.f_known = true,
            "static" => fct.f_static = true,
            "invisible" => fct.f_invisible = true,
            "useshiddenjumps" => fct.f_useshiddenjumps = true,
            "standing" => fct.script_standing = nxml::node_string(node)?,
            "spawn" => fct.script_spawn = nxml::node_string(node)?,
            "equip" => fct.script_equip = nxml::node_string(node)?,
            "tags" => {
               for node in node.children() {
                  if !node.is_element() {
                     continue;
                  }
                  if let Some(t) = node.text() {
                     fct.tags.push(String::from(t));
                  }
               }
               // Remove when not needed for C interface
               fct.ctags = ArrayCString::new(&fct.tags)?;
            }
            // Temporary scaoffolding stuff
            "allies" => {
               for node in node.children() {
                  if !node.is_element() {
                     continue;
                  }
                  fctload.allies.push(nxml::node_string(node)?);
               }
            }
            "enemies" => {
               for node in node.children() {
                  if !node.is_element() {
                     continue;
                  }
                  fctload.enemies.push(nxml::node_string(node)?);
               }
            }
            "neutrals" => {
               for node in node.children() {
                  if !node.is_element() {
                     continue;
                  }
                  fctload.neutrals.push(nxml::node_string(node)?);
               }
            }
            "generator" => {
               let weight = match node.attribute("weight") {
                  Some(str) => str.parse::<f32>()?,
                  None => 1.0,
               };
               fctload.generator.push((nxml::node_string(node)?, weight));
            }

            // Case we missed everything
            tag => nxml_warn_node_unknown!("Faction", &fct.name, tag),
         }
      }

      // Initaialize Lua scripts
      {
         if !fct.script_spawn.is_empty() {
            fct.sched_env = Some({
               let mut env = lua.environment_new(&fct.script_spawn)?;
               env.load_standard(lua)?;
               env
            });
         }
         if !fct.script_equip.is_empty() {
            fct.equip_env = Some({
               let mut env = lua.environment_new(&fct.script_equip)?;
               env.load_standard(lua)?;
               env
            });
         }
         if !fct.script_standing.is_empty() {
            fct.lua_env = Some({
               let mut env = lua.environment_new(&fct.script_standing)?;
               env.load_standard(lua)?;
               env
            });
         }
      }

      Ok((fct, fctload))
   }

   fn init_lua(&self, lua: &NLua) -> Result<()> {
      fn init_env(lua: &NLua, env: &Option<LuaEnv>, script: &str) -> Result<()> {
         if let Some(env) = env {
            let path = format!("factions/equip/{}.lua", script);
            let data = ndata::read(&path)?;
            let func = lua
               .lua
               .load(std::str::from_utf8(&data)?)
               .set_name(path)
               .into_function()?;
            env.call::<()>(lua, &func, ())?;
         }
         Ok(())
      }

      init_env(lua, &self.equip_env, &self.script_equip)?;
      init_env(lua, &self.sched_env, &self.script_spawn)?;
      init_env(lua, &self.lua_env, &self.script_standing)?;

      Ok(())
   }

   /// Checks to see if two factions are allies
   fn are_allies(&self, other: &Self) -> bool {
      for a in &self.allies {
         if other.id == *a {
            return true;
         }
      }
      for a in &other.allies {
         if self.id == *a {
            return true;
         }
      }
      false
   }

   /// Checks to see if two factions are enemies
   fn are_enemies(&self, other: &Self) -> bool {
      for a in &self.enemies {
         if other.id == *a {
            return true;
         }
      }
      for a in &other.enemies {
         if self.id == *a {
            return true;
         }
      }
      false
   }

   /// Checks to see if two factions are truly neutral to each other
   fn are_neutrals(&self, other: &Self) -> bool {
      for a in &self.neutrals {
         if other.id == *a {
            return true;
         }
      }
      for a in &other.neutrals {
         if self.id == *a {
            return true;
         }
      }
      false
   }

   fn shortname(&self) -> &str {
      gettext(self.displayname.as_ref().unwrap_or(&self.name))
   }

   fn longname(&self) -> &str {
      gettext(
         self
            .longname
            .as_ref()
            .unwrap_or(self.displayname.as_ref().unwrap_or(&self.name)),
      )
   }

   fn mapname(&self) -> &str {
      gettext(
         self
            .mapname
            .as_ref()
            .unwrap_or(self.displayname.as_ref().unwrap_or(&self.name)),
      )
   }
}

/// Loads all the Data
pub fn load() -> Result<()> {
   let ctx = Context::get().as_safe_wrap();
   let base: PathBuf = "factions/".into();
   let files: Vec<_> = ndata::read_dir(&base)?
      .into_iter()
      .filter(|filename| filename.extension() == Some(OsStr::new("xml")))
      .collect();
   let mut data = FACTIONS.write().unwrap();
   let mut load = SecondaryMap::new();
   let mut fctmap: HashMap<String, FactionRef> = HashMap::new();

   // Add play faction before parsing files
   std::iter::once((
      FactionData {
         name: String::from(PLAYER_FACTION_NAME),
         cname: CString::new(PLAYER_FACTION_NAME)?,
         f_static: true,
         f_invisible: true,
         ..Default::default()
      },
      FactionLoad::default(),
   ))
   .chain(
      files
         //.par_iter()
         .iter()
         .filter_map(
            |filename| match FactionData::new(&ctx, &NLUA, &base.join(filename)) {
               Ok(sp) => Some(sp),
               Err(e) => {
                  warn!("Unable to load Faction '{}': {e}", filename.display());
                  None
               }
            },
         ),
   )
   .for_each(|(mut fd, fl)| {
      let name = fd.name.clone();
      let id = data.insert_with_key(|k| {
         fd.id = k;
         Faction {
            api: OnceLock::new(),
            standing: RwLock::new(Standing {
               player: fd.player_def,
               p_override: None,
               f_known: fd.f_known,
               f_invisible: fd.f_invisible,
            }),
            data: fd,
         }
      });
      load.insert(id, fl);
      fctmap.insert(name, id);
   });

   // Seconda pass, social and generators
   for (id, fd) in data.iter_mut() {
      let fl = load.get(id).unwrap();
      // Load generators
      for (name, weight) in &fl.generator {
         if let Some(id) = fctmap.get(name) {
            fd.data.generators.push(Generator {
               id: *id,
               weight: *weight,
            });
         }
      }
      // Load social
      for name in &fl.enemies {
         if let Some(id) = fctmap.get(name) {
            fd.data.enemies.push(*id);
         }
      }
      for name in &fl.allies {
         if let Some(id) = fctmap.get(name) {
            fd.data.allies.push(*id);
         }
      }
      for name in &fl.neutrals {
         if let Some(id) = fctmap.get(name) {
            fd.data.neutrals.push(*id);
         }
      }
   }

   // Save the data
   drop(data);
   match FactionRef::new(PLAYER_FACTION_NAME) {
      Some(id) => PLAYER.set(id).unwrap_or_else(|_| {
         warn!("unable to set player faction ID");
      }),
      None => unreachable!(),
   };

   // Compute grid
   GRID.write().unwrap().recompute()
}

pub fn load_lua() -> Result<()> {
   // Last pass: initialize Lua
   let lua = &NLUA;
   for (id, fct) in FACTIONS.read().unwrap().iter() {
      fct.init_lua(lua)?;
   }
   Ok(())
}

/*@
 * @brief Lua bindings to deal with factions.
 *
 * Use like:
 * @code
 * f = faction.get( "Empire" )
 * if f:playerStanding() < 0 then
 *    -- player is hostile to Empire
 * end
 * @endcode
 *
 * @lua_mod faction
 */
impl UserData for FactionRef {
   fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
      /*@
       * @brief Gets a faction if it exists.
       *
       * @usage f = faction.exists( "Empire" )
       *
       *    @luatparam string name Name of the faction to get if exists.
       *    @luatreturn Faction The faction matching name or nil if not matched.
       * @luafunc exists
       */
      methods.add_function(
         "exists",
         |_, name: BorrowedStr| -> mlua::Result<Option<Self>> {
            for (id, fct) in FACTIONS.read().unwrap().iter() {
               if name == fct.data.name {
                  return Ok(Some(id));
               }
            }
            Ok(None)
         },
      );
      /*@
       * @brief Gets the faction based on its name.
       *
       * @usage f = faction.get( "Empire" )
       *
       *    @luatparam string name Name of the faction to get.
       *    @luatreturn Faction The faction matching name.
       * @luafunc get
       */
      methods.add_function("get", |_, name: BorrowedStr| -> mlua::Result<Self> {
         for (id, fct) in FACTIONS.read().unwrap().iter() {
            if name == fct.data.name {
               return Ok(id);
            }
         }
         Err(mlua::Error::RuntimeError(format!(
            "Faction '{name}' not found."
         )))
      });
      /*@
       * @brief Gets all the factions.
       *
       *    @luatreturn {Faction,...} An ordered table containing all of the factions.
       * @luafunc getAll
       */
      methods.add_function("getAll", |_, ()| -> mlua::Result<Vec<Self>> {
         Ok(FACTIONS.read().unwrap().keys().collect())
      });
      /*@
       * @brief Gets the player's faction.
       *
       * @usage pf = faction.player()
       *
       *    @luareturn Faction The player's faction.
       * @luafunc player
       */
      methods.add_function("player", |_, ()| -> mlua::Result<Self> {
         Ok(*PLAYER.get().unwrap())
      });
      /*@
       * @brief Gets the faction's translated short name.
       *
       * This translated name should be used for display purposes (e.g.
       * messages) where the shorter version of the faction's display name
       * should be used. It cannot be used as an identifier for the faction;
       * for that, use faction.nameRaw() instead.
       *
       * @usage shortname = f:name()
       *
       *    @luatparam Faction f The faction to get the name of.
       *    @luatreturn string The name of the faction.
       * @luafunc name
       */
      methods.add_method("shortname", |_, this, ()| -> mlua::Result<String> {
         Ok(this.call(|fct| fct.data.shortname().to_string())?)
      });
      /*@
       * @brief Gets the faction's raw / "real" (untranslated, internal) name.
       *
       * This untranslated name should be used for identification purposes
       * (e.g. can be passed to faction.get()). It should not be used for
       * display purposes; for that, use faction.name() or faction.longname()
       * instead.
       *
       * @usage name = f:nameRaw()
       *
       *    @luatparam Faction f The faction to get the name of.
       *    @luatreturn string The name of the faction.
       * @luafunc nameRaw
       */
      methods.add_method("shortname", |_, this, ()| -> mlua::Result<String> {
         Ok(this.call(|fct| fct.data.name.clone())?)
      });
      /*@
       * @brief Gets the faction's translated long name.
       *
       * This translated name should be used for display purposes (e.g.
       * messages) where the longer version of the faction's display name
       * should be used. It cannot be used as an identifier for the faction;
       * for that, use faction.nameRaw() instead.
       *
       * @usage longname = f:longname()
       *    @luatparam Faction f Faction to get long name of.
       *    @luatreturn string The long name of the faction (translated).
       * @luafunc longname
       */
      methods.add_method("longname", |_, this, ()| -> mlua::Result<String> {
         Ok(this.call(|fct| fct.data.longname().to_string())?)
      });
      /*@
       * @brief Checks to see if two factions are truly neutral with respect to each
       * other.
       *
       *    @luatparam Faction f Faction to check against.
       *    @luatparam Faction n Faction to check if is true neutral.
       *    @luatreturn boolean true if they are truly neutral, false if they aren't.
       * @luafunc areNeutral
       */
      methods.add_method(
         "areNeutral",
         |_, this, other: UserDataRef<Self>| -> mlua::Result<bool> {
            Ok(this.call2(&other, |fct1, fct2| fct1.data.are_neutrals(&fct2.data))?)
         },
      );
      /*@
       * @brief Checks to see if f is an enemy of e.
       *
       * @usage if f:areEnemies( faction.get( "Dvaered" ) ) then
       *
       *    @luatparam Faction f Faction to check against.
       *    @luatparam Faction e Faction to check if is an enemy.
       *    @luatparam[opt] System sys System to check to see if they are enemies in.
       * Mainly for when comparing to the player's faction.
       *    @luatreturn boolean true if they are enemies, false if they aren't.
       * @luafunc areEnemies
       */
      methods.add_method(
         "areEnemies",
         |_, this, other: UserDataRef<Self>| -> mlua::Result<bool> {
            // TODO system check
            Ok(this.call2(&other, |fct1, fct2| fct1.data.are_enemies(&fct2.data))?)
         },
      );
      /*@
       * @brief Checks to see if f is an ally of a.
       *
       * @usage if f:areAllies( faction.get( "Pirate" ) ) then
       *
       *    @luatparam Faction f Faction to check against.
       *    @luatparam faction a Faction to check if is an enemy.
       *    @luatparam[opt] System sys System to check to see if they are allies in.
       * Mainly for when comparing to the player's faction.
       *    @luatreturn boolean true if they are enemies, false if they aren't.
       * @luafunc areAllies
       */
      methods.add_method(
         "areAllies",
         |_, this, other: UserDataRef<Self>| -> mlua::Result<bool> {
            // TODO system check
            Ok(this.call2(&other, |fct1, fct2| fct1.data.are_allies(&fct2.data))?)
         },
      );

      /*@
       * @brief Modifies the player's standing with the faction.
       *
       * Also can modify the standing with allies and enemies of the faction.
       *
       * @usage f:hit( -5, system.cur() ) -- Lowers faction by 5 at the current system
       * @usage f:hit( 10 ) -- Globally increases faction by 10
       * @usage f:hit( 10, nil, nil, true ) -- Globally increases faction by 10, but
       * doesn't affect allies nor enemies of the faction.
       *
       *    @luatparam Faction f Faction to modify player's standing with.
       *    @luatparam number mod Amount of reputation to change.
       *    @luatparam System|nil extent Whether to make the faction hit local at a
       * system, or global affecting all systems of the faction.
       *    @luatparam[opt="script"] string reason Reason behind it. This is passed as
       * a string to the faction `hit` function. The engine can generate `destroy` and
       * `distress` sources. For missions the default is `script`.
       *    @luatparam[opt=false] boolean ignore_others Whether or not the hit should
       * affect allies/enemies of the faction getting a hit.
       *    @luatreturn How much the reputation was actually changed after Lua script
       * was run.
       * @luafunc hit
       */
      methods.add_method(
         "hit",
         |_,
          this,
          (modifier, system, extent, reason, ignore_others): (
            f32,
            mlua::Value,
            Option<BorrowedStr>,
            Option<BorrowedStr>,
            Option<bool>,
         )|
          -> mlua::Result<f32> {
            let reason = reason.as_ref().map_or("script", |v| v);
            let ignore_others = ignore_others.unwrap_or(false);
            Ok(this.hit(modifier, &system, reason, ignore_others)?)
         },
      );
      /*@
       * @brief Simulates modifying the player's standing with a faction and computes
       * how much would be changed.
       *
       *    @luatparam Faction f Faction to simulate player's standing with.
       *    @luatparam number mod Amount of reputation to simulate change.
       *    @luatparam System|nil extent Whether to make the faction hit local at a
       * system, or global.
       *    @luatparam[opt="script"] string reason Reason behind it. This is passed as
       * a string to the faction `hit` function. The engine can generate `destroy` and
       * `distress` sources. For missions the default is `script`.
       *    @luatreturn How much the reputation was actually changed after Lua script
       * was run.
       * @luafunc hitTest
       */
      methods.add_method(
         "hitTest",
         |_,
          this,
          (modifier, system, extent, reason): (
            f32,
            mlua::Value,
            Option<BorrowedStr>,
            Option<BorrowedStr>,
         )|
          -> mlua::Result<f32> {
            let reason = reason.as_ref().map_or("script", |v| v);
            let ignore_others = true;
            Ok(this.call(|fct| fct.hit_test_lua(modifier, &system, reason, ignore_others))??)
         },
      );
      /*@
       * @brief Gets the player's global reputation with the faction.
       *
       * @usage if f:reputationGlobal() >= 0 then -- Player is not hostile
       *
       *    @luatparam Faction f Faction to get player's standing with.
       *    @luatreturn number The value of the standing.
       * @luafunc reputationGlobal
       */
      methods.add_method("reputationGlobal", |_, this, ()| -> mlua::Result<f32> {
         Ok(this.call(|fct| fct.player())?)
      });
      /*@
       * @brief Gets the human readable standing text corresponding (translated).
       *
       *    @luatparam faction f Faction to get standing text from.
       *    @luatparam[opt=f:reputationGlobal()] number|nil val Value to get the
       * standing text of, or nil to use the global faction standing.
       *    @luatreturn string Translated text corresponding to the faction value.
       * @luafunc reputationText
       */
      methods.add_method(
         "reputationText",
         |_, this, value: Option<f32>| -> mlua::Result<String> {
            Ok(this.call(|fct| {
               if let Some(f) = &fct.api.get().unwrap().text_rank {
                  let value = value.unwrap_or(fct.player());
                  f.call(value)
               } else {
                  Ok(String::from("???"))
               }
            })??)
         },
      );
      /*@
       * @brief Gets the player's default reputation with the faction.
       *
       *    @luatparam Faction f Faction to get player's default standing with.
       *    @luatreturn number The value of the standing.
       * @luafunc reputationDefault
       */
      methods.add_method("reputationDefault", |_, this, ()| -> mlua::Result<f32> {
         Ok(this.call(|fct| fct.data.player_def)?)
      });
      /*@
       * @brief Overrides the player's faction global standing with a faction. Use
       * sparingly as it overwrites local standings at all systems.
       *
       *    @luatparam Faction f Faction to set the player's global reputation with.
       *    @luatparam number The value of the reputation to set to.
       * @luafunc setReputationGlobal
       */
      methods.add_method(
         "setReputationGlobal",
         |_, this, value: f32| -> mlua::Result<()> { Ok(this.call(|fct| fct.set_player(value))?) },
      );
      /*@
       * @brief Enforces the local threshold of a faction starting at a particular
       * system. Meant to be used when computing faction hits from the faction
       * standing Lua scripts. Not meant for use elsewhere.
       *
       *    @luatparam Faction f Faction to apply local threshold to.
       *    @luatparam System sys System to compute the threshold from. This will
       * be the reference and will not have its value modified.
       * @luafunc applyLocalThreshold
       */
      // TODO
      /*
      static int factionL_applyLocalThreshold( lua_State *L )
      {
         FactionRef  f   = luaL_validfaction( L, 1 );
         StarSystem *sys = luaL_validsystem( L, 2 );
         faction_applyLocalThreshold( f, sys );
         return 0;
      }
            */
      /*@
       * @brief Gets the enemies of the faction.
       *
       * @usage for k,v in pairs(f:enemies()) do -- Iterates over enemies
       *
       *    @luatparam Faction f Faction to get enemies of.
       *    @luatreturn {Faction,...} A table containing the enemies of the faction.
       * @luafunc enemies
       */
      methods.add_method("enemies", |_, this, ()| -> mlua::Result<Vec<Self>> {
         Ok(this.call(|fct| fct.data.enemies.clone())?)
      });
      /*@
       * @brief Gets the allies of the faction.
       *
       * @usage for k,v in pairs(f:allies()) do -- Iterate over faction allies
       *
       *    @luatparam Faction f Faction to get allies of.
       *    @luatreturn {Faction,...} A table containing the allies of the faction.
       * @luafunc allies
       */
      methods.add_method("allies", |_, this, ()| -> mlua::Result<Vec<Self>> {
         Ok(this.call(|fct| fct.data.allies.clone())?)
      });
      /*@
       * @brief Gets whether or not a faction uses hidden jumps.
       *
       *    @luatparam Faction f Faction to get whether or not they use hidden jumps.
       *    @luatreturn boolean true if the faction uses hidden jumps, false
       * otherwise.
       * @luafunc usesHiddenJumps
       */
      methods.add_method("usesHiddenJumps", |_, this, ()| -> mlua::Result<bool> {
         Ok(this.call(|fct| fct.data.f_useshiddenjumps)?)
      });
      /*@
       * @brief Gets the faction logo.
       *
       *    @luatparam Faction f Faction to get logo from.
       *    @luatreturn Tex The faction logo or nil if not applicable.
       * @luafunc logo
       */
      methods.add_method(
         "logo",
         |_, this, ()| -> mlua::Result<Option<texture::Texture>> {
            Ok(this
               .call(|fct| fct.data.logo.as_ref().map(|t| t.try_clone()))?
               .transpose()?)
         },
      );
      /*@
       * @brief Gets the faction colour.
       *
       *    @luatparam Faction f Faction to get colour from.
       *    @luatreturn Colour|nil The faction colour or nil if not applicable.
       * @luafunc colour
       */
      methods.add_method("colour", |_, this, ()| -> mlua::Result<colour::Colour> {
         Ok(this.call(|fct| fct.data.colour)?.into())
      });
      /*@
       * @brief Checks to see if a faction is known by the player.
       *
       * @usage b = f:known()
       *
       *    @luatparam Faction f Faction to check if the player knows.
       *    @luatreturn boolean true if the player knows the faction.
       * @luafunc known
       */
      methods.add_method("known", |_, this, ()| -> mlua::Result<bool> {
         Ok(this.call(|fct| fct.known())?)
      });
      /*@
       * @brief Sets a faction's known state.
       *
       * @usage f:setKnown( false ) -- Makes faction unknown.
       *    @luatparam Faction f Faction to set known.
       *    @luatparam[opt=false] boolean b Whether or not to set as known.
       * @luafunc setKnown
       */
      methods.add_method_mut("setKnown", |_, this, known: bool| -> mlua::Result<()> {
         Ok(this.call(|fct| fct.set_known(known))?)
      });
      /*@
       * @brief Checks to see if a faction is invisible the player.
       *
       * @usage b = f:invisible()
       *
       *    @luatparam Faction f Faction to check if is invisible to the player.
       *    @luatreturn boolean true if the faction is invisible to the player.
       * @luafunc invisible
       */
      methods.add_method("invisible", |_, this, ()| -> mlua::Result<bool> {
         Ok(this.call(|fct| fct.invisible())?)
      });
      /*@
       * @brief Checks to see if a faction has a static standing with the player.
       *
       * @usage b = f:static()
       *
       *    @luatparam Faction f Faction to check if has a static standing to the
       * player.
       *    @luatreturn boolean true if the faction is static to the player.
       * @luafunc static
       */
      methods.add_method("static", |_, this, ()| -> mlua::Result<bool> {
         Ok(this.call(|fct| fct.fixed())?)
      });
      /*@
       * @brief Gets the overridden reputation value of a faction.
       *
       *    @luatparam Faction f Faction to get whether or not the reputation is
       * overridden and the value.
       *    @luatreturn number|nil The override reputation value or nil if not
       * overridden.
       * @luafunc reputationOverride
       */
      methods.add_method(
         "reputationOverride",
         |_, this, ()| -> mlua::Result<Option<f32>> { Ok(this.call(|fct| fct.r#override())?) },
      );
      /*@
       * @brief Sets the overridden reputation value of a faction.
       *
       *    @luatparam Faction f Faction to enable/disable reputation override of.
       *    @luatparam number|nil value Sets the faction reputation override to the value if
       * a number, or disables it if nil.
       * @luafunc setReputationOverride
       */
      methods.add_method(
         "setReputationOverride",
         |_, this, value: Option<f32>| -> mlua::Result<()> {
            Ok(this.call(|fct| fct.set_override(value))?)
         },
      );
      /*@
       * @brief Gets the tags a faction has.
       *
       * @usage for k,v in ipairs(f:tags()) do ... end
       * @usage if f:tags().likes_cheese then ... end
       * @usage if f:tags("generic") then ... end
       *
       *    @luatparam[opt=nil] string tag Tag to check if exists.
       *    @luatreturn table|boolean Table of tags where the name is the key and true
       * is the value or a boolean value if a string is passed as the second parameter
       * indicating whether or not the specified tag exists.
       * @luafunc tags
       */
      methods.add_method("tags", |_, this, ()| -> mlua::Result<Vec<String>> {
         Ok(this.call(|fct| fct.data.tags.clone())?)
      });
      /*@
       * @brief Adds a faction dynamically. Note that if the faction already exists as
       * a dynamic faction, the existing one is returned.
       *
       * @note Defaults to known.
       *
       *    @luatparam Faction|nil base Faction to base it off of or nil for new
       * faction.
       *    @luatparam string name Name to give the faction.
       *    @luatparam[opt] string display Display name to give the faction.
       *    @luatparam[opt] table params Table of parameters. Options include `ai`
       * (string) to set the AI to use, `clear_allies` (boolean) to clear all allies,
       * `clear_enemies` (boolean) to clear all enemies, `player` (number) to set the
       * default player standing, `colour` (string|colour) which represents the
       * factional colours.
       *    @luatreturn The newly created faction.
       * @luafunc dynAdd
       */
      methods.add_function(
         "dynAdd",
         |lua,
          (base, name, display, params): (
            Option<UserDataRef<Self>>,
            String,
            Option<String>,
            Option<mlua::Table>,
         )|
          -> mlua::Result<Self> {
            let data = FACTIONS.write().unwrap();
            let params = params.unwrap_or_else(|| lua.create_table().unwrap());
            let base = if let Some(reference) = base {
               let base = &data.get(*reference).context("faction not found")?.data;
               let ai = params
                  .get::<Option<String>>("ai")?
                  .unwrap_or(base.ai.clone());
               let clear_allies: bool = params.get("clear_allies")?;
               let clear_enemies: bool = params.get("clear_enemies")?;
               let player_def = params
                  .get::<Option<f32>>("player")?
                  .unwrap_or(base.player_def);
               let colour = params
                  .get::<Option<colour::Colour>>("colour")?
                  .unwrap_or(base.colour.into());
               let allies = if clear_allies {
                  Vec::new()
               } else {
                  base.allies.clone()
               };
               let enemies = if clear_enemies {
                  Vec::new()
               } else {
                  base.enemies.clone()
               };
               FactionData {
                  id: FactionRef::null(),
                  cname: CString::new(name.as_str()).unwrap(),
                  name,
                  cdisplayname: display.clone().map(|s| CString::new(s.as_str()).unwrap()),
                  displayname: display,
                  ai,
                  logo: base.logo.as_ref().map(|l| l.try_clone()).transpose()?,
                  colour: colour.into(),
                  player_def,
                  allies,
                  enemies,
                  // TODO more stuff
                  f_static: base.f_static,
                  tags: base.tags.clone(),
                  f_dynamic: true,
                  ..Default::default()
               }
            } else {
               let ai: String = params.get("ai").unwrap_or(String::new());
               FactionData {
                  cname: CString::new(name.as_str()).unwrap(),
                  name,
                  cdisplayname: display.clone().map(|s| CString::new(s.as_str()).unwrap()),
                  displayname: display,
                  ai,
                  f_dynamic: true,
                  ..Default::default()
               }
            };
            todo!();
         },
      );
   }
}

// Here be C API
use std::mem::ManuallyDrop;
use std::os::raw::{c_char, c_double, c_int};

#[unsafe(no_mangle)]
pub extern "C" fn _faction_isFaction(f: i64) -> c_int {
   match FACTIONS.read().unwrap().get(FactionRef::from_ffi(f)) {
      Some(_) => 1,
      None => 0,
   }
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_exists(name: *const c_char) -> i64 {
   let ptr = unsafe { CStr::from_ptr(name) };
   let name = ptr.to_str().unwrap();
   for (id, val) in FACTIONS.read().unwrap().iter() {
      if name == val.data.name {
         return id.as_ffi();
      }
   }
   0
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_get(name: *const c_char) -> i64 {
   let ptr = unsafe { CStr::from_ptr(name) };
   let name = ptr.to_str().unwrap();
   for (id, val) in FACTIONS.read().unwrap().iter() {
      if name == val.data.name {
         return id.as_ffi();
      }
   }
   warnx!(gettext("Faction '{}' not found in stack."), name);
   0
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_getAll() -> *const i64 {
   let mut fcts: Vec<i64> = vec![];
   for (id, val) in FACTIONS.read().unwrap().iter() {
      fcts.push(id.as_ffi());
   }
   let arr = ManuallyDrop::new(array::Array::new(&fcts).unwrap());
   arr.as_ptr() as *const i64
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_getAllVisible() -> *const i64 {
   let mut fcts: Vec<i64> = vec![];
   for (id, fct) in FACTIONS.read().unwrap().iter() {
      if !fct.data.f_invisible {
         fcts.push(id.as_ffi());
      }
   }
   let arr = ManuallyDrop::new(array::Array::new(&fcts).unwrap());
   arr.as_ptr() as *const i64
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_getKnown() -> *const i64 {
   let mut fcts: Vec<i64> = vec![];
   for (id, val) in FACTIONS.read().unwrap().iter() {
      if !val.data.f_invisible && !val.standing.read().unwrap().f_known {
         fcts.push(id.as_ffi());
      }
   }
   let arr = ManuallyDrop::new(array::Array::new(&fcts).unwrap());
   arr.as_ptr() as *const i64
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_clearKnown() {
   for (id, val) in FACTIONS.read().unwrap().iter() {
      val.standing.write().unwrap().f_known = val.data.f_known;
   }
}

/// Helper function for the C-side
fn faction_c_call<F, R>(id: i64, f: F) -> Result<R>
where
   F: Fn(&Faction) -> R,
{
   let factions = FACTIONS.read().unwrap();
   match factions.get(FactionRef::from_ffi(id)) {
      Some(fct) => Ok(f(fct)),
      None => anyhow::bail!("faction not found"),
   }
}
fn faction_c_call_mut<F, R>(id: i64, f: F) -> Result<R>
where
   F: Fn(&mut Faction) -> R,
{
   let mut factions = FACTIONS.write().unwrap();
   match factions.get_mut(FactionRef::from_ffi(id)) {
      Some(fct) => {
         if fct.dynamic() {
            Ok(f(fct))
         } else {
            anyhow::bail!("trying to modify a non-dynamic faction!")
         }
      }
      None => anyhow::bail!("faction not found"),
   }
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_isStatic(id: i64) -> i64 {
   faction_c_call(id, |fct| match fct.fixed() {
      true => 1,
      false => 0,
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      0
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_isInvisible(id: i64) -> i64 {
   faction_c_call(id, |fct| match fct.invisible() {
      true => 1,
      false => 0,
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      0
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_setInvisible(id: i64, state: i64) -> c_int {
   faction_c_call(id, |fct| {
      fct.set_invisible(!matches!(state, 0));
      0
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      -1
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_isKnown(id: i64) -> i64 {
   faction_c_call(id, |fct| match fct.known() {
      true => 1,
      false => 0,
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      0
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_isDynamic(id: i64) -> i64 {
   faction_c_call(id, |fct| match fct.dynamic() {
      true => 1,
      false => 0,
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      0
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_name(id: i64) -> *const c_char {
   faction_c_call(id, |fct| {
      // Not translated on purpose
      fct.data.cname.as_ptr()
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_shortname(id: i64) -> *const c_char {
   faction_c_call(id, |fct| {
      let ptr = match &fct.data.cdisplayname {
         Some(name) => name.as_ptr(),
         None => fct.data.cname.as_ptr(),
      };
      unsafe { naevc::gettext_rust(ptr) }
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_longname(id: i64) -> *const c_char {
   faction_c_call(id, |fct| {
      let ptr = match &fct.data.clongname {
         Some(name) => name.as_ptr(),
         None => match &fct.data.cdisplayname {
            Some(name) => name.as_ptr(),
            None => fct.data.cname.as_ptr(),
         },
      };
      unsafe { naevc::gettext_rust(ptr) }
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_mapname(id: i64) -> *const c_char {
   faction_c_call(id, |fct| {
      let ptr = match &fct.data.cmapname {
         Some(name) => name.as_ptr(),
         None => match &fct.data.cdisplayname {
            Some(name) => name.as_ptr(),
            None => fct.data.cname.as_ptr(),
         },
      };
      unsafe { naevc::gettext_rust(ptr) }
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_description(id: i64) -> *const c_char {
   faction_c_call(id, |fct| {
      let ptr = fct.data.cdescription.as_ptr();
      unsafe { naevc::gettext_rust(ptr) }
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_default_ai(id: i64) -> *const c_char {
   faction_c_call(id, |fct| fct.data.cai.as_ptr()).unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_tags(id: i64) -> *mut *const c_char {
   faction_c_call(id, |fct| fct.data.ctags.as_ptr()).unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null_mut()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_lane_length_per_presence(id: i64) -> c_double {
   faction_c_call(id, |fct| fct.data.lane_length_per_presence as c_double).unwrap_or_else(|err| {
      warn_err!(err);
      0.0
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_lane_base_cost(id: i64) -> c_double {
   faction_c_call(id, |fct| fct.data.lane_base_cost as c_double).unwrap_or_else(|err| {
      warn_err!(err);
      0.0
   })
}

/*
#[unsafe(no_mangle)]
pub extern "C" fn _faction_clearEnemy(id: i64) {
   faction_c_call_mut(id, |fct| {
      fct.data.enemies = Vec::new();
   } ) .unwrap_or_else(|err| {
      warn_err!(err);
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_addEnemy(id: i64, o: i64) {
   if id==o { return; }
   faction_c_call_mut(id, |fct| {
      if !fct.data.enemies.contains(o) {
         fct.data.enemies.push( o );
      }
   } ) .unwrap_or_else(|err| {
      warn_err!(err);
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_rmEnemy(id: i64, o: i64) {
   if id==o { return; }
   let o = FactionRef::from_ffi(o);
   faction_c_call_mut(id, |fct| {
      if let Some(pos) = fct.data.enemies.iter().position(|x| *x == o) {
         fct.data.enemies.swap_remove(pos);
      }
   } ) .unwrap_or_else(|err| {
      warn_err!(err);
   })
}
*/

#[unsafe(no_mangle)]
pub extern "C" fn _faction_logo(id: i64) -> *const naevc::glTexture {
   faction_c_call(id, |fct| match &fct.data.logo {
      Some(logo) => logo as *const texture::Texture as *const naevc::glTexture,
      None => std::ptr::null(),
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_colour(id: i64) -> *const naevc::glColour {
   faction_c_call(id, |fct| {
      &fct.data.colour as *const Vector4<f32> as *const naevc::glColour
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}

#[unsafe(no_mangle)]
pub extern "C" fn _faction_setKnown(id: i64, state: i64) -> c_int {
   faction_c_call(id, |fct| {
      fct.set_known(!matches!(state, 0));
      0
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      -1
   })
}
