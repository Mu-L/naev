#![allow(dead_code, unused_variables, unused_imports)]
use crate::array;
use crate::array::ArrayCString;
use crate::nlua::LuaEnv;
use crate::nlua::{NLUA, NLua};
use anyhow::Result;
use gettext::gettext;
use helpers::{binary_search_by_key_ref, sort_by_key_ref};
use naev_core::{nxml, nxml_err_attr_missing, nxml_warn_node_unknown};
use nalgebra::{Vector3, Vector4};
use nlog::warn_err;
use nlog::{warn, warnx};
use rayon::prelude::*;
use renderer::{Context, ContextWrapper, texture};
use slotmap::{Key, KeyData, SlotMap};
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
      &self.data[self.offset(index.slot())]
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
      self.size = factions.len();
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
      (self.data().as_ffi() & 0xffff_ffff) as usize
   }

   pub fn as_ffi(self) -> i64 {
      self.data().as_ffi() as i64
   }

   pub const fn from_ffi(value: i64) -> Self {
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

   pub fn hit(&self, val: f32, source: &str, single: bool) -> Result<f32> {
      let factions = FACTIONS.read().unwrap();
      match factions.get(*self) {
         Some(fct) => {
            let ret = fct.hit_lua(val, source, false, None)?;
            if !single {
               for a in &fct.data.allies {
                  factions[*a].hit_lua(val, source, true, Some(fct))?;
               }
               for e in &fct.data.enemies {
                  factions[*e].hit_lua(-val, source, true, Some(fct))?;
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
   pub data: DataWrapper,
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
      match &self.data {
         DataWrapper::Static(_) => false,
         DataWrapper::Dynamic(_) => true,
      }
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
         Some(hit) => hit.call((val, source, secondary))?,
         None => anyhow::bail!("hit function not defined for faction '{}'", &self.data.name),
      };
      Ok(ret)
   }
}

/// Wrapper for both Dynamic and Static factions
#[derive(Debug)]
pub enum DataWrapper {
   Static(&'static FactionData),
   Dynamic(Box<FactionData>),
}
impl std::ops::Deref for DataWrapper {
   type Target = FactionData;
   fn deref(&self) -> &<Self as std::ops::Deref>::Target {
      match self {
         Self::Static(d) => d,
         Self::Dynamic(d) => d,
      }
   }
}

#[derive(Debug)]
pub struct Generator {
   /// Generator ID
   id: usize,
   /// Weight modifier
   weight: f32,
}
impl Generator {
   fn new(factions: &[FactionLoad], names: &[String], weights: &[f32]) -> Vec<Self> {
      let mut generator: Vec<Generator> = vec![];
      for (name, weight) in names.iter().zip(weights.iter()) {
         if let Some(id) = FactionLoad::get(factions, name) {
            generator.push(Generator {
               id,
               weight: *weight,
            })
         }
      }
      generator
   }
}

#[derive(Debug, Default)]
pub struct FactionData {
   id: usize,
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
   pub colour: Vector3<f32>,

   // Relationships
   enemies: Vec<usize>,
   allies: Vec<usize>,
   neutrals: Vec<usize>,

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
   ccolour: Vector4<f32>,
}
impl FactionData {
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
}

#[derive(Default)]
struct FactionSocial {
   enemies: Vec<usize>,
   allies: Vec<usize>,
   neutrals: Vec<usize>,
}
impl FactionSocial {
   pub fn new(fct: &FactionLoad, factions: &[FactionLoad]) -> Self {
      let mut social = FactionSocial::default();
      for name in &fct.enemies {
         if let Some(f) = FactionLoad::get(factions, name) {
            social.enemies.push(f)
         };
      }
      for name in &fct.allies {
         if let Some(f) = FactionLoad::get(factions, name) {
            social.allies.push(f)
         };
      }
      for name in &fct.neutrals {
         if let Some(f) = FactionLoad::get(factions, name) {
            social.neutrals.push(f)
         };
      }
      social
   }
}

#[derive(Debug, Default)]
struct FactionLoad {
   /// Base data
   data: FactionData,

   // Generators
   generator_name: Vec<String>,
   generator_weight: Vec<f32>,

   // Relationships
   enemies: Vec<String>,
   allies: Vec<String>,
   neutrals: Vec<String>,
}
impl FactionLoad {
   /// Loads the elementary faction stuff, does not fill out information dependent on other
   /// factions
   fn new<P: AsRef<Path>>(ctx: &ContextWrapper, lua: &NLua, filename: P) -> Result<Self> {
      let mut fctload = FactionLoad::default();
      let fct = &mut fctload.data;

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
               fctload.generator_name.push(nxml::node_string(node)?);
               fctload
                  .generator_weight
                  .push(match node.attribute("weight") {
                     Some(str) => str.parse::<f32>()?,
                     None => 1.0,
                  });
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

      Ok(fctload)
   }

   fn apply_social(&mut self, social: FactionSocial) {
      let fct = &mut self.data;
      fct.enemies = social.enemies;
      fct.allies = social.allies;
      fct.neutrals = social.neutrals;
   }

   fn into_data(self) -> FactionData {
      self.data
   }

   fn get(factions: &[FactionLoad], name: &str) -> Option<usize> {
      match binary_search_by_key_ref(factions, name, |fctload: &FactionLoad| &fctload.data.name) {
         Ok(id) => Some(id),
         Err(err) => {
            warn!("Faction '{name}' not found during loading!");
            None
         }
      }
   }
}

/// Static factions that are never modified after creation
pub static FACTIONDATA: OnceLock<Vec<FactionData>> = OnceLock::new();

pub fn load() -> Result<()> {
   let ctx = Context::get().as_safe_wrap();
   let base: PathBuf = "factions/".into();
   let files = ndata::read_dir_filter(&base, |filename| {
      filename.extension() == Some(OsStr::new("xml"))
   })?;

   // First pass: set up factions
   let mut factionload: Vec<FactionLoad> = files
      //.par_iter()
      .iter()
      .filter_map(|filename| match FactionLoad::new(&ctx, &NLUA, filename) {
         Ok(sp) => Some(sp),
         Err(e) => {
            warn!("Unable to load Faction '{}': {e}", filename.display());
            None
         }
      })
      .collect();
   // Add Player before sorting
   factionload.push(FactionLoad {
      data: FactionData {
         name: String::from(PLAYER_FACTION_NAME),
         cname: CString::new(PLAYER_FACTION_NAME)?,
         f_static: true,
         f_invisible: true,
         ..Default::default()
      },
      ..Default::default()
   });
   sort_by_key_ref(&mut factionload, |fctload: &FactionLoad| &fctload.data.name);

   // Second pass: set allies/enemies and generators
   let factionsocial: Vec<FactionSocial> = factionload
      //.par_iter()
      .iter()
      .map(|fct| FactionSocial::new(fct, &factionload))
      .collect();
   for (id, social) in factionsocial.into_iter().enumerate() {
      factionload[id].apply_social(social);
   }

   // Third pass: set faction generators
   let factiongenerator: Vec<Vec<Generator>> = factionload
      //.par_iter()
      .iter()
      .map(|fct| Generator::new(&factionload, &fct.generator_name, &fct.generator_weight))
      .collect();
   for (id, generator) in factiongenerator.into_iter().enumerate() {
      factionload[id].data.generators = generator;
      factionload[id].data.id = id; // Also set the ID here
   }

   // Convert to factions
   let factions: Vec<FactionData> = factionload
      .into_iter()
      .map(|fctload| fctload.into_data())
      .collect();

   // Save the data
   FACTIONDATA.set(factions).unwrap_or_else(|err| {
      warn!("unable to set factions");
   });
   match FactionRef::new(PLAYER_FACTION_NAME) {
      Some(id) => PLAYER.set(id).unwrap_or_else(|_| {
         warn!("unable to set player faction ID");
      }),
      None => unreachable!(),
   };

   // Populate the arena with the static factions
   let mut data = FACTIONS.write().unwrap();
   for fct in FACTIONDATA.get().unwrap() {
      data.insert(Faction {
         api: OnceLock::new(),
         standing: RwLock::new(Standing {
            player: fct.player_def,
            p_override: None,
            f_known: fct.f_known,
            f_invisible: fct.f_invisible,
         }),
         data: DataWrapper::Static(fct),
      });
   }

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

// Here be C API
use std::mem::ManuallyDrop;
use std::os::raw::{c_char, c_double, c_int};

#[unsafe(no_mangle)]
pub extern "C" fn _faction_isFaction(f: i64) -> c_int {
   if f < 0 {
      return 0;
   }
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
pub extern "C" fn _faction_mapname(id: i64) -> *const c_char {
   faction_c_call(id, |fct| {
      let ptr = match &fct.data.cmapname {
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
      &fct.data.ccolour as *const Vector4<f32> as *const naevc::glColour
   })
   .unwrap_or_else(|err| {
      warn_err!(err);
      std::ptr::null()
   })
}
