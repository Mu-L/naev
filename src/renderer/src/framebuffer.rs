use crate::texture::{AddressMode, FilterMode, Texture, TextureBuilder, TextureFormat};
use crate::{Context, ContextWrapper};
use anyhow::Result;
use glow::*;
#[allow(unused_imports)]
use mlua::{
   BorrowedStr, Either, FromLua, Lua, MetaMethod, UserData, UserDataMethods, UserDataRef,
   UserDataRefMut, Value,
};
use std::num::NonZero;

pub struct FramebufferC {
   fb: glow::NativeFramebuffer,
   w: usize,
   h: usize,
}
#[allow(clippy::large_enum_variant)]
pub enum FramebufferTarget {
   Screen,
   Framebuffer(Framebuffer),
   FramebufferC(FramebufferC),
}
impl FramebufferTarget {
   pub fn from_gl(fb: u32, w: usize, h: usize) -> Self {
      FramebufferTarget::FramebufferC(FramebufferC {
         fb: glow::NativeFramebuffer(NonZero::new(fb).unwrap()),
         w,
         h,
      })
   }

   pub fn dimensions(&self) -> (usize, usize) {
      match self {
         Self::Screen => todo!(),
         Self::Framebuffer(fb) => (fb.w, fb.h),
         Self::FramebufferC(fb) => (fb.w, fb.h),
      }
   }

   pub fn bind(&self, ctx: &Context) {
      let fb = match self {
         Self::Screen => None,
         Self::Framebuffer(fb) => Some(fb.framebuffer),
         Self::FramebufferC(fb) => Some(fb.fb),
      };
      unsafe {
         ctx.gl.bind_framebuffer(glow::FRAMEBUFFER, fb);
      }
   }

   pub fn unbind(&self, ctx: &Context) {
      unsafe {
         ctx.gl.bind_framebuffer(
            glow::FRAMEBUFFER,
            NonZero::new(naevc::gl_screen.current_fbo).map(glow::NativeFramebuffer),
         );
      }
   }
}

pub struct Framebuffer {
   pub framebuffer: glow::Framebuffer,
   pub w: usize,
   pub h: usize,
   pub texture: Option<Texture>,
   pub depth: Option<Texture>,
}
impl Drop for Framebuffer {
   fn drop(&mut self) {
      crate::message_push(crate::Message::DeleteFramebuffer(self.framebuffer));
   }
}
impl Framebuffer {
   pub fn bind(&self, ctx: &Context) {
      self.bind_gl(&ctx.gl)
   }

   pub fn bind_gl(&self, gl: &glow::Context) {
      unsafe {
         gl.bind_framebuffer(glow::FRAMEBUFFER, Some(self.framebuffer));
      }
   }

   pub fn unbind(ctx: &Context) {
      Framebuffer::unbind_gl(&ctx.gl)
   }
   pub fn unbind_gl(gl: &glow::Context) {
      unsafe {
         gl.bind_framebuffer(
            glow::FRAMEBUFFER,
            NonZero::new(naevc::gl_screen.current_fbo).map(glow::NativeFramebuffer),
         );
      }
   }

   pub fn into_texture(mut self) -> Result<Texture> {
      match self.texture.take() {
         Some(tex) => Ok(tex),
         None => anyhow::bail!("unable to remove texture from framebuffer"),
      }
   }
}

pub struct FramebufferBuilder {
   name: Option<String>,
   w: usize,
   h: usize,
   texture: bool,
   depth: bool,
   filter: FilterMode,
   address_mode: AddressMode,
}

impl FramebufferBuilder {
   pub fn new(name: Option<&str>) -> Self {
      FramebufferBuilder {
         name: name.map(String::from),
         w: 0,
         h: 0,
         texture: true,
         depth: false,
         filter: FilterMode::Linear,
         address_mode: AddressMode::ClampToEdge,
      }
   }

   pub fn width(mut self, width: usize) -> Self {
      self.w = width;
      self
   }

   pub fn height(mut self, height: usize) -> Self {
      self.h = height;
      self
   }

   pub fn texture(mut self, enable: bool) -> Self {
      self.texture = enable;
      self
   }

   pub fn depth(mut self, enable: bool) -> Self {
      self.depth = enable;
      self
   }

   pub fn filter(mut self, mode: FilterMode) -> Self {
      self.filter = mode;
      self
   }

   pub fn address_mode(mut self, mode: AddressMode) -> Self {
      self.address_mode = mode;
      self
   }

   pub fn name(mut self, name: Option<&str>) -> Self {
      self.name = name.map(String::from);
      self
   }

   pub fn build(self, ctx: &Context) -> Result<Framebuffer> {
      let wctx: ContextWrapper = ctx.into();
      self.build_wrap(&wctx)
   }

   pub fn build_wrap(self, ctx: &ContextWrapper) -> Result<Framebuffer> {
      let texture = if self.texture {
         let name = self.name.as_ref().map(|name| format!("{name}-Texture"));
         let texture = TextureBuilder::new()
            .name(name.as_deref())
            .width(Some(self.w))
            .height(Some(self.h))
            .filter(self.filter)
            .address_mode(self.address_mode)
            .build_wrap(ctx)?;
         Some(texture)
      } else {
         None
      };

      let depth = if self.depth {
         let name = self.name.as_ref().map(|name| format!("{name}-Depth"));
         let depth = TextureBuilder::new()
            .name(name.as_deref())
            .empty(TextureFormat::Depth)
            .width(Some(self.w))
            .height(Some(self.h))
            .filter(self.filter)
            .address_mode(self.address_mode)
            .build_wrap(ctx)?;
         Some(depth)
      } else {
         None
      };

      let lctx = ctx.lock();
      let gl = &lctx.gl;

      let framebuffer = unsafe { gl.create_framebuffer().map_err(|e| anyhow::anyhow!(e)) }?;
      unsafe {
         gl.bind_framebuffer(glow::FRAMEBUFFER, Some(framebuffer));
         if gl.supports_debug() {
            gl.object_label(glow::FRAMEBUFFER, framebuffer.0.into(), self.name);
         }
      }

      if let Some(ref texture) = texture {
         texture.bind_gl(gl, 0);
         unsafe {
            gl.framebuffer_texture_2d(
               glow::FRAMEBUFFER,
               glow::COLOR_ATTACHMENT0,
               glow::TEXTURE_2D,
               Some(texture.texture.texture),
               0,
            );
         }
      }

      if let Some(ref depth) = depth {
         depth.bind_gl(gl, 0);
         unsafe {
            gl.framebuffer_texture_2d(
               glow::FRAMEBUFFER,
               glow::DEPTH_ATTACHMENT,
               glow::TEXTURE_2D,
               Some(depth.texture.texture),
               0,
            );
         }
      };

      let status = unsafe { gl.check_framebuffer_status(glow::FRAMEBUFFER) };
      if status != glow::FRAMEBUFFER_COMPLETE {
         anyhow::bail!("error setting up framebuffer");
      }

      unsafe {
         Texture::unbind_gl(gl);
         gl.bind_framebuffer(
            glow::FRAMEBUFFER,
            NonZero::new(naevc::gl_screen.current_fbo).map(glow::NativeFramebuffer),
         );
      }

      Ok(Framebuffer {
         framebuffer,
         w: self.w,
         h: self.h,
         texture,
         depth,
      })
   }
}
