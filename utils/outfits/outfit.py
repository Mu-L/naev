# python

import xml.etree.ElementTree as ET
import sys
from os import path
script_dir = path.join(path.dirname(__file__), '..')
sys.path.append(path.realpath(script_dir))
from xml_name import xml_name as nam2fil


MOBILITY_PARAMS = {'speed', 'turn', 'accel', 'thrust'}
LOWER_BETTER = {'mass', 'price', 'delay', 'ew_range', 'falloff', 'trackmin', 'trackmax', 'dispersion', 'speed_dispersion', 'energy_regen_malus', 'ew_stealth', 'ew_stealth_timer', 'ew_signature', 'launch_lockon', 'launch_calibration', 'fwd_energy', 'tur_energy', 'ew_track', 'cooldown_time', 'cargo_inertia', 'land_delay', 'jump_delay', 'delay', 'reload_time', 'iflockon', 'jump_warmup', 'rumble', 'ammo_mass', 'time_mod', 'ew_hide', 'launch_reload'}

def shorten( s ):
   L = s.split(' ')
   while L != [] and L[0][1:2] == '.':
      L = L[1:]

   if L == []:
      return '???'
   elif L[0] == 'Beat':
      if L[2] == 'Medium':
         L[2] = 'Med.'
      return 'B. ' + L[2]
   else:
      return L[0]

def text2val( s ):
   try:
      inp = s.split('/', 1)
      inp = [float(x) for x in inp]
      return (inp[0], inp[-1])
   except:
      return None

def roundit( f ):
   f = round(f*2.0) / 2.0
   return int(f) if f == round(f) else f

def fmtval( v ):
   return str(roundit(v))

def andamp( s ):
   return '' if s is None else s.replace('&', '&amp;')

def fmt_kv( kv ):
   (key, value) = kv
   return key + '="' + str(andamp(value)) + '"'

def prisec( tag, r1, r2, eml1, eml2 ):
   a = 0 if r1 is None else r1[0]

   if r2 is not None:
      if tag in MOBILITY_PARAMS:
         a = (a*eml1+r2[1]*eml2)/(eml1+eml2)
      else:
         a += r2[1]

   return roundit(a)

def stackvals( tag, text1, text2, eml1, eml2 ):
   return str(roundit(prisec(tag, text2val(text1), text2val(text2), eml1, eml2)))

def r_prisec( tag, v1, v2, eml1, eml2 ):
   if tag in MOBILITY_PARAMS:
      return v1, round((v2*(eml1+eml2) - eml1*v1)/float(eml2))
   else:
      return v1, v2-v1

def unstackvals( tag, text1, text2, eml1, eml2 ):
   o1, o2 = r_prisec(tag, float(text1), 0 if text2 == '' else float(text2), eml1, eml2)
   if o2 == o1:
      return fmtval(o1)
   else:
      return fmtval(o1) + '/' + fmtval(o2)

def readval( what ):
   if what is None:
      what = ''
   if len(prisec := what.split('/')) <= 2:
      try:
         what = tuple(map(float, prisec))
         if len(what) == 1:
            what = what[0]
      except:
            pass
   return what

class _outfit():
   def __init__( self, fil, content = False ):
      self.pri = None

      if content:
         self.r = ET.fromstring(fil)
      elif type(fil) == type(''):
         with sys.stdin if fil == '-' else open(fil, 'rt') as fp:
            self.r = ET.parse(fp).getroot()
      else:
         self.r = ET.parse(fil).getroot()
      self.short = False
      self.fil = fil

   def name( self ):
      return self.r.attrib['name']

   def set_name( self, name ):
      self.r.attrib['name'] = name

   def find( self, tag, el = False ):
      for i in (e if el else e.text for e in self if e.tag == tag):
         return i

   def shortname( self ):
      if not self.short:
         if (res := self.find('shortname')) is None:
            res = self.name()
         if res.split(' ')[-1] == 'Engine':
            res = ' '.join(res.split(' ')[:-1])
         self.short = res
      return self.short

   def size( self, doubled = False ):
      res = self.find('size')
      for i, k in enumerate(['small', 'medium', 'large']):
         if res == k:
            return 2*i + (2 if doubled else 1)

   def can_pri_sec( self ):
      if self.pri is None:
         k = self.find('slot', True).attrib
         self.pri = 'prop' in k and k['prop'].find('secondary') == -1
         self.sec = 'prop_extra' in k and k['prop_extra'].find('secondary') != -1
         self.sec = self.sec or 'prop' in k and k['prop'].find('secondary') != -1
      return self.pri, self.sec

   can_pri = lambda self: self.can_pri_sec()[0]
   can_sec = lambda self: self.can_pri_sec()[1]

   def eml( self ):
      return readval(self.find('engine_limit'))

   def can_alone( self ):
      return self.name().find('Twin') == -1

   def can_stack( self, other ):
      return(
         (self.name() == other.name() and self.name().find('Twin') != -1) or
         (self.name().split(' ')[0] != 'Krain' and other.name().split(' ')[0] != 'Krain')
      )

   def stack( self, other = None ):
      if other is None:
         self.short = self.shortname() + ' x1'
      elif self.shortname() == other.shortname():
         self.short = self.shortname() + ' x2'
      else:
         self.short = shorten(self.shortname())+' + '+shorten(other.shortname())
      res = self.eml()
      if type(res) == type(()):
         (eml1, _) = res
      else:
         eml1 = res

      if other is None:
         eml2 = 0
         sec = {}
      else:
         eml2 = other.eml()
         if type(eml2) == type(()):
            (_, eml2) = eml2
         sec = other.to_dict()

      d = self.to_dict()
      e = self.find('specific', True)
      if e is not None:
         for missing in sec:
            if missing not in d:
               el = ET.Element(missing)
               el.text = ''
               e.append(el)

      for e in self:
         res = text2val(e.text)
         try:
            res2 = sec[e.tag]
            if type(res2) == type(''):
               res2 = None
            else:
               if type(res2) == type([]):
                  res2 = res2[0]
               if type(res2) != type(()):
                  res2 = (res2, res2)
         except:
            res2 = None

         if res is not None or res2 is not None:
            e.text = str(prisec(e.tag, res, res2, eml1, eml2))
      return self

   def autostack( self, doubled = False ):
      self.stack(self if doubled else None)

   def __iter__( self ):
      def _subs( r ):
         for e in r:
            yield e
            for s in _subs(e):
               yield s

      return iter(_subs(self.r))

   def write( self, dst = sys.stdout ):
      def output_r( e, fp, ind = 0 ):
         li = [e.tag] + [fmt_kv(x) for x in e.attrib.items()]

         try:
            iter(e).next()
            flag = True
         except:
            flag = False

         if e.text is None and not flag:
            fp.write(' '*ind+'<'+' '.join(li)+' />\n')
         else:
            fp.write(' '*ind+'<'+' '.join(li)+'>'+andamp(e.text).rstrip())
            fst = True
            for s in e:
               if fst:
                  fp.write('\n')
                  fst = False
               output_r(s, fp, ind+1)
            if not fst:
               fp.write(' '*ind)
            fp.write('</'+e.tag+'>\n')

      closeit = False
      if dst == '-':
         dest = sys.stdout
      elif type(dst) == type(''):
         dest = open(dst, 'w')
         closeit = True
      else:
         dest = dst

      output_r(self.r, dest)
      if closeit:
         dest.close()

   def to_dict( self ):
      d = dict()
      for k in self:
         if not k.tag in d:
            d[k.tag] = []
         what = readval(k.text)
         d[k.tag].append(what)
      for k in d:
         if len(d[k]) == 1:
            d[k] = d[k][0]
      return d

def outfit( fil, content = False ):
   if fil is None:
      return None

   if content or type(fil) != type('') or fil.endswith('.xml') or fil.endswith('.mvx') or fil == '-':
      o = _outfit(fil, content)
      if o.r.tag == 'outfit':
         return o

   if content:
      raise Exception('Invalid outfit <user-data content>')
   else:
      raise Exception('Invalid outfit "' + str(fil) + '"')
