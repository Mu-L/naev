# python3

from os import path, utime
from sys import stderr
from pathlib import Path
from outfit import outfit

from xmllua2mvx import xmllua2mvx
from mvx2xmllua import mvx2xmllua

def mvx_nam(xml):
   return path.join(path.dirname(xml), '.' + path.basename(xml)[:-3] + 'mvx')

def _gen_if_needed( xml, force = False, quiet = False, multicore_only = False ):
   mvx = mvx_nam(xml)
   exists = (not force) and Path(mvx).is_file()
   if exists and not path.getmtime(mvx) < path.getmtime(xml):
      return outfit(mvx)
   else:
      return xmllua2mvx(xml, mvx, quiet, multicore_only)

def core_outfit( nam, try_again = False, quiet = False ):
   if nam.endswith('.xml'):
      try:
         o = _gen_if_needed(nam, False, quiet)
      except:
         o = None
      if o is None and try_again:
         o = _gen_if_needed(nam, True, quiet)
      if o is not None:
         o.fil = nam
      return o

def some_outfit( nam, quiet = False ):
   return _gen_if_needed(nam, force = False, quiet = quiet, multicore_only = True)

def core_write( o, fil ):
   mvx = mvx_nam(fil)
   o.write(mvx)
   mvx2xmllua(mvx, fil, quiet = True)
   with open(mvx, 'ab'):   # mark mvx as up to date
      utime(mvx, None)
