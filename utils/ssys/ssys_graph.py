#!/usr/bin/env python3


import os
from sys import stderr, exit
import xml.etree.ElementTree as ET
from ssys import nam2base, getpath, PATH


default_col = (0.25, 0.25, 0.25)
faction_color = {
   None:                default_col,
   'empire':            (0.0,  0.85, 0.0),
   'zalek':             (0.6,  0.0,  0.0),
   'dvaered':           (0.6,  0.2,  0.0),
   'sirius':            (0.0,  0.7,  0.8),
   'soromid':           (0.95, 0.5,  0.0),
   'frontier':          (0.8,  0.8,  0.0),
   'pirate':            (1.0,  0.0,  0.0),
   'independent':       (0.0,  0.0,  1.0),
   'proteron':          (0.9,  0.0,  0.9),
   'thurion':           (0.5,  0.5,  0.5),
   'collective':        (0.8,  0.8,  0.8),
   'goddard':           (0.0,  0.1,  1.0),
   'traders_society':   (0.0,  0.1,  1.0),
   'orez':              (0.0,  0.4,  0.9),
   'flf':               (0.9,  0.9,  0.0),
}

for f in ['wild_ones', 'raven_clan', 'dreamer_clan', 'black_lotus', 'lost', 'marauder']:
   faction_color[f] = faction_color['pirate']

def get_spob_faction( nam ):
   T = ET.parse(getpath(PATH, "spob", nam + ".xml")).getroot()
   e = T.find("./presence/faction")
   return None if e is None else nam2base(e.text)

def all_ssys( args = None ):
   def gen():
      if args is None or args == []:
         path = os.path.join(PATH, 'ssys')
         for arg in os.listdir(path):
            yield arg, os.path.join(path, arg)
      else:
         for a in args:
            yield os.path.basename(a), a
   for a, b in gen():
      if a[-4:] == '.xml':
         yield a[:-4], b

# Vnames, Vpos, E, tradelane, color = xml_files_to_graph(args, use_color)
def xml_files_to_graph( args = None, get_colors = False ):
   name2id = dict()
   name, acc, pos, tradelane, color = [], [], [], set(), dict()
   for bname, filename in all_ssys(args):
      name.append(bname)
      T=ET.parse(filename).getroot()

      try:
         name[-1] = T.attrib['name']
      except:
         stderr.write('no name defined in "' + bname + '"\n')

      try:
         e = T.find('pos')
         pos.append((bname, (e.attrib['x'], e.attrib['y'])))
      except:
         stderr.write('no position defined in "' + bname + '"\n')

      for e in T.findall('tags/tag'):
         if e.text == 'tradelane':
            tradelane.add(bname)
            break

      if get_colors:
         fact = dict()
         for e in T.findall('spobs/spob'):
            if f := get_spob_faction(nam2base(e.text)):
               if f not in fact:
                  fact[f] = 0
               fact[f] += 1

         fact = list(sorted([(n,f) for (f,n) in fact.items()], reverse = True))
         if len(fact)>1 and fact[0][1]=='independent' and fact[0][0] == fact[1][0]:
            fact = fact[1]
         else:
            fact = (fact+[(None,None)])[0]
         if (fact:= fact[1]) not in faction_color:
            fact = None
         color[bname] = faction_color[fact]

      name2id[name[-1]] = bname
      acc.append([])
      count = 1
      for e in T.findall('./jumps/jump'):
         try:
            acc[-1].append((e.attrib['target'], e.find('hidden') is not None))
         except:
            stderr.write('no target defined in "'+filename+'"jump#'+str(count)+'\n')
      count += 1

   ids = [name2id[x] for x in name]
   acc = [[(name2id[t[0]],t[1]) for t in L] for L in acc]
   return dict(zip(ids,name)), dict(pos), dict(zip(ids,acc)), tradelane, color


if __name__ == '__main__':
   from sys import argv, exit, stdout, stderr, stdin

   def do_reading(args):
      for bname, filename in all_ssys(args):
         T = ET.parse(filename).getroot()
         """
         try:
            name = T.attrib['name']
         except:
            stderr.write('no name defined in "' + bname + '"\n')
         """
         try:
            e = T.find('pos')
            x, y = (e.attrib['x'], e.attrib['y'])
         except:
            stderr.write('no position defined in "' + bname + '"\n')
         #print(bname, x, y, name)
         print(bname, x, y)

   def _read_stdin_and_scale(scale):
      for l in stdin:
         if (line := l.strip()) != '':
            (n, x, y, r) = (l.strip().split(' ',4) + [''])[:4]
            x = str(float(x) * scale)
            y = str(float(y) * scale)
            yield n, x, y, r

   def do_writing( scale = 1.0 ):
      from ssys import fil_ET
      for n, x, y, _ in _read_stdin_and_scale(scale):
         name = os.path.join(PATH, 'ssys', n + '.xml')
         T = fil_ET(name)
         e = T.getroot().find('pos')
         e.attrib['x'], e.attrib['y'] = x, y
         T.write(name)

   def do_scaling( scale = 1.0 ):
      for t in _read_stdin_and_scale(scale):
         print(' '.join(t).strip())

   if do_write := ('-w' in argv[1:]):
      argv.remove('-w')
      scale = 1.0

   if do_scale := ('-s' in argv[1:]):
      index = argv.index('-s')
      argv.pop(index)
      try:
         scale = float(argv.pop(index))
      except:
         stderr.write('Error: expected <scale> after -s.\n')
         exit(1)

   help_f = '-h' in argv or '--help' in argv[1:]
   if help_f or (argv[1:] != [] and do_write):
      msg = lambda s: (stdout if help_f else stderr).write(s + '\n')
      msg('usage:  ' + os.path.basename(argv[0]) + ' (-s <scale> [-w]) | -w | [<files>..]')
      msg('  Lists (ssys, x, y, name) for all ssys in <files.xml>')
      msg('  If <files.xml> not provided, uses dat/ssys/*.xml.')
      msg('  If -s is set, reads (ssys, x, y, ...) on stdin, rescales it')
      msg('  with <scale> and, unless -w is set, outputs the result on stdout.')
      msg('  If -w is set, reads (ssys, x, y, ...) on stdin and update dat/ssys.')
      exit(0 if ok else 1)

   if do_write:
      do_writing(scale)
   elif do_scale:
      do_scaling(scale)
   else:
      do_reading(argv[1:])
