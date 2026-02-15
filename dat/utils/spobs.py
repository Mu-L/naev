#!/bin/env python3

import sys
from os import path, scandir
script_dir = path.join(path.dirname(__file__), '..', '..', 'utils')
spob_dir = path.join(path.dirname(__file__), '..', 'spob')
sys.path.append(path.realpath(script_dir))
from naev_content import naev_xml

serv = set()
tags = set()

for f in scandir(spob_dir):
   if not f.is_file or f.name[-4:] != '.xml':
      continue
   i = path.join(spob_dir, f.name)
   sp = naev_xml(i)['spob']
   if 'tags' in sp and sp['tags']:
      if type(sp['tags']['tag']) == type (''):
         tags |= {sp['tags']['tag']}
      else:
         tags |= {i for i in sp['tags']['tag']}
   sp = sp['general']
   if 'services' in sp and sp['services']:
      serv |= {i for i in sp['services']}

print ('\n\n# Services\n')
for i in serv:
   print (' - **' + i + '**:')

print ('\n\n# Tags\n')
for i in tags:
   print (' - **' + i + '**:')
