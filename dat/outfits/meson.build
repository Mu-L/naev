# All generated outfits should be tossed in here so that the
# other scripts can be happy with it
gen_outfits = []

subdir('bioship')

customoutfits = {
  'neutralizer' : {
     'command': 'neutralizer.py',
     'input'  : 'weapons/heavy_ion_cannon.xml',
     'output' : 'neutralizer.xml',
   },
  'reaver' : {
     'command': 'reaver.py',
     'input'  : 'weapons/heavy_ripper_cannon.xml',
     'output' : 'reaver_cannon.xml',
   },
  'corsair_systems' : {
     'command': 'corsair_systems.py',
     'input'  : 'core_system/medium/unicorp_pt200_core_system.xml',
     'output' : 'corsair_systems.xml',
   },
  'corsair_hull' : {
     'command': 'corsair_hull.py',
     'input'  : 'core_hull/medium/nexus_ghost_weave.xml',
     'output' : 'corsair_hull_plating.xml',
   },
  'corsair_engine' : {
     'command': 'corsair_engine.py',
     'input'  : 'core_engine/medium/nexus_arrow_700_engine.xml',
     'output' : 'corsair_engine.xml',
   },
}

foreach name, data : customoutfits
  gen_outfits += [custom_target( name,
    command: [find_program( 'py' / data['command'] ), '@INPUT@', '@OUTPUT@', ],
    install: true,
    install_dir: ndata_path / 'dat/outfits',
    depend_files: 'py' / data['command'],
    input: data['input'],
    output: data['output'],
  )]
endforeach
