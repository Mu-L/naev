subdir('glcheck')

test('main_menu',
    find_program('watch-for-msg.py'),
    args: [
        naev_py,
        'Reached main menu'
    ],
    env: ['WITHGDB=NO'],
    workdir: meson.project_source_root(),
    protocol: 'exitcode'
    )

if (ascli_exe.found())
    metainfo_test_file = 'org.naev.Naev.metainfo.xml'
    test('validate_metainfo',
    ascli_exe,
    args: [
        'validate',
        '--no-net',
        '--pedantic',
        '--verbose',
        metainfo_test_file
    ],
    workdir: meson.project_source_root()
    )
endif

if (luacheck.found())
    env = environment()
    env.set( 'LUA_PATH', meson.project_source_root() / '?.lua;;' )
    test('luacheck',
        nluacheck,
        args: ['dat', 'docs', 'extras', 'utils', '--no-color'],
        env: env,
        workdir: meson.project_source_root(),
        timeout: 150,
        )
endif

# If this test fails, someone put a standard constant like PRIu64 in a translatable string.
# We can't support that (and you don't want to know how GNU gettext does it), but %.0f is typically good enough.
test('search_for_unsupported_sysdep_strings',
   find_program('grep'),
   args: ['-q', '%<PRI', join_paths(meson.project_source_root(), 'po', 'naev.pot')],
   should_fail: true,
   )


# Run all our Rust tests (if we have them)
test('cargo test --workspace',
  find_program('cargo'),
  args: ['test', '--workspace'],
  workdir: meson.project_build_root(),
  )
