project(
    'SuiteSparse',
    'c',
    default_options : ['c_std=c11'],
    version: '7.3.1.beta1',
)

cc = meson.get_compiler('c')
c_args = cc.get_supported_arguments(['-Wno-implicit-const-int-float-conversion', '-Wno-implicit-fallthrough', '-Wno-maybe-uninitialized',
   '-Wno-parentheses', '-Wno-sign-compare', '-Wno-unused-but-set-variable', '-Wno-unused-const-variable', '-Wno-unused-variable',
   '-Wno-unused-function', '-Wno-unused-parameter', '-D_XOPEN_SOURCE=700'])
add_project_arguments(c_args, language: 'c')

# Note to maintainers: after fetching subprojects, you can: cd subprojects/SuiteSparse; git fetch --all --tags; git diff v7.3.0 '**/CMakeLists.txt'
# That should show when these defines should change.
config = configuration_data()
config.set('AMD_DATE', 'Sept 18, 2023')
config.set('AMD_VERSION_MAJOR', 3)
config.set('AMD_VERSION_MINOR', 2)
config.set('AMD_VERSION_SUB', 1)
config.set('CHOLMOD_DATE', 'Oct 31, 2023')
config.set('CHOLMOD_VERSION_MAJOR', 5)
config.set('CHOLMOD_VERSION_MINOR', 0)
config.set('CHOLMOD_VERSION_SUB', 1)
config.set('COLAMD_DATE', 'Sept 18, 2023')
config.set('COLAMD_VERSION_MAJOR', 3)
config.set('COLAMD_VERSION_MINOR', 2)
config.set('COLAMD_VERSION_SUB', 1)
config.set('CSPARSE_DATE', 'Sept 8, 2023')
config.set('CSPARSE_VERSION_MAJOR', 4)
config.set('CSPARSE_VERSION_MINOR', 2)
config.set('CSPARSE_VERSION_SUB', 0)
config.set ('FortranCInterface_GLOBAL__MACRO', '(name,NAME) name##_')
config.set ('FortranCInterface_GLOBAL_MACRO', '(name,NAME) name##_')
config.set('SuiteSparse_BLAS_integer', get_option('blas_integer'))
config.set('SUITESPARSE_DATE', 'Oct 10, 2024')
config.set('SUITESPARSE_VERSION_MAJOR', 7)
config.set('SUITESPARSE_VERSION_MINOR', 8)
config.set('SUITESPARSE_VERSION_SUB', 3)
config.set('SUITESPARSE_TIMER_ENABLED', 1)
config.set('SUITESPARSE_HAVE_CLOCK_GETTIME', 1)
config.set('SUITESPARSE_CONFIG_TIMER', 1)


subdir('SuiteSparse_config')
subdir('AMD')
subdir('COLAMD')
subdir('CSparse')
subdir('CHOLMOD')

SuiteSparse_dep = [SuiteSparse_config_dep, AMD_dep, COLAMD_dep, CSparse_dep, CHOLMOD_dep]
