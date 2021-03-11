"""User provided customizations.

Here one changes the default arguments for compiling _gpaw.so.

Here are all the lists that can be modified:

* libraries
  List of libraries to link: -l<lib1> -l<lib2> ...
* library_dirs
  Library search directories: -L<dir1> -L<dir2> ...
* include_dirs
  Header search directories: -I<dir1> -I<dir2> ...
* extra_link_args
  Arguments forwarded directly to linker
* extra_compile_args
  Arguments forwarded directly to compiler
* runtime_library_dirs
  Runtime library search directories: -Wl,-rpath=<dir1> -Wl,-rpath=<dir2> ...
* extra_objects
* define_macros

The following lists work like above, but are only linked when compiling
the parallel interpreter:

* mpi_libraries
* mpi_library_dirs
* mpi_include_dirs
* mpi_runtime_library_dirs
* mpi_define_macros

To override use the form:

    libraries = ['somelib', 'otherlib']

To append use the form

    libraries += ['somelib', 'otherlib']
"""

import numpy as np

# flake8: noqa

compiler = 'mpiicc -fPIC'
mpicompiler = 'mpiicc -fPIC'
mpilinker = mpicompiler
# platform_id = ''

FFTW3_HOME = '/home/stevezhang/software/intel-2018-update-1/compilers_and_libraries_2018.1.163/linux/mkl/'
MKLROOT    = '/home/stevezhang/software/intel-2018-update-1/compilers_and_libraries_2018.1.163/linux/mkl/'
LIBXC_HOME = '/home/stevezhang/quantum-codes/libxc-4.2.1-intel-2018.0.1'
#ELPA_HOME = '/home/stevezhang/software/elpa-2017.11.001-intel-2018.1'
#ELPA_VERSION = 'elpa-2017.11.001'

libraries = [
              'mkl_scalapack_lp64',
              'mkl_intel_lp64', 'mkl_def', 'mkl_sequential' ,'mkl_core',
              'mkl_blacs_intelmpi_lp64',
              'pthread','m','dl',
              'fftw3xc_intel',
            ]
extra_link_args += ['-Wl,-rpath={}/lib/intel64/'.format(MKLROOT)]
mpi_libraries = []

library_dirs = [MKLROOT+'/lib/intel64/', '/home/stevezhang/intel/interfaces/fftw3xc']
# include numpy header to use array object
include_dirs += [np.get_include(), MKLROOT+'/include/', FFTW3_HOME+'/include/fftw']

## FFTW3:
#fftw = False
#if fftw:
#    libraries += ['fftw3']

# ScaLAPACK (version 2.0.1+ required):
scalapack = True
if scalapack:
    define_macros += [('GPAW_NO_UNDERSCORE_CBLACS', '1')]
    define_macros += [('GPAW_NO_UNDERSCORE_CSCALAPACK', '1')]

## Use Elpa (requires ScaLAPACK and Elpa API 20171201):
#if 0:
#    elpa = True
#    elpadir = '/home/user/elpa'
#    libraries += ['elpa']
#    library_dirs += ['{}/lib'.format(elpadir)]
#    extra_link_args += ['-Wl,-rpath={}/lib'.format(elpadir)]
#    include_dirs += ['{}/include/elpa-xxxx.xx.xxx'.format(elpadir)]

# LibXC:
# In order to link libxc installed in a non-standard location
# (e.g.: configure --prefix=/home/user/libxc-2.0.1-1), use:

# - static linking:
# - dynamic linking (requires rpath or setting LD_LIBRARY_PATH at runtime):
include_dirs += [LIBXC_HOME + '/include']
library_dirs += [LIBXC_HOME + '/lib']
# You can use rpath to avoid changing LD_LIBRARY_PATH:
extra_link_args += ['-Wl,-rpath={xc}/lib'.format(xc=LIBXC_HOME)]
if 'xc' not in libraries:
    libraries.append('xc')


## libvdwxc:
#if 0:
#    libvdwxc = True
#    path = '/home/user/libvdwxc'
#    extra_link_args += ['-Wl,-rpath=%s/lib' % path]
#    library_dirs += ['%s/lib' % path]
#    include_dirs += ['%s/include' % path]
#    libraries += ['vdwxc']
