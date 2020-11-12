#!/usr/bin/env bash

abinit_ver="8.10.3"

./configure --prefix="/opt/software/abinit/${abinit_ver}/intel-2019.3" \
    CXX=mpiicpc FC=mpiifort CC=mpiicc MPI_RUNNER=mpirun \
    --enable-64-bit-flags \
    --with-atompaw-bins="$ATOMPAW_HOME/bin" --with-atompaw-incs="-I$ATOMPAW_HOME/include" --with-atompaw-libs="$ATOMPAW_HOME/libatompaw.a" \
    --with-mpi-incs="-I$MKLROOT/../mpi/intel64/include" --with-mpi-libs="-L$MKLROOT/../mpi/intel64/lib -lmpi" \
    --with-linalg-flavor="mkl+scalapack" --with-linalg-incs=-I$MKLROOT/include/ \
    --with-linalg-libs="-L$MKLROOT/lib/intel64 -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_scalapack_lp64 -Wl,--start-group -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lmkl_blacs_intelmpi_lp64 -Wl,--end-group -lpthread -lm -ldl" \
    --enable-gw-dpc \
    --with-fft-flavor="dfti" --with-fft-libs="-L${MKLROOT}/lib/intel64 -Wl,--start-group  -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -Wl,--end-group" --with-fft-incs="-I$MKLROOT/include/" \
    --with-dft-flavor="atompaw+libxc+wannier90" --with-libxc-incs="-I$LIBXCDIR/include/" --with-libxc-libs="-L$LIBXCDIR/lib -lxcf90 -lxc" \
    --with-trio-flavor="netcdf" --with-netcdf-incs="-I$NETCDF_HOME/include/ -I$NETCDF_F_HOME/include/" --with-netcdf-libs="-L$NETCDF_HOME/lib -L$NETCDF_F_HOME/lib -lnetcdff -lnetcdf" \
    --with-wannier-libs="$WANNIER90_HOME/libwannier90.a" --with-wannier90-bins="$WANNIER90_HOME/" --with-wannier90-incs="-I$WANNIER90_HOME/src"

