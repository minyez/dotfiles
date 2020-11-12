#!/usr/bin/env bash
LIBXCDIR="/opt/software/libxc/3.0.1/intel-2019.3"
HDF5DIR="/opt/software/hdf5/1.12.0/intel-2019.3"
FFTWDIR="/opt/math/fftw3/3.3.8/intel-2019.3"
#LAPACKDIR="/opt/math/lapack/3.9.0/intel-2019.3"

# do not add prefix
# fpp options are required
./configure \
  --enable-dp \
  FC=ifort MPIFC=mpiifort CC=icc FPP="fpp -e -free"  \
  --enable-netcdf-hdf5 \
  --enable-hdf5-compression \
  --with-blas-libs="-lmkl_intel_lp64  -lmkl_sequential -lmkl_core" \
  --with-lapack-libs="-lmkl_intel_lp64  -lmkl_sequential -lmkl_core" \
  --with-blacs-libs="-lmkl_scalapack_lp64 -lmkl_blacs_intelmpi_lp64" \
  --with-fft-path="$FFTWDIR" \
  --with-fft-includedir="-I$FFTWDIR/include" \
  --with-fft-libs="$FFTWDIR/lib/libfftw3_mpi.a $FFTWDIR/lib/libfftw3.a" \
  --with-netcdf-libs="${NETCDF_F_HOME}/lib/libnetcdff.a ${NETCDF_HOME}/lib/libnetcdf.a" \
  --with-netcdf-includedir="-I${NETCDF_F_HOME}/include -I${NETCDF_HOME}/include/" \
  --with-hdf5-libs="${HDF5DIR}/lib/libhdf5hl_fortran.a ${HDF5DIR}/lib/libhdf5_fortran.a ${HDF5DIR}/lib/libhdf5_hl.a ${HDF5DIR}/lib/libhdf5.a" \
  --with-hdf5-includedir="-I${HDF5DIR}/include" \
  --with-scalapack-libs="${MKLROOT}/lib/intel64/libmkl_scalapack_lp64.a -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_lp64.a ${MKLROOT}/lib/intel64/libmkl_sequential.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_blacs_intelmpi_lp64.a -Wl,--end-group -lpthread -lm -ldl" \
  --with-libxc-path="${LIBXCDIR}" \
  --with-libxc-libdir="-L${LIBXCDIR}/lib/" \
  --with-libxc-libs="-lxcf90 -lxc" \
  --with-libxc-includedir="-I${LIBXCDIR}/include"

