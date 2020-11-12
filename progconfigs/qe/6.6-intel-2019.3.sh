#!/usr/bin/env bash

libxcdir="/opt/software/libxc/4.2.3/intel-2019.3"

./configure prefix="/opt/software/qe/6.6/intel-2019.3/build" \
    FC=ifort F90=ifort MPIF90=mpiifort CC=icc F77=ifort \
    FCFLAGS="-O2 -I$MKLROOT/include/fftw -I$MKLROOT/include" \
    CFLAGS="-O2 -I$MKLROOT/include/fftw -I$MKLROOT/include" \
    --with-libxc=yes --with-libxc-prefix=$libxcdir --with-libxc-include=$libxcdir/include \
    --with-scalapack="intel" \
    --with-hdf5="/opt/software/hdf5/1.12.0/intel-2019.3"
