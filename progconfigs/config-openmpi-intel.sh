#!/usr/bin/env bash

# 2018-01-04 15:20
./configure --prefix=$HOME/software/compiler/openmpi/3.0.0/intel/18.0.1/ CC=icc CXX=icpc FC=ifort --enable-mpi-cxx --enable-mpi-fortran --enable-static

# make
# make install
# make clean # to save disk space
