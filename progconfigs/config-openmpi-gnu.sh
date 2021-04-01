#!/usr/bin/env bash

# 2018-01-04 15:20
./configure --prefix=$HOME/software/compiler/openmpi/3.0.0/gnu/8.2.0 CC=gcc CPP=cpp CXX=g++ FC=gfortran --enable-mpi-cxx --enable-mpi-fortran --enable-static

# make
# make install
# make clean # to save disk space
