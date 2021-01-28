#!/usr/bin/env bash
###########################################################
#   Copy this shell script to the path you want to install. 
#   For example /gpfs/share/software/compilers/gcc/6.4.0
#
# This script is brought down from PKU HPC

# adapt gnusrc to use a fast mirror for one's own case
#gnusrc="http://mirrors-usa.go-parts.com/releases"
gnusrc="https://mirrors.tuna.tsinghua.edu.cn/gnu/gcc"

# Function
breakIfFail(){
    if [ $? != 0 ];then
        exit 1
    fi
}

installGcc(){
    # param $1: install path of gcc
    # param $2: install version of gcc
    ins_path=$(cd $(dirname ${BASH_SOURCE}) && pwd -P)
    version=6.4.0
    if [ -n "$1" ];then
        ins_path=$1
    fi
    if [ -n "$2" ];then
        version=$2
    fi
    if [ ! -d "${ins_path}" ];then
        mkdir -p ${ins_path}
    fi
    cd ${ins_path}
    if [ ! -d "${ins_path}/gcc-${version}" ]; then
        if [ ! -f "${ins_path}/gcc-${version}.tar.gz" ];then
            wget $gnusrc/gcc-${version}/gcc-${version}.tar.gz
            breakIfFail
        fi
        tar xf gcc-${version}.tar.gz
        rm -f gcc-${version}.tar.gz
    fi
    cd ${ins_path}/gcc-${version}
    ./contrib/download_prerequisites
    breakIfFail
    ./configure --prefix=${ins_path} --enable-languages=c,c++,fortran --build=x86_64-linux --disable-multilib
    make -j $(nproc) && make install
    breakIfFail
    export PATH=${ins_path}/bin:$PATH
    export LD_LIBRARY_PATH=${ins_path}/lib:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=${ins_path}/lib64:$LD_LIBRARY_PATH
    export LIBRARY_PATH=${ins_path}/lib:$LIBRARY_PATH
    export LIBRARY_PATH=${ins_path}/lib64:$LIBRARY_PATH
    export CPATH=${ins_path}/include:$CPATH
}

#installOpenmpi(){
#    # param $1: install path of openmpi
#    # param $2: install version of openmpi
#    ins_path=$(cd $(dirname ${BASH_SOURCE}) && pwd -P)
#    version=3.1.3
#    if [ -n "$1" ];then
#        ins_path=$1
#    fi
#    if [ -n "$2" ];then
#        version=$2
#    fi
#    if [ ! -d "${ins_path}" ];then
#        mkdir -p ${ins_path}
#    fi
#    cd ${ins_path}
#    if [ ! -d "${ins_path}/openmpi-${version}" ]; then
#        if [ ! -f "${ins_path}/openmpi-${version}.tar.gz" ];then
#            wget https://download.open-mpi.org/release/open-mpi/v${version:0:3}/openmpi-${version}.tar.gz
#            breakIfFail
#        fi
#        tar xf openmpi-${version}.tar.gz
#        rm -f openmpi-${version}.tar.gz
#        fi
#    cd openmpi-${version}
#    ./configure --prefix=${ins_path}
#    make -j $(nproc) && make install
#    breakIfFail
#    export PATH=${ins_path}/bin:$PATH
#    export LD_LIBRARY_PATH=${ins_path}/lib:$LD_LIBRARY_PATH
#    export LIBRARY_PATH=${ins_path}/lib:$LIBRARY_PATH
#    export CPATH=${ins_path}/include:$CPATH
#}
#
#installCmake(){
#    # param $1: install path of cmake
#    # param $2: install version of cmake
#    ins_path=$(cd $(dirname ${BASH_SOURCE}) && pwd -P)
#    version=3.13.1
#    if [ -n "$1" ];then
#        ins_path=$1
#    fi
#    if [ -n "$2" ];then
#        version=$2
#    fi
#    if [ ! -d "${ins_path}" ];then
#        mkdir -p ${ins_path}
#    fi
#    cd ${ins_path}
#    if [ ! -d "${ins_path}/cmake-${version}" ]; then
#        if [ ! -f "${ins_path}/cmake-${version}.tar.gz" ];then
#            wget https://github.com/Kitware/CMake/releases/download/v${version}/cmake-${version}.tar.gz
#            breakIfFail
#        fi
#        tar xf cmake-${version}.tar.gz
#        rm -f cmake-${version}.tar.gz
#        fi
#    cd cmake-${version}
#    ./configure --prefix=${ins_path}
#    make -j $(nproc) && make install
#    breakIfFail
#    export PATH=${ins_path}/bin:$PATH
#}
#
#moudelFileHead(){
#    # param $1: modulefile name
#    # param $2: project name
#    # param $3: project version
#    module_file_name=$1;project=$2;project_ver=$3
#cat << EOF > ${module_file_name}
##%Module########################################################################
###
### ${project}@${project_ver}  modulefile
###
#proc ModulesHelp { } {
#
#    puts stderr "    This module defines environment variables, aliases and add PATH, LD_LIBRARY_PATH for ${project}"
#    puts stderr "    Version ${project_ver}"
#                      }
#
#module-whatis   "${project}@${project_ver}"
#EOF
#}

modulefileAddEnv(){
    # Write modulefile 
    # param $1: modulefile name
    # param $2: topdir
    # param $3...: env type,include bin,lib,lib64,include
    module_file_name=$1;topdir=$2;env=${*:3}
    declare -A array
    for constant in ${env}
    do
        array[$constant]=1
    done
    if [[ ${array[bin]} ]];then 
        echo "prepend-path    PATH                    \"${topdir}/bin\"" >> ${module_file_name}
    fi
    if [[ ${array[include]} ]];then 
        echo "prepend-path    CPATH                   \"${topdir}/include\"" >> ${module_file_name}
    fi
    if [[ ${array[lib]} ]]; then 
        echo "prepend-path    LIBRARY_PATH            \"${topdir}/lib\"" >> ${module_file_name}
        echo "prepend-path    LD_LIBRARY_PATH         \"${topdir}/lib\"" >> ${module_file_name}
    fi
    if [[ ${array[lib64]} ]]; then 
        echo "prepend-path    LIBRARY_PATH            \"${topdir}/lib64\"" >> ${module_file_name}
        echo "prepend-path    LD_LIBRARY_PATH         \"${topdir}/lib64\"" >> ${module_file_name}
    fi
}

### Main 
INS_PATH=$(cd $(dirname ${BASH_SOURCE:-$0}) && pwd -P)
Project="GCC"
ProjectVer="5.5.0"
GccVer="5.5.0"
moduleFileName="5.5.0"
installGcc ${INS_PATH} ${GccVer}
cd ${INS_PATH}
#moudelFileHead      ${moduleFileName} ${Project} ${ProjectVer}
#modulefileAddEnv    ${moduleFileName} ${INS_PATH} bin include lib lib64
