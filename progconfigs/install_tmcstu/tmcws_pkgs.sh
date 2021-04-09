#!/usr/bin/env bash
# This file included th names and URLs of packages/files
# on TMCWS for retrieving and installation

# TMCWS ssh connection, for retriving packages from TMC workstation
TMCWS_CONNECTION="zhangmy@222.29.156.110"
# you can add port like this
#TMCWS_CONNECTION="-p 2022 zhangmy@222.29.156.110"

# options for rsync command
# note:
#  1. you must first ensure that connecting to tmcws is password-free. This will be checked by ssh_connection_check
#  2. info=progress2 to give a summary of current speed and progress
#  3. Caveat: may break if TMCWS is behind some route such that a port number may be required
#     A workaround: add `-p port ` option in rsync or before the account
rsync_opts="--exclude=*.[ox] --exclude=*.mod  --exclude=*.out  --exclude=*.pyc --exclude=vasp --exclude=vasp_* -azru --info=progress2 "
# 

pkgs_names=(
  # VASP
  "vasp-5.4.4"
  "vasppot-5.4"
  # WIEN2k
  "wien2k-v19.2"
  #"wien2k-v16.1"
  # Intel
  "intel_xe_2019_update3"
  "intel_xe_2020_update4.tgz"
  # Intel licenses (registered under MY Zhang, you may need to refresh with your own one)
  "intel_licenses"
  # Gaussian09 e1
  "g09e1"
  # VMD
  "vmd-1.9.4a51"
  # direct download from VMD website is not possible due to the request of filling a form
  # we retrieve it from the TMCWS
)

# URL can either be the path to the root directory of the pacakge (!!!must end with a backslash!!!)
#         or a path to single file.
# using -a may raise syntax error: invalid arithmetic operator for names having dots
declare -A pkgs_urls
pkgs_urls=(
  ["vasp-5.4.4"]="/opt/software/vasp/5.4.4-16052018-patched/intel/2019.3/"
  ["vasppot-5.4"]="/opt/software/vasp/vasppot-5.4/"
  ["wien2k-v19.2"]="/home/jiangh/WIEN2k_19.2.tar"
  #["wien2k-v16.1"]="/opt/software/wien2k/16.1/intel-2019.3/"
  #["intel_xe_2019_update3"]="/opt/compiler/intel/2019.3/"
  ["intel_xe_2020_update4.tgz"]="/opt/compiler/intel/parallel_studio_xe_2020_update4_cluster_edition.tgz"
  ["intel_licenses"]="/opt/intel/licenses/"
  ["g09e1"]="/opt/software/g09e01/"
  ["vmd-1.9.4a51"]="/opt/tool/vmd-1.9.4a51.LINUXAMD64.tar.gz"
)

declare -A pkgs_outputs
pkgs_outputs=()

# directory to store retrieved packages. No need to change, basically
PKGS_DIR="pkgs"

