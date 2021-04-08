#!/usr/bin/env bash
# This file included th names and URLs of packages/files
# on TMCWS for retrieving and installation

pkgs_names=(
  # VASP
  "vasp-5.4.4" "vasppot-5.4"
  # WIEN2k
  "wien2k-v19.2" "wien2k-v16.1"
  # Intel 2020 Update 4
  "parallel_studio_xe_2020_update4.tgz"
  # Intel licenses (registered under MY Zhang, you may need to refresh with your own one)
  "intel_licenses"
  # Gaussian09 e1
  "g09e1"
)

declare -A pkgs_urls
pkgs_urls=(
  # VASP
  ["vasp-5.4.4"]="/opt/software/vasp/5.4.4-16052018-patched/intel/2019.3/"
  ["vasppot-5.4"]="/opt/software/vasp/vasppot-5.4/"
  # WIEN2k
  ["wien2k-v19.2"]="/opt/software/wien2k/19.2/intel/2019.3/"
  ["wien2k-v16.1"]="/opt/software/wien2k/16.1/intel/2019.3/"
  # Intel 2020 Update 4
  ["parallel_studio_xe_2020_update4.tgz"]="/opt/compiler/intel/parallel_studio_xe_2020_update4_cluster_edition.tgz"
  # Intel licenses (registered under MY Zhang, you may need to refresh with your own one)
  ["intel_licenses"]="/opt/intel/licenses/"
  # Gaussian09 e1
  ["g09e1"]="/opt/software/g09e01/"
)

declare -A pkgs_urls

export pkgs_names pkgs_urls
