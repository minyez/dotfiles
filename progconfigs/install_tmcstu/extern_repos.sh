#!/usr/bin/env bash
# This file included th names and URLs of external repositories
# to download

repos_names=(
  "VESTA"
  "XCrySDen"
  "Zotero"
  "JabRef"
  # two lapack releases. The old 3.8.0 is hosted on netlib and should be easily accessible,
  # while the new 3.9.1 is hosted on GitHub and connection may fail.
  "lapack-3.8.0"
  "lapack-3.9.1"
  "scalapack"
  "fftw-3.3.9"
  "libxc-4.3.4"
  "libxc-5.1.3"
  "v_sim"
  "atat3-44"
  # placeholders for repos, without implemented URLs
  "hdf5"
  "netcdf"
)

declare -A repos_urls
repos_urls=(
  ["VESTA"]="https://jp-minerals.org/vesta/archives/3.5.7/VESTA-gtk3.tar.bz2"
  ["XCrySDen"]="http://www.xcrysden.org/download/xcrysden-1.6.2-linux_x86_64-shared.tar.gz"
  ["Zotero"]="https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64"
  ["JabRef"]="https://www.fosshub.com/JabRef.html?dwl=jabref-5.2-1.x86_64.rpm"
  ["lapack-3.8.0"]="http://www.netlib.org/lapack/lapack-3.8.0.tar.gz"
  ["lapack-3.9.1"]="https://github.com/Reference-LAPACK/lapack/archive/refs/tags/v3.9.1.tar.gz"
  ["scalapack"]="http://www.netlib.org/scalapack/scalapack-2.1.0.tgz"
  ["v_sim"]="https://gitlab.com/l_sim/v_sim/-/archive/3.8.0/v_sim-3.8.0.tar.gz"
  ["fftw-3.3.9"]="http://www.fftw.org/fftw-3.3.9.tar.gz"
  ["libxc-4.3.4"]="http://www.tddft.org/programs/libxc/down.php?file=4.3.4/libxc-4.3.4.tar.gz"
  ["libxc-5.1.3"]="http://www.tddft.org/programs/libxc/down.php?file=5.1.3/libxc-5.1.3.tar.gz"
  ["atat3-44"]="http://alum.mit.edu/www/avdw/atat/atat3_44.tar.gz"
)

# optional array to set the name of the downloaded file
# if not set, the basename of url will be used.
declare -A repos_outputs
repos_outputs=(
  ["Zotero"]="zotero.tar.bz2"
  ["JabRef"]="jabref-5.2-1.x86_64.rpm"
  ["scalapack"]="scalapack-2.1.0.tar.gz"
)

# directory to store downloaded repositories. No need to change, basically
REPOS_DIR="repos"

