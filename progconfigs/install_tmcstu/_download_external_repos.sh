#!/usr/bin/env bash
# TODO
# - [ ] vscode

if [[ ! -f /etc/os-release ]]; then
  echo "Error! Not a Fedora release"
  exit 1
fi

FEDORA_VERSION=$(awk -F = '/VERSION_ID/' /etc/os-release)

function get_gh_latest_release() {
  curl -sL "https://api.github.com/repos/$1/releases/latest" | grep '"tag_name":' | cut -d'"' -f4
}

function wget_link_source() {
  # $1: name of source
  # $2: URL link
  # $3: (optional) output file name of the downloaded source
  # returncode 0 if success, otherwise 1
  case $# in
    0|1 ) echo "Error! specify name and url"; exit 1;;
    2 ) name="$1"; url="$2"; output="";;
    * ) name="$1"; url="$2"; output="$3";;
  esac
  if [[ -n "$output" ]]; then
    wget_cmd="wget $url -O $output"
  else
    wget_cmd="wget $url"
  fi
  logname=tmcstd_pcws_wget.log
  echo "==wget==: try downloading $name with"
  echo "  $wget_cmd"
  if [[ -f "$output" ]]; then
    echo -e "Warning: $output found, overwrite? [y/N] "
    read -r ans
    if [[ "$ans" != "y" ]]; then
      echo "Skip download $name, post-processing ..."
      return 0
    fi
  fi
  if ($wget_cmd >> "$logname" 2>&1); then
    if [[ -n "$output" ]]; then
      echo "Success: $name downloaded as $3"
    else
      echo "Success: $name downloaded"
    fi
    echo "post-processing $name ..."
    return 0
  else
    echo "Error: $name not downloaded. See $logname for wget log."
    return 1
  fi
}

function _zotero() {
  # Zotero for bibliography
  if (wget_link_source "Zotero" \
      "https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64" \
      zotero.tar.bz2); then
    bunzip2 zotero.tar.bz2
    tar -xf zotero.tar
    cd Zotero_linux-x86_64 || return
    ./set_launcher_icon
    echo "You may need to link the desktop on your own, e.g."
    echo "  \$ mkdir -p ~/.local/share/applications"
    echo "  \$ ln -s \"$(realpath zotero.desktop)\" ~/.local/share/applications/zotero.desktop"
    cd ..
  fi
}

function _clash() {
  # clash appimage for get over GFW
  clash_ver=$(get_gh_latest_release Dreamacro/clash)
  echo "Getting clash, version: $clash_ver"
  if (wget_link_source "clash" \
      "https://github.com/Dreamacro/clash/releases/$clash_ver/clash-linux-amd64-$clash_ver.gz" \
      clash.gz); then
    gunzip clash.gz
    chmod +x clash
  fi
}

function _vesta() {
  # VESTA, require GTK3
  if (wget_link_source "VESTA" \
      "https://jp-minerals.org/vesta/archives/3.5.7/VESTA-gtk3.tar.bz2" \
      "VESTA-gtk3.tar.bz2"); then
    sudo dnf install -y gtk3 gtk3-devel
    bunzip2 VESTA-gtk3.tar.bz2
    tar -xf VESTA-gtk3.tar
  fi
}

function _xcrysden() {
  # Xcrysden
  if (wget_link_source "XcrysDen" \
      "http://www.xcrysden.org/download/xcrysden-1.6.2-linux_x86_64-shared.tar.gz" \
      "xcrysden-1.6.2-linux_x86_64-shared.tar.gz"); then
    # install xcrysden requirements
    sudo dnf install -y tk tk-devel tcl tcl-devel tcl-togl tcl-togl-devel openbabel openbabel-devel \
      fftw-libs libXmu libXmu-devel libX11-devel mesa-libGLU mesa-libGLU-devel ImageMagick
    tar -zxf xcrysden-1.6.2-linux_x86_64-shared.tar.gz
    mv xcrysden-1.6.2-bin-shared xcrysden-1.6.2
    # one needs to download 64-bit Togl 2.0 to make it work on Fedora > 30
    if (( FEDORA_VERSION >= 30 )); then
      if (wget_link_source "libTogl2" \
          "https://sourceforge.net/projects/togl/files/Togl/2.0/Togl2.0-8.4-Linux64.tar.gz" \
          "Togl2.0-8.4-Linux.tar.gz"); then
        tar -zxf Togl2.0-8.4-Linux.tar.gz
        sudo cp -n Togl2.0-8.4-Linux/lib/Togl2.0/libTogl2.0.so /usr/lib64/libTogl.so.2
      fi
    fi
  fi
}

function download_external_repos() {
  REPOS_DIR="repos"
  mkdir -p "$REPOS_DIR"
  cd "$REPOS_DIR" || exit 0
  ## TODO: check repos availabliblity
  _zotero
  _vesta
  _xcrysden
  cd ..
}

sudo dnf update
download_external_repos "$@"

