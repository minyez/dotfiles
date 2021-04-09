#!/usr/bin/env bash

source _common.sh
source extern_repos.sh

function help() {
  echo "Download repositories from external online sources by using wget"
  echo ""
  echo "To add a new target, you only have to add the following to arrays in extern_repos.sh"
  echo ""
  echo "  1. repo name as an identifier to \`repos_names\`"
  echo "  2. repo URL to \`repos_urls\`"
  echo "  3. (optional) output path to \`repos_outputs\`"
  echo ""
  echo "Check existing examples before you want to write one."
  echo ""
  echo "Note that GitHub extracting may fail due to network problem"
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
    sudo dnf -y install gtk3 gtk3-devel
    bunzip2 VESTA-gtk3.tar.bz2
    tar -xf VESTA-gtk3.tar
  fi
}

function _xcrysden() {
  # Xcrysden
  if (wget_link_source "XCrySDen" \
      "http://www.xcrysden.org/download/xcrysden-1.6.2-linux_x86_64-shared.tar.gz" \
      "xcrysden-1.6.2-linux_x86_64-shared.tar.gz"); then
    # install xcrysden requirements
    sudo dnf -y install tk tk-devel tcl tcl-devel tcl-togl tcl-togl-devel openbabel openbabel-devel \
      fftw-libs libXmu libXmu-devel libX11-devel mesa-libGLU mesa-libGLU-devel ImageMagick
    tar -zxf xcrysden-1.6.2-linux_x86_64-shared.tar.gz
    mv xcrysden-1.6.2-bin-shared xcrysden-1.6.2
    # one needs to download 64-bit Togl 2.0 to make it work on Fedora > 30
    FEDORA_VERSION=$(get_fedora_ver)
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

function download_extern_repos() {
  for name in "${repos_names[@]}"; do
    unset url
    unset output
    url="${repos_urls[$name]}"
    output="${repos_outputs[$name]}"
    if [[ -z "$url" ]]; then
      echo "Warning: URL on external repo $name not set, skip"
      continue
    fi
    wget_repo "$REPOS_DIR" "$name" "$url" "$output"
  done
  #_zotero
  #_vesta
  #_xcrysden
}

if (( $# == 0 )); then
  help
  exit 0
fi

download_extern_repos "$@"

