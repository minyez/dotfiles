#!/usr/bin/env bash

function help () {
  echo "$1: A script to initialize a TMC student PC workstation for daily usage"
  echo ""
  echo "Notes: "
  echo "  1. you must run the script with sudo"
  echo "  2. to retrieve scientific packages, you need a password-free connection to your TMC workstation account"
  echo "     Currently try will retrieve:"
  echo "       VASP   (5.4.4 with patch)"
  echo "       WIEN2k (v16.1, v19.2)"
  echo ""
  echo "Options:"
  echo "  [ -h | --help     ] : this help info"
  echo "  [ -s | --start    ] : start the initialization"
  echo "  [ -r | --retrieve ] : retrieve sci. packages on TMCWS"
  echo ""
  echo "Update: 2021-04-07"
  echo ""
  echo "Contributors: MY Zhang"
}

## if you want to install emacs, new-version git or Doom
## just uncomment out the corresponding variable
#EMACS_VERSION="27.1.91"
#GIT_VERSION="2.31.1"
#INSTALL_DOOM=YES
REPO_SOURCES="THU"
# TMCWS account for retriving scientific packages from TMC workstation
TMCWS_ACCOUNT="zhangmy"
TMCWS_IP="222.29.156.110"

if [[ ! -f /etc/os-release ]]; then
  echo "Error! Not a Fedora release"
  echo ""
  help "$0"
  exit 1
fi

FEDORA_VERSION=$(awk -F = '/VERSION_ID/' /etc/os-release)
FEDORA_VERSION_MIN=29

function ssh_connection_check () {
  ssh -q -o BatchMode=yes  -o StrictHostKeyChecking=no -o ConnectTimeout=5 "$1" 'exit 0'
  return "$?"
}

function get_latest_release () {
  curl -sL "https://api.github.com/repos/$1/releases/latest" | grep '"tag_name":' | cut -d'"' -f4
}

function backup_fedora_repo () {
  [[ -f /etc/yum/repos.d/$1.repo ]] && \
    cp "/etc/yum/repos.d/$1.repo" "/etc/yum/repos.d/$1.repo_backup_$(date +"%y%m%d")"
}

function replace_repo_sources () {
  # add repository sources to make full use of network
  # see https://mirrors.ustc.edu.cn
  #     https://mirrors.tuna.tsinghua.edu.cn
  #     https://mirrors.pku.edu.cn
  if [[ "$REPO_SOURCES" == "THU" ]]; then
    echo "Replacing with TUNA mirror"
    backup_fedora_repo fedora
    backup_fedora_repo fedora-updates
    backup_fedora_repo fedora-modular
    backup_fedora_repo fedora-updates-modular
    cat > /etc/yum/repos.d/fedora.repo << EOF
[fedora]
name=Fedora \$releasever - \$basearch
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/releases/\$releasever/Everything/\$basearch/os/
metadata_expire=28d
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
    cat > /etc/yum/repos.d/fedora-updates.repo << EOF
[updates]
name=Fedora \$releasever - \$basearch - Updates
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/updates/\$releasever/Everything/\$basearch/
enabled=1
gpgcheck=1
metadata_expire=6h
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
    cat > /etc/yum/repos.d/fedora-modular.repo << EOF
[fedora-modular]
name=Fedora Modular \$releasever - \$basearch
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/releases/\$releasever/Modular/\$basearch/os/
enabled=1
metadata_expire=7d
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
    cat > /etc/yum/repos.d/fedora-updates-modular.repo << EOF
[updates-modular]
name=Fedora Modular \$releasever - \$basearch - Updates
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/updates/\$releasever/Modular/\$basearch/
enabled=1
gpgcheck=1
metadata_expire=6h
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
  elif [[ "$REPO_SOURCES" == "USTC" ]]; then
    echo "Replacing with USTC mirror (not implemented)"
  elif [[ -z "$REPO_SOURCES" ]]; then
    echo "Empty repo source, skip."
    return
  else
    # unknown 
    echo "Warning!! Unknown repo source, skip."
    return
  fi

  dnf makecache
}

function install_emacs () {
  cd /opt || exit 1
  if (wget "https://alpha.gnu.org/pub/pub/gnu/emacs/pretest/emacs-$1.tar.xz" > /dev/null 2>&1); then
    tar -xf "emacs-$1.tar.xz"
    dnf install -y gtk3-devel gtk3-immodules gtk3-tests motif-devel giflib giflib-devel libtiff libtiff-devel libtiff-tools gnutls-devel gnutls libXpm-devel ncurses ncurses-devel
    cd "emacs-$1" || exit 1
    ./configure --with-mailutils --with-x --with-sound=no # optional: --with-imagemagick --with-xwidgets
    make
    make install
  fi
}

function install_fd () {
  # Install FD.
  FD_VERSION="8.2.1"
  if (( FEDORA_VERSION < 28 )); then
  # For fedora < 28, need to install from the GitHub page
    wget https://github.com/sharkdp/fd/releases/download/v${FD_VERSION}/fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
    tar -xf fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
    cp fd-v${FD_VERSION}-x86_64-unknown-linux-gnu/fd /usr/bin/fd
  else
  # For fedora >= 28, just install from dnf
    dnf install -y fd-find
  fi
}

function install_git () {
  # (Optional) use up-to-date version of git
  # install belows to enable remote-https
  dnf install -y curl-devel expat-devel gettext-devel openssl-devel zlib-devel
  wget "https://mirrors.edge.kernel.org/pub/software/scm/git/git-$1.tar.gz"
  tar -zxf "git-$1.tar.gz"
  cd "git-$1" || exit 1
  ./configure
  make
  make install
}

function install_doom () {
  # Install Doom Emacs
  # Note that for better use, it also installs fd, ripgrep
  install_fd
  dnf install -y ripgrep
  git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
  if [[ "$REPO_SOURCES" == "THU" ]]; then
    sed -i_backup -e 's/elpa\.gnu\.org\/packages/mirrors.tuna.tsinghua.edu.cn\/elpa\/gnu/g' \
      -e 's/melpa\.org\/packages/mirrors.tuna.tsinghua.edu.cn\/elpa\/melpa/g' \
      -e 's/orgmode\.org\/elpa/mirrors.tuna.tsinghua.edu.cn\/elpa\/org/g' \
      ~/.emacs.d/core/core-packages.el
  fi
  ~/.emacs.d/bin/doom install
}

function install_pyenv () {
  dnf install -y xz make gcc zlib-devel bzip2 bzip2-devel \
    readline-devel sqlite sqlite-devel tk-devel libffi-devel openssl-devel
  curl https://pyenv.run | bash
}

function install_config_tools () {
  dnf install -y make cmake autoconf automake autopoint git \
                gcc gfortran \
                bzip2 gzip p7zip \
                environment-modules
}

function install_network_tools () {
  dnf install -y openssl-devel curl libcurl-devel wget
  # clash appimage for get over GFW
  clash_ver=$(get_latest_release Dreamacro/clash)
  if (wget "https://github.com/Dreamacro/clash/releases/$clash_ver/clash-linux-amd64-$clash_ver.gz" \
    -O clash.gz > /dev/null 2>&1); then
    gunzip clash.gz
    chmod +x clash
    mv clash /usr/bin/
  fi
}

function retrieve_sci_packages_from_tmcws () {
  # retrieve scientific packages from tmc workstation
  # note: you must first ensure that connecting to tmcws is password-free. The following command check it
  rsync_cmd="rsync --exclude=*.o --exclude=*.a --exclude=*.pyc -vazru --inplace --progress"
  # TODO use an array for a better code view
  if (ssh_connection_check "$TMCWS_ACCOUNT@$TMCWS_IP"); then
    # VASP
    $rsync_cmd "${TMCWS_ACCOUNT}@$TMCWS_IP:/opt/software/vasp/5.4.4-16052018-patched/intel/2019.3" vasp-5.4.4
    $rsync_cmd "${TMCWS_ACCOUNT}@$TMCWS_IP:/opt/software/vasp/vasppot-5.4" .
    # WIEN2k
    $rsync_cmd "${TMCWS_ACCOUNT}@$TMCWS_IP:/opt/software/wien2k/19.2/intel/2019.3" wien2k-v19.2
    $rsync_cmd "${TMCWS_ACCOUNT}@$TMCWS_IP:/opt/software/wien2k/16.1/intel/2019.3" wien2k-v16.1
  else
    echo "Warning!! Fail to connect to TMCWS account ${TMCWS_ACCOUNT}@$TMCWS_IP. Check IP and SSH setup."
  fi
}

function install_sci_tools () {
  # Zotero for bibliography
  if (wget "https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64" \
    -O zotero.tar.bz2 > /dev/null 2>&1); then
    bunzip2 zotero.tar.bz2
    tar -xf zotero.tar
    cd Zotero_linux-x86_64 || return
    ./set_launcher_icon
    mkdir -p ~/.local/share/applications
    ## you may need to link the desktop on your own
    #ln -s "$(realpath zotero.desktop)" ~/.local/share/applications/zotero.desktop
  fi
  # texstudio and texlive for writing tex
  # units for unit conversion
  # (xm)grace, gnuplot, imagemagick, ghostscript, povray for visualization
  dnf install -y texstudio units grace gnuplot imagemagick ghostscript \
    povray povray-scences \
    texlive texlive-texlive-en-doc texlive-texlive-zh-cn-doc

  if [[ "$REPO_SOURCES" == "THU" ]]; then
  # permanently switch to TUNA repo for CTAN
  # tlmgr is shipped with texlive
    tlmgr option repository https://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/tlnet
  fi

  # VESTA, require GTK3
  if (wget "https://jp-minerals.org/vesta/archives/3.5.7/VESTA-gtk3.tar.bz2" > /dev/null 2>&1); then
    dnf install -y gtk3 gtk3-devel
    bunzip2 VESTA-gtk3.tar.bz2
    tar -xf VESTA-gtk3.tar
  fi
  # Xcrysden
  if (wget "http://www.xcrysden.org/download/xcrysden-1.6.2-linux_x86_64-shared.tar.gz" > /dev/null 2>&1); then
    # install xcrysden requirements
    dnf install -y tk tk-devel tcl tcl-devel tcl-togl tcl-togl-devel openbabel openbabel-devel \
      fftw-libs libxmu libxmu-devel libx11-devel mesa-libglu mesa-libglu-devel imagemagick
    tar -zxf xcrysden-1.6.2-linux_x86_64-shared.tar.gz
    mv xcrysden-1.6.2-bin-shared xcrysden-1.6.2
  fi
  return
}

# the main stream
function init_workstation_fedora () {

  if (( FEDORA_VERSION < FEDORA_VERSION_MIN )); then
    echo "Error! Too low Fedora release version"
    echo "($FEDORA_VERSION < minimal version $FEDORA_VERSION_MIN)"
    exit 1
  fi

  replace_repo_sources
  dnf update
  dnf upgrade
  
  install_network_tools
  install_config_tools
  install_sci_tools
  install_pyenv
  retrieve_sci_packages_from_tmcws
  
  [[ -n "$EMACS_VERSION" ]] && install_emacs "$EMACS_VERSION"
  [[ -n "$GIT_VERSION" ]] && install_git "$GIT_VERSION"
  [[ "$INSTALL_DOOM" == "YES" ]] && install_doom
}

function main () {
  opts=("$@")
  if (( $# == 0 )); then
    help "$0"
  else
    case ${opts[0]} in
      "--help"     | "-h" ) help "$0" ;;
      "--start"    | "-s" ) init_workstation_fedora ;;
      "--retrieve" | "-r" ) retrieve_sci_packages_from_tmcws ;;
      * ) echo "Error: unknown option " "${opts[0]}"; \
        help "$0"; exit 1 ;;
    esac
  fi
}

main "$@"

