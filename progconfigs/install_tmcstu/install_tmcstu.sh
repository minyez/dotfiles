#!/usr/bin/env bash

function help() {
  echo "$1: A script to initialize a TMC student PC workstation for daily use"
  echo ""
  echo "Notes: "
  echo "  1. it may require sudo when replaceing repo source or installing by dnf"
  echo "  2. to retrieve scientific packages, you need a password-free connection to your TMC workstation account"
  echo "     Currently try will retrieve:"
  echo "       VASP   (5.4.4 with patch)"
  echo "       VASPPOT                  "
  echo "       WIEN2k (v16.1, v19.2)"
  echo ""
  echo "Options:"
  echo "  [ -h ] : this help info"
  echo "  [ -i ] : start the install"
  echo "  [ -r ] : retrieve sci. packages on TMCWS"
  echo "  [ -e ] : download external useful tools"
  echo "  [ -p ] : install pyenv"
  echo ""
  echo "TODOs:"
  echo "  1. obtain newest workable hosts and setup"
  echo "  2. more repos: VMD, jabref, vscode"
  echo "  3. more pkgs: pyscf, conda, QE, ATAT, deepmd, lammps"
  echo ""
  echo "Update: 2021-04-08"
  echo ""
  echo "Contributors: MY Zhang"
}

## if you want to install emacs, new-version git or Doom
## just uncomment out the corresponding variable
#EMACS_VERSION="27.1.91"
#GIT_VERSION="2.31.1"
#INSTALL_DOOM=YES
REPO_SOURCES="THU"
# TMCWS ssh connection, for retriving packages from TMC workstation
TMCWS_CONNECTION="zhangmy@222.29.156.110"

if [[ ! -f /etc/os-release ]]; then
  echo "Error! Not a Fedora release"
  echo ""
  help "$0"
  exit 1
fi

FEDORA_VERSION=$(awk -F = '/VERSION_ID/' /etc/os-release)
FEDORA_VERSION_MIN=29

#function install_emacs() {
#  cd /opt || exit 1
#  if (wget "https://alpha.gnu.org/pub/pub/gnu/emacs/pretest/emacs-$1.tar.xz" > /dev/null 2>&1); then
#    tar -xf "emacs-$1.tar.xz"
#    sudo dnf install -y gtk3-devel gtk3-immodules gtk3-tests motif-devel giflib giflib-devel libtiff libtiff-devel libtiff-tools gnutls-devel gnutls libXpm-devel ncurses ncurses-devel
#    cd "emacs-$1" || exit 1
#    ./configure --with-mailutils --with-x --with-sound=no # optional: --with-imagemagick --with-xwidgets
#    make
#    sudo make install
#  fi
#}
#
#function install_fd() {
#  # Install FD.
#  FD_VERSION="8.2.1"
#  if (( FEDORA_VERSION < 28 )); then
#  # For fedora < 28, need to install from the GitHub page
#    wget https://github.com/sharkdp/fd/releases/download/v${FD_VERSION}/fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
#    tar -xf fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
#    cp fd-v${FD_VERSION}-x86_64-unknown-linux-gnu/fd /usr/bin/fd
#  else
#  # For fedora >= 28, just install from dnf
#    sudo dnf install -y fd-find
#  fi
#}
#
#function install_git() {
#  # (Optional) use up-to-date version of git
#  # install belows to enable remote-https
#  sudo dnf install -y curl-devel expat-devel gettext-devel openssl-devel zlib-devel
#  wget "https://mirrors.edge.kernel.org/pub/software/scm/git/git-$1.tar.gz"
#  tar -zxf "git-$1.tar.gz"
#  cd "git-$1" || exit 1
#  ./configure
#  make
#  sudo make install
#}

#function install_doom() {
#  # Install Doom Emacs
#  # Note that for better use, it also installs fd, ripgrep
#  install_fd
#  sudo dnf install -y ripgrep
#  git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
#  if [[ "$REPO_SOURCES" == "THU" ]]; then
#    sed -i_backup -e 's/elpa\.gnu\.org\/packages/mirrors.tuna.tsinghua.edu.cn\/elpa\/gnu/g' \
#      -e 's/melpa\.org\/packages/mirrors.tuna.tsinghua.edu.cn\/elpa\/melpa/g' \
#      -e 's/orgmode\.org\/elpa/mirrors.tuna.tsinghua.edu.cn\/elpa\/org/g' \
#      ~/.emacs.d/core/core-packages.el
#  fi
#  ~/.emacs.d/bin/doom install
#}

function install_pyenv() {
  sudo dnf install -y xz xz-devel make gcc zlib-devel bzip2 bzip2-devel \
    readline-devel sqlite sqlite-devel tk-devel libffi-devel openssl-devel
  #curl https://pyenv.run | bash
}

function install_config_tools() {
  sudo dnf install -y make cmake autoconf automake git \
                gcc gfortran gcc-c++ \
                bzip2 gzip p7zip \
                environment-modules \
                vim-enhanced neovim \
                jq
}

function install_network_tools() {
  sudo dnf install -y openssl-devel curl libcurl-devel wget
}

function install_sci_tools () {
  sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
  sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
  # units for unit conversion
  # insect for REPL scientific calculation
  # (xm)grace, gnuplot, imagemagick, ghostscript, povray for visualization
  # texstudio and texlive for writing tex, jabref for bibtex
  sudo dnf install -y units insect \
    grace gnuplot ImageMagick ghostscript povray \
    texstudio texlive texlive-texlive-en-doc texlive-texlive-zh-cn-doc jabref
  if [[ "$REPO_SOURCES" == "THU" ]]; then
  # permanently switch to TUNA repo for CTAN
  # tlmgr is shipped with texlive
    tlmgr option repository https://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/tlnet
  fi
  return
}

function init_fedora() {

  if (( FEDORA_VERSION < FEDORA_VERSION_MIN )); then
    echo "Error! Too low Fedora release version"
    echo "($FEDORA_VERSION < minimal version $FEDORA_VERSION_MIN)"
    exit 1
  fi

  sudo bash _renew_repo_sources.sh "$REPO_SOURCES"
  sudo dnf update
  sudo dnf upgrade
  
  install_network_tools
  install_config_tools
  install_sci_tools
  
  [[ -n "$EMACS_VERSION" ]] && install_emacs "$EMACS_VERSION"
  [[ -n "$GIT_VERSION" ]] && install_git "$GIT_VERSION"
  [[ "$INSTALL_DOOM" == "YES" ]] && install_doom
}

function main() {
  opts=("$@")
  if (( $# == 0 )); then
    help "$0"
  else
    case ${opts[0]} in
      "-h" ) help "$0" ;;
      "-i" ) init_fedora ;;
      "-e" ) bash _download_external_repos.sh ;;
      "-r" ) bash _retrieve_tmcws_pkgs.sh "${TMCWS_CONNECTION}" ;;
      "-p" ) install_pyenv ;;
      * ) echo "Error: unknown option " "${opts[0]}"; \
        help "$0"; exit 1 ;;
    esac
  fi
}

main "$@"

