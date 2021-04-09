#!/usr/bin/env bash
source _common.sh
source custom.sh

function help() {
  echo "$1: A script to install a new TMC student PC workstation for daily use"
  echo ""
  echo "Commands:"
  echo "  [ init   ] : minimal install from Fedora repository"
  echo "  [ full   ] : full install pakages and repos"
  echo "               missing ones will be downloaded or retrived from TMCWS"
  echo "  [ help   ] : this help info"
  echo "  [ docker ] : install Docker engine"
  #echo "  [ -r ] : retrieve packages on TMCWS"
  #echo "  [ -e ] : download external repositories"
  echo ""
  echo "Notes: "
  echo "  1. it may require sudo when replacing repo source or installing by dnf"
  echo "  2. to retrieve packages from your TMC workstation account, you need a password-free connection"
  echo ""
  echo "TODOs:"
  echo "  1. obtain newest workable hosts and setup for Google and GitHub, etc"
  echo "  2. more repos: VMD, jabref, v_sim"
  echo "  3. more pkgs: pyscf, conda, QE, ATAT, deepmd, lammps"
  echo "  4. installing these repos and packages"
  echo ""
  echo "Update: 2021-04-09"
  echo ""
  echo "Contributors: MY Zhang"
}

FEDORA_VERSION=$(get_fedora_ver)
FEDORA_VERSION_MIN=29

#function install_emacs() {
#  cd /opt || exit 1
#  if (wget "https://alpha.gnu.org/pub/pub/gnu/emacs/pretest/emacs-$1.tar.xz" > /dev/null 2>&1); then
#    tar -xf "emacs-$1.tar.xz"
#    sudo dnf -y install gtk3-devel gtk3-immodules gtk3-tests motif-devel giflib giflib-devel libtiff libtiff-devel libtiff-tools gnutls-devel gnutls libXpm-devel ncurses ncurses-devel
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
#  # since now the minimal version is 29, this is in fact obsolete.
#    wget https://github.com/sharkdp/fd/releases/download/v${FD_VERSION}/fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
#    tar -xf fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
#    cp fd-v${FD_VERSION}-x86_64-unknown-linux-gnu/fd /usr/bin/fd
#  else
#  # For fedora >= 28, just install from dnf
#    sudo dnf -y install fd-find
#  fi
#}
#
#function install_git() {
#  # (Optional) use up-to-date version of git
#  # install belows to enable remote-https
#  sudo dnf -y install curl-devel expat-devel gettext-devel openssl-devel zlib-devel
#  wget "https://mirrors.edge.kernel.org/pub/software/scm/git/git-$1.tar.gz"
#  tar -zxf "git-$1.tar.gz"
#  cd "git-$1" || exit 1
#  ./configure --with-openssl
#  make
#  sudo make install
#}

#function install_doom() {
#  # Install Doom Emacs
#  # Note that for better use, it also installs fd, ripgrep
#  install_fd
#  sudo dnf -y install ripgrep
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
  sudo dnf -y install xz xz-devel make gcc zlib-devel bzip2 bzip2-devel \
    readline-devel sqlite sqlite-devel tk-devel libffi-devel openssl-devel
  #curl https://pyenv.run | bash
}

function install_config_tools() {
  sudo dnf -y install make cmake autoconf automake git binutils binutils-devel\
                gcc gfortran gcc-c++ \
                bzip2 gzip p7zip \
                environment-modules \
                vim-enhanced neovim \
                jq \
                ripgrep fd-find
}

function install_network_tools() {
  sudo dnf -y install openssl-devel curl libcurl-devel wget
}

function install_sci_tools () {
  # units for unit conversion
  ##### insect for REPL scientific calculation
  # (xm)grace, gnuplot, imagemagick, ghostscript, povray for visualization
  # texstudio and texlive for writing tex, texdoc for documentation
  # extra useful packages
  # code (VS code) as a popular IDE
  sudo dnf -y install units \
    grace gnuplot ImageMagick ghostscript povray \
    texstudio texlive texlive-{texdoc,texlive-en-doc,texlive-zh-cn-doc} \
    code
  sudo dnf -y install texlive-{ctex,xcjk2uni} \
    texlive-{physics,abstract,wordcount,xargs,worksheet,wordlike,zhnumber} \
    texlive-{cleveref,overpic,SIunits,a0poster,algorithmicx,algorithms,answers,annotate} \
    texlive-{tabulary,appendix,augie,autonum,autopdf,babel,babelbib} \
    texlive-{tikz-dependency,tikz-3dplot}
  sudo dnf -y install texlive-beamer{,audience}
  sudo dnf -y install texlive-{vancouver,achemso,tocbibind,pkuthss,pkuthss-doc} \
    texlive-biblatex-{chem,chicago,phys,publist}

  # Texlive packages is managed by dnf, so no need to change tlmgr options. comment out
  #if [[ "$REPO_SOURCES" == "THU" ]]; then
  ## permanently switch to TUNA repo for CTAN
  ## tlmgr is shipped with texlive
  #  sudo tlmgr option repository https://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/tlnet
  ## may have error that cannot save 00texlive.installation to /usr/share/texlive/tlpkg/tlpobj/...
  ## but the default package repository is indeed changed to TUNA
  #fi
  return
}

function install_docker() {
  sudo dnf remove -y docker docker-client docker-client-latest \
                  docker-common \
                  docker-latest docker-latest-logrotate \
                  docker-logrotate \
                  docker-selinux docker-engine-selinux docker-engine
  sudo dnf -y install docker-ce docker-ce-cli containerd.io
}

function init_fedora() {

  if (( FEDORA_VERSION < FEDORA_VERSION_MIN )); then
    echo "Error! Too low Fedora release version"
    echo "($FEDORA_VERSION < minimal version $FEDORA_VERSION_MIN)"
    exit 1
  fi

  sudo bash _renew_repo_sources.sh "$REPO_SOURCES"
  sudo dnf -y update
  sudo dnf -y upgrade
  
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
      "help" | "h" | "-h" | "--help" ) help "$0" ;;
      "init" ) init_fedora ;;
      "full" ) echo "Not implemented :("; exit 2 ;;
      "docker" ) install_docker ;;
      # for test use
      "-e" ) bash _download_extern_repos.sh 1 ;;
      "-r" ) bash _retrieve_tmcws_pkgs.sh 1 ;;
#      "-p" ) install_pyenv ;;
      * ) echo "Error: unknown command/option " "${opts[0]}"; \
        help "$0"; exit 1 ;;
    esac
  fi
}

main "$@"

