#!/usr/bin/env bash

IS_LINUX=0
[ "$(uname)" == "Linux" ] && IS_LINUX=1
USE_DNF=0
IS_FEDORA=0
FEDORA_VERSION_MIN=30
if (( IS_LINUX )); then
  which dnf 1>/dev/null 2>&1 && USE_DNF=1
  [[ $(awk -F = '/^NAME/ {print $2}' /etc/os-release) == "Fedora" ]] && IS_FEDORA=1
  FEDORA_VERSION=$(awk -F = '/VERSION_ID/ {print $2}' /etc/os-release)
fi
YES_NO_P=("No" "Yes")

cecho() {
  # colorize echo
  case $1 in
    "s" | "success" ) color="\e[1;32m";;
    "e" | "error" ) color="\e[1;31m";;
    "i" | "info" ) color="\e[1;34m";;
    *) color="\e[0m"
  esac
  shift 1
  echo -e "$color$*\e[0m"
}

help_info() {
  cecho i "=================================="
  cecho i "Install script of Fedora (AMD CPU)"
  cecho i "=================================="
  echo "  Your OS: $(uname -snr)"
  (( IS_LINUX )) && \
  echo "      CPU: $(lscpu | awk '/Model name/ {sep = ""; for (i = 3; i <= NF; i++) {printf("%s%s", sep, $i); sep=OFS}; printf("\n")}')"
}

check_prereq() {
  echo "Is Fedora: ${YES_NO_P[$IS_FEDORA]}"
  echo "  Use DNF: ${YES_NO_P[$USE_DNF]}"
  if (( IS_FEDORA == 0 )) || (( USE_DNF == 0 )); then
    cecho e "Error: This script require Fedora Linux and DNF for package manager. Exit"
    exit 1
  fi
  if (( FEDORA_VERSION < FEDORA_VERSION_MIN )); then
    cecho e "Error: Fedora version $FEDORA_VERSION < minimal version $FEDORA_VERSION_MIN. Exit"
    exit 1
  fi
}

while [ ${#} -gt 0 ]; do
  case "$1" in
  -h | --help | help | h ) help_info; \
    exit ;;
  esac
  shift 1
done

update_dnf() {
  cecho i "Updating Fedora..."
  sudo dnf -y update
}

install_compiler() {
  cecho i "Installing compilers..."
  sudo dnf --disablerepo="*" --enablerepo=fedora -y install gcc gfortran gcc-c++
}

install_config_tools() {
  cecho i "Installing config tools..."
  sudo dnf -y install \
    make cmake autoconf automake git binutils binutils-devel tcsh \
    bzip2 gzip p7zip \
    environment-modules \
    vim-enhanced neovim \
    jq \
    ripgrep fd-find \
    lshw htop
}

install_network_tools() {
  cecho i "Installing network tools..."
  sudo dnf -y install openssl-devel curl libcurl-devel wget clash
}

install_texlive_full() {
  cecho i "Installing full texlive..."
  sudo dnf -y install texlive-scheme-full
}

install_basic_python_tools() {
  cecho i "Installing basic python tools..."
  sudo pip install rst2html
  cecho s "Installed rst2html"
}

# No more sougou
#install_sogou() {
#  cecho i "Installing Sogou Pingyin..."
#  sudo dnf -y install fcitx fcitx-{qt{4,5},table,configtool,table-{extra,other,chinese}}
#  sudo dnf -y install gnome-tweaks
#}
install_rime() {
  cecho i "Installing Rime..."
  sudo dnf -y install ibus-rime
  ibus-daemon -drx
  cecho s "Rime installed, but you need to run ibus-setup to add Rime to input method"
}

help_info
check_prereq

install_rime
