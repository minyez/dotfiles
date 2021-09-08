#!/usr/bin/env bash
# shellcheck disable=SC2086

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
_DRY_RUN=1

wgetopts="-nv --show-progress --progress=bar:force"

cecho() {
  # colorize echo
  case $1 in
    "s" | "success" ) color="\e[1;32m";;
    "e" | "error" ) color="\e[1;31m";;
    "i" | "info" ) color="\e[1;34m";;
    "w" | "warn" ) color="\e[38;2;234;80;3m";;
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
  (( _DRY_RUN )) && cecho w "Dry mode is switched on. Change _DRY_RUN to 0 to actually install"
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
  (( _DRY_RUN )) && return
  sudo dnf -y update
}

install_compiler() {
  cecho i "Installing compilers..."
  (( _DRY_RUN )) && return
  sudo dnf --disablerepo="*" --enablerepo=fedora -y install gcc gfortran gcc-c++
}

install_config_tools() {
  cecho i "Installing config tools..."
  (( _DRY_RUN )) && return
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
  (( _DRY_RUN )) && return
  sudo dnf -y install openssl-devel curl libcurl-devel wget clash
}

install_texlive_full() {
  cecho i "Installing full texlive..."
  (( _DRY_RUN )) && return
  sudo dnf -y install texlive-scheme-full
}

install_basic_python_tools() {
  cecho i "Installing basic python tools with system pip..."
  (( _DRY_RUN )) && return
  if [[ $(which pip) != "/usr/bin/pip" ]]; then
    cecho e "Error: not system pip, current pip = $(which pip)"
    exit 2
  fi
  sudo pip install rst2html
  cecho s "Installed rst2html"
}

# No more sougou
#install_sogou() {
#  cecho i "Installing Sogou Pingyin..."
#  sudo dnf -y install fcitx fcitx-{qt{4,5},table,configtool,table-{extra,other,chinese}}
#  sudo dnf -y install gnome-tweaks
#}
install_language_tools() {
  cecho i "Installing languages tools: Rime, sdcv..."
  (( _DRY_RUN )) && return
  sudo dnf -y install ibus-rime
  ibus-daemon -drx
  cecho s "Rime installed, but you need to run ibus-setup to add Rime to input method"
  sudo dnf -y install sdcv
  cecho s "sdcv installed"
  cecho i "Start dowloading zh_CN dictionary: "
  sudo mkdir -p /usr/share/stardict/dic
  wget $wgetopts http://download.huzheng.org/zh_CN/stardict-oxford-gb-2.4.2.tar.bz2
  sudo tar -xjf stardict-oxford-gb-2.4.2.tar.bz2 -C /usr/share/stardict/dic
  cecho s "sdcv dictionary installed"
}

install_wine() {
  # add winehq repo and install stable branch
  sudo dnf -y install dnf-plugins-core
  sudo dnf config-manager --add-repo "https://dl.winehq.org/wine-builds/fedora/${FEDORA_VERSION}/winehq.repo"
  sudo dnf install winehq-stable
  # download the latest winetricks
  sudo wget $wgetopts https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks -O /usr/local/bin/winetricks \
    && sudo chmod +x /usr/local/bin/winetricks || exit 1
  # install prerequisites, including fonts
  sudo dnf install wine-mono cabextract p7zip-plugins wqy-zenhei-fonts
  cecho s "You are now prepared to run winecfg!"
}

help_info
check_prereq

#update_dnf
#install_config_tools
#install_compiler
#install_network_tools
#install_texlive_full
#install_language_tools
install_wine
