#!/usr/bin/env bash

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

install_compiler_config() {
  # compiler. conservative install
  sudo dnf --disablerepo="*" --enablerepo=fedora -y install \
  	gcc gfortran gcc-c++ clang llvm clang-tools-extra
  # auto config
  sudo dnf -y install make cmake autoconf automake binutils binutils-devel tcsh ShellCheck
}

# network
sudo dnf -y install openssl-devel curl libcurl-devel wget clash

# python
sudo dnf -y install python mkdocs
if [[ $(which pip) != "/usr/bin/pip" ]]; then
  cecho e "Error: not system pip, current pip = $(which pip)"
  exit 2
fi
sudo pip install rst2html


# Write with latex
sudo dnf -y install texstudio texlive texlive-scheme-medium texlive-{texlive-en-doc,texlive-zh-cn-doc} \
	texlive-{vancouver,revtex,revtex-doc,revtex4,revtex4-doc,achemso,tocbibind}
# sudo dnf -y install texlive-scheme-full


# pyenv
install_pyenv() {
  sudo dnf -y install xz xz-devel make zlib-devel bzip2 bzip2-devel \
    readline-devel sqlite sqlite-devel tk-devel libffi-devel openssl-devel
  curl https://pyenv.run | bash
}

# rbenv
install_rbenv() {
  curl -fsSL https://github.com/rbenv/rbenv-installer/raw/HEAD/bin/rbenv-installer | bash
}

# various tools
sudo dnf -y install units okular \
  grace gnuplot ImageMagick ghostscript povray \
    gzip p7zip zstd \
    environment-modules direnv \
    vim-enhanced neovim \
    jq \
    ripgrep fd-find \
    lshw htop \
    qalculate-gtk flameshot

install_snap() {
  # may have "snap is unusable due to missing files" error
  # use a lower squashfs version
  # see https://stackoverflow.com/questions/68580043/snap-is-unusable-due-to-missing-files
  sudo dnf -y install snapd squashfs-tools-4.4-5.git1.fc34
}

install_language_tools() {
  cecho i "Installing languages tools: Rime, sdcv..."
  #(( _DRY_RUN )) && return
  sudo dnf -y install ibus-rime
  ibus-daemon -drx
  cecho s "Rime installed, but you need to run ibus-setup to add Rime to input method"
  sudo dnf -y install sdcv
  cecho s "sdcv installed"

  preferdict="stardict-oxford-gb-formated-2.4.2"
  cecho i "Start dowloading the prefered zh_CN dictionary: $preferdict"
  sudo mkdir -p /usr/share/stardict/dic
  wget $wgetopts http://download.huzheng.org/zh_CN/$preferdict.tar.bz2
  sudo tar -xjf $preferdict.tar.bz2 -C /usr/share/stardict/dic
  cecho s "sdcv dictionary installed"
}

# oh-my-zsh and related plugins
install_zsh_omz() {
  sudo dnf -y install zsh autojump-zsh
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
}

# on graphics issue, when installing with basic graphics mode
# https://www.reddit.com/r/Fedora/comments/o9onf7/help_fedora_34_is_not_using_my_amd_gpui_think/h3cwkk6/
