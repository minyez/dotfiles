#!/usr/bin/env bash
# shellcheck disable=SC2086

cwd=$(pwd)
# conversertive install on this intel+AMD machine
DNF_CMD="dnf --disablerepo=* --enablerepo=fedora"
# or disable the updates repo in /etc/yum.repos.d/ and set
#DNF_CMD="dnf"

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
  #sudo $DNF_CMD --disablerepo="*" --enablerepo=fedora -y install \
  sudo $DNF_CMD -y install \
  	gcc gfortran gcc-c++ clang llvm clang-tools-extra
  # auto config
  sudo $DNF_CMD -y install make cmake autoconf automake binutils binutils-devel tcsh ShellCheck
}

# network
install_net_tools() {
  sudo $DNF_CMD -y install openssl-devel curl libcurl-devel wget
  # use the latest clash
  wget https://github.com/Dreamacro/clash/releases/download/v1.8.0/clash-linux-amd64-v1.8.0.gz
  gunzip clash-linux-amd64-v1.8.0.gz && mv clash-linux-amd64-v1.8.0 clash && chmod +x clash && sudo mv clash /usr/local/bin/
}

# python
install_python() {
  sudo $DNF_CMD -y install python mkdocs
  if [[ $(which pip) != "/usr/bin/pip" ]]; then
    cecho e "Error: not system pip, current pip = $(which pip)"
    exit 2
  fi
  sudo pip install rst2html
}

install_latex() {
sudo $DNF_CMD -y install texstudio texlive texlive-scheme-medium texlive-{texlive-en-doc,texlive-zh-cn-doc} \
	texlive-{vancouver,revtex,revtex-doc,revtex4,revtex4-doc,achemso,tocbibind}
}

install_latex_full() {
  sudo $DNF_CMD -y install texlive-scheme-full
}

# pyenv, rbenv and nvm
install_xxenv() {
  sudo $DNF_CMD -y install xz xz-devel make zlib-devel bzip2 bzip2-devel \
    readline-devel sqlite sqlite-devel tk-devel libffi-devel openssl-devel
  curl https://pyenv.run | bash
  curl -fsSL https://github.com/rbenv/rbenv-installer/raw/HEAD/bin/rbenv-installer | bash
  # modify pager to avoid hanging at git branch in nvm install
  # see comments in https://stackoverflow.com/a/48370253
  export LESS="--no-init --quit-if-one-screen -R"
  wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
  # loads nvm and install the latest long-term-support version
  \. ~/.nvm/nvm.sh
  nvm install --lts
}

# various tools
install_misc() {
sudo $DNF_CMD -y install units okular \
  grace gnuplot ImageMagick ghostscript povray \
    gzip p7zip zstd \
    environment-modules direnv \
    vim-enhanced neovim \
    jq \
    ripgrep fd-find \
    lshw htop \
    qalculate-gtk flameshot
sudo $DNF_CMD -y install pandoc*
}

install_snap() {
  # may have "snap is unusable due to missing files" error
  # use a lower squashfs version
  # see https://stackoverflow.com/questions/68580043/snap-is-unusable-due-to-missing-files
  #sudo $DNF_CMD -y install snapd squashfs-tools-4.4-5.git1.fc34
  sudo $DNF_CMD -y install snapd
  sudo ln -s /var/lib/snapd/snap /snap
}

install_language_tools() {
  cecho i "Installing languages tools: Rime, sdcv..."
  #(( _DRY_RUN )) && return
  sudo $DNF_CMD -y install ibus-rime librime librime-devel
  ibus-daemon -drx
  cecho s "Rime installed"
  cecho i "  you need to run ibus-setup to add Rime to input method"
  sudo $DNF_CMD -y install sdcv
  cecho s "sdcv installed"
  cecho i "  consider download dicts from http://download.huzheng.org to /usr/share/stardict/dic"
  #preferdict="stardict-oxford-gb-formated-2.4.2"
  #cecho i "Start dowloading the prefered zh_CN dictionary: $preferdict"
  #sudo mkdir -p /usr/share/stardict/dic
  #wget $wgetopts http://download.huzheng.org/zh_CN/$preferdict.tar.bz2
  #sudo tar -xjf $preferdict.tar.bz2 -C /usr/share/stardict/dic
  #cecho s "sdcv dictionary installed"
}

# oh-my-zsh and related plugins
install_zsh_omz() {
  sudo $DNF_CMD -y install zsh autojump-zsh
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
}

install_emacs() {
  # install emacs-native-comp
  sudo dnf -y copr enable deathwish/emacs-pgtk-nativecomp
  sudo dnf -y install emacs emacs-devel
  cecho s "emacs-native-comp installed"
  rm -rf ~/.emacs.d && git clone --depth=1 https://github.com/hlissner/doom-emacs.git .emacs.d
  cecho s "Doom emacs cloned"
}

install_doom() {
  cecho i "Running ~/.emacs.d/bin/doom install (may break due to network problem)"
  ~/.emacs.d/bin/doom -y install
  ~/.emacs.d/bin/doom sync
  builddir=$(ls -d ~/.emacs.d/.local/straight/build-*[0-9] 2>/dev/null)
  cd "$builddir" || return
  cd liberime || return
  EMACS_MAJOR_VERSION=$(emacs --version | head -1 | awk '{print $3}' | awk -F"." '{print $1}')
  export EMACS_MAJOR_VERSION && make
  cd "$cwd" || return
}
# on graphics issue, when installing with basic graphics mode
# https://www.reddit.com/r/Fedora/comments/o9onf7/help_fedora_34_is_not_using_my_amd_gpui_think/h3cwkk6/



