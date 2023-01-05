#!/usr/bin/env bash
# shellcheck disable=SC2086

source "$(dirname ${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]})/common.sh"

cwd=$(pwd)

IS_LINUX=0
[ "$(uname)" == "Linux" ] && IS_LINUX=1
USE_DNF=0
IS_FEDORA=0
FEDORA_VERSION_MIN=35
FEDORA_VERSION_MAX=35
if (( IS_LINUX )); then
  which dnf 1>/dev/null 2>&1 && USE_DNF=1
  OSNAME=$(awk -F = '/^NAME/ {print $2}' /etc/os-release)
  # strip quotes
  OSNAME=${OSNAME%\"}
  OSNAME=${OSNAME#\"}
  echo $OSNAME
  [[ $OSNAME == "Fedora" ]] || [[ $OSNAME == "Fedora Linux" ]] && IS_FEDORA=1
  FEDORA_VERSION=$(awk -F = '/VERSION_ID/ {print $2}' /etc/os-release)
fi
YES_NO_P=("No" "Yes")
_DRY_RUN=${dry:=1}
update=${update:=0}
if (( update )); then
  DNF_CMD="dnf"
else
  DNF_CMD="dnf --disablerepo=*updates*"
fi

help_info() {
  cecho i "============================================="
  cecho i "Install script of minyez Fedora (Y9000P 3060)"
  cecho i "============================================="
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
  (( FEDORA_VERSION < FEDORA_VERSION_MIN )) && \
    cecho e "Error: Fedora version $FEDORA_VERSION < minimal version $FEDORA_VERSION_MIN. Exit" && exit 1
  (( FEDORA_VERSION > FEDORA_VERSION_MAX )) && \
    cecho e "Error: Fedora version $FEDORA_VERSION > maximal version $FEDORA_VERSION_MAX. Exit" && exit 1
  echo "  Version: ${FEDORA_VERSION}"
}

_install_config() {
  cecho i "Installing tools for config ..."
  ((_DRY_RUN)) && return
  # compiler. conservative install
  #sudo $DNF_CMD --disablerepo="*" --enablerepo=fedora -y install \
  # auto config
  sudo $DNF_CMD -y install make cmake doxygen autoconf automake binutils binutils-devel libtool || exit 2
  # shells, ruby
  sudo $DNF_CMD -y install zsh tcsh ShellCheck ruby ruby-devel || exit 2
  # vi
  sudo $DNF_CMD -y install vim-enhanced || exit 2
}

_install_nvidia() {
  cecho i "Installing NVIDIA drivers and CUDA"
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y config-manager --add-repo=https://negativo17.org/repos/fedora-nvidia.repo
  sudo $DNF_CMD -y install cuda nvidia-driver nvidia-settings nvidia-driver-libs.i686
  cecho i "Installing CUDA samples from NVIDIA official repo"
  distro=fedora$(rpm -E %fedora)
  sudo $DNF_CMD -y config-manager --add-repo https://developer.download.nvidia.com/compute/cuda/repos/$distro/x86_64/cuda-$distro.repo
  sudo $DNF_CMD -y install cuda-examples-11-6
}

_install_flatpak() {
  cecho i "Installing flatpak and related"
  flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
  cecho s "Flatpak installed. Some softwares can be installed: (may need proxy to accelarate)"
  cecho i "  flatpak install flathub com.netease.CloudMusic"
  cecho i "  flatpak install flathub com.tencent.wemeet"
  cecho i "  flatpak install flathub org.audacityteam.Audacity"
}

_install_rpmfree() {
  cecho i "Installing RPMfree, codecs and OBS"
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install "https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm"
  sudo $DNF_CMD -y groupupdate multimedia --setop="install_weak_deps=False" --exclude=PackageKit-gstreamer-plugin
  sudo $DNF_CMD -y groupupdate sound-and-video
  sudo $DNF_CMD -y obs-studio
}

_install_rust() {
  # install rust by rustup
  # the rust in Fedora repo is somewhat out-of-date
  cecho i "Installing Rust and cargo by rustup ..."
  ((_DRY_RUN)) && return
  curl https://sh.rustup.rs -sSf | bash -s -- -q -y --no-modify-path || exit 2
  cecho s "Rust installed"
}

_install_helix() {
  cecho e "Not implements"
  return
  sudo $DNF_CMD group install "C Development Tools and Libraries"
  git clone --recurse-submodules --shallow-submodules -j8 https://github.com/helix-editor/helix
  cd helix || return 0
  cargo install --path helix-term
  cecho i "If you meet error, try git submodule update --init --recursive --recommend-shallow"
}

_install_dev() {
  cecho i "Installing dev tools, including compilers and gdb..."
  ((_DRY_RUN)) || sudo $DNF_CMD -y debuginfo-install libgcc libstdc++ glibc libibverbs libnl3 librdmacm libuuid numactl-libs || exit 2
  ((_DRY_RUN)) || sudo $DNF_CMD -y install gdb cgdb || exit 2
  ((_DRY_RUN)) || sudo $DNF_CMD -y install gcc gfortran gcc-c++ clang llvm clang-tools-extra || exit 2
}

# network
_install_net_tools() {
  cecho i "Installing net tools, e.g. SSL, curl, wget ..."
  ((_DRY_RUN)) || sudo $DNF_CMD -y install openssl-devel curl libcurl-devel wget || exit 2
  # use the latest clash
  ((_DRY_RUN)) || sudo $DNF_CMD -y install clash || exit 2
  # cecho i "Downloading clash ..."
  # ((_DRY_RUN)) && return
  # wget https://github.com/Dreamacro/clash/releases/download/v1.8.0/clash-linux-amd64-v1.8.0.gz || exit 2
  # if ! gunzip clash-linux-amd64-v1.8.0.gz && mv clash-linux-amd64-v1.8.0 clash && chmod +x clash && sudo mv clash /usr/local/bin/; then
  #   exit 2
  # fi
  echo s "Net tools installed"
}

# network
_install_ocr() {
  cecho i "Installing OCR tools tesseract ..."
  ((_DRY_RUN)) || sudo $DNF_CMD -y install tesseract tesseract-equ \
    tesseract-langpack-{deu,chi_sim,chi_tra,jpn}* \
    tesseract-script-{hans,hant,japanese}* tesseract-tools || exit 2
  # use the latest clash
  cecho s "Tesseract and language packages are installed"
}

# python
_install_python() {
  cecho i "Installing system python and packages ..."
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install python python3 mkdocs* python3-notebook python3-scipy python3-rasterio || exit 2
  if [[ $(which pip) != "/usr/bin/pip" ]]; then
    cecho e "Error: not system pip, current pip = $(which pip)"
    exit 2
  fi
  sudo pip install rst2html
}

_install_latex() {
  cecho i "Installing LaTeX of TeXLive with medium scheme..."
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install texstudio texlive texlive-scheme-medium texlive-{texlive-en-doc,texlive-zh-cn-doc} \
  	texlive-{vancouver,revtex,revtex-doc,revtex4,revtex4-doc,achemso,tocbibind} || exit 2
  cecho i "Updating font map"
  # On Fedora 35, the font map is not correctly updated after TeXLive installation
  # Update it manually.
  # see https://tex.stackexchange.com/questions/621447/unable-to-generate-a-pdf-on-fedora-35-pdftex-error-cannot-open-type-1-font-fi
  (( FEDORA_VERSION == 35 )) && sudo updmap-sys
}

_install_latex_full() {
  cecho i "Installing LaTeX of TeXLive with full scheme..."
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install texlive-scheme-full || exit 2
}

# pyenv, rbenv and nvm
_install_xxenv() {
  cecho i "Installing environment managements, e.g. pyenv, rbenv and nvm..."
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install xz xz-devel make zlib-devel bzip2 bzip2-devel \
    readline-devel sqlite sqlite-devel tk-devel libffi-devel openssl-devel || exit 2
  curl https://pyenv.run | bash
  curl -fsSL https://github.com/rbenv/rbenv-installer/raw/HEAD/bin/rbenv-installer | bash || exit 2
  # modify pager to avoid hanging at git branch in nvm install
  # see comments in https://stackoverflow.com/a/48370253
  export LESS="--no-init --quit-if-one-screen -R"
  wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash || exit 2
  # loads nvm and install the latest version
  \. ~/.nvm/nvm.sh || exit 2
  nvm install node || exit 2
}

# various tools
_install_misc() {
  cecho i "Installing misc tools, e.g. PDF reading, plotting, audio, CLOC (count lines of code) ..."
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install units cloc okular pdfmod \
    grace gnuplot ImageMagick ghostscript povray \
      gzip p7zip zstd \
      environment-modules direnv \
      jq thefuck fzf \
      ripgrep fd-find \
      lshw htop \
      screen{fetch,key} \
      qalculate-gtk flameshot \
      rofi rofi-themes rofi-devel*
      pavucontrol paman || exit 2
  # NOTE: paman is useful to increase the volume larger than 150 and even 200

  sudo $DNF_CMD -y install pandoc* || exit 2

  cecho i "Installing tools written in Rust ..."
  sudo $DNF_CMD -y procs exa gitui hyperfine dua-cli duf || exit 2
  cecho s "Finished installing misc tools :)"
}

_install_snap() {
  cecho i "Installing snap ..."
  ((_DRY_RUN)) && return
  # may have "snap is unusable due to missing files" error
  # use a lower squashfs version
  # see https://stackoverflow.com/questions/68580043/snap-is-unusable-due-to-missing-files
  #sudo $DNF_CMD -y install snapd squashfs-tools-4.4-5.git1.fc34
  sudo $DNF_CMD -y install snapd || exit 2
  sudo ln -s /var/lib/snapd/snap /snap || exit 2
  cecho s "snap installed and linked"
  cecho i "Useful snaps:"
  echo    " draw.io: snap install drawio"
  echo    "  Typora: snap install typora"
}

_install_language_tools() {
  cecho i "Installing languages tools: Rime, Kana Kanji, sdcv ..."
  ((_DRY_RUN)) && return
  #(( _DRY_RUN )) && return
  #sudo $DNF_CMD -y install ibus-rime librime librime-devel ibus-kkj || exit 2
  sudo $DNF_CMD -y install ibus-rime librime librime-devel || exit 2
  ibus-daemon -drx
  cecho s "Rime and Kana Kanji installed"
  cecho i "  you need to run ibus-setup to add them to input method"
  sudo $DNF_CMD -y install sdcv || exit 2
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
_install_omz() {
  cecho i "Installing oh-my-zsh ..."
  ((_DRY_RUN)) && return
  sudo $DNF_CMD -y install autojump-zsh || exit 2
  [[ ! -d ${ZSH_CUSTOM:=~/.oh-my-zsh} ]] && sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  [[ ! -e ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions ]] && git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
  [[ ! -e ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions ]] && git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
  [[ ! -e ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/themes/powerlevel10k ]] && git clone --depth=1 https://gitee.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
}

_install_emacs() {
  # install emacs-native-comp
  cecho i "Installing Emacs of native-comp branch ..."
  ((_DRY_RUN)) && return
  sudo dnf -y copr enable deathwish/emacs-pgtk-nativecomp || exit 2
  sudo dnf -y install emacs emacs-devel emacs-pkg-info || exit 2
  cecho s "emacs-native-comp installed"
  rm -rf ~/.emacs.d && git clone https://github.com/hlissner/doom-emacs.git ~/.emacs.d || exit 2
  cecho s "Doom emacs cloned, please run doom install manually"
}

_install_doom() {
  cecho i "Install Doom. Running ~/.emacs.d/bin/doom install (may break due to network problem) ..."
  ((_DRY_RUN)) && return
  ~/.emacs.d/bin/doom -y install || exit 2
  ~/.emacs.d/bin/doom sync || exit 2
  builddir=$(ls -d ~/.emacs.d/.local/straight/build-*[0-9] 2>/dev/null)
  cd "$builddir" || cd "$cwd" && return
  cd liberime || cd "$cwd" && return
  EMACS_MAJOR_VERSION=$(emacs --version | head -1 | awk '{print $3}' | awk -F"." '{print $1}')
  export EMACS_MAJOR_VERSION && make
  cd "$cwd" || return
}
# on graphics issue, when installing with basic graphics mode
# https://www.reddit.com/r/Fedora/comments/o9onf7/help_fedora_34_is_not_using_my_amd_gpui_think/h3cwkk6/

_install_virtualbox() {
  cecho i "Installing Virtual Box from virtualbox.org ..."
  ((_DRY_RUN)) && return
  sudo dnf -y install @development-tools || exit 2
  sudo dnf -y install kernel-headers kernel-devel dkms elfutils-libelf-devel qt5-qtx11extras || exit 2
  wget -q https://download.virtualbox.org/virtualbox/rpm/fedora/35/x86_64/VirtualBox-6.1-6.1.34_150636_fedora35-2.x86_64.rpm -O VirtualBox-6.1.rpm || exit 2
  sudo rpm -i VirtualBox-6.1.rpm || exit 2
  sudo dnf -y install virtualbox-guest-additions fence-agents-vbox || exit 2
  cecho s "Virtual Box v6.1, guest additions and fence agents installed"
  cecho i "To install Win10, you need image from https://www.microsoft.com/en-gb/software-download/windows10ISO"
  cecho w "Note: to enable shared clipboard and directories, you also have to install guest additions inside the Guest machine (say Win10)"
  cecho w "      Device - Insert GuestAddition Image... - Go to My Computer and install from the disk"
}

_install_manually() {
  # some thing to install but not implemented in script
  cecho i "You have to install the following by yourself:"
}

i_basic() {
  # basics
  cecho i "Updating the system ..."
  ((_DRY_RUN)) || sudo $DNF_CMD -y update --exclude kernel* || exit 2
  _install_compiler_config
  _install_net_tools
  _install_language_tools
  _install_omz
  _install_python
  _install_xxenv
}

i_emacs() {
  # emacs
  _install_emacs
  _install_doom
}

i_latex() {
  # latex
  [[ $1 == "f" ]] && _install_latex_full && return
  _install_latex
}

usage() {
  echo "Usage: $0 <arg>"
  echo "Args:"
  echo "  c : check prerequisite and exit"
  echo "  h : print this message and exit"
  echo ""
  echo "  b : install basic tools, e.g. compilers, intepreters"
  echo "  l : install full latex"
  echo "  s : install snap"
  echo "  e : install Doom emacs of native comp branch"
  echo "  m : install misc tools"
  echo "  f : install flatpak and suggest softwares"
  echo "  v : install virtual box"
  echo ""  
  echo "  a : automatic install, i.e. all except Virtual Box"
}

help_info

(( $# == 0 )) && usage && exit 2

case "$1" in
  -h | --help | help | h ) usage; exit ;;
  -c | c ) check_prereq; exit ;;
  * ) check_prereq ;;
esac

(( _DRY_RUN )) && cecho w "Dry mode is switched on. Prefix the script with dry=0 to actually install"

case "$1" in
  -b | b ) i_basic;;
  -e | e ) i_emacs ;;
  -l | l ) i_latex f ;;
  -s | s ) _install_snap ;;
  -f | f ) _install_flatpak ;;
  -o | o ) _install_ocr ;;
  -m | m ) _install_misc ;;
  -z | z ) _install_zotero ;;
  -v | v ) _install_virtualbox ;;
  -a | a ) i_basic;
    i_emacs; i_latex;
    # snap and other tools
    _install_snap;
    _install_misc;
    _install_zotero;
    # remind of manual installs
    _install_manually;
esac
