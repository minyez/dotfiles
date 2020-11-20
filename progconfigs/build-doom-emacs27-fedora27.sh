#!/usr/bin/env bash
#run with sudo
EMACS_VERSION="27.0.91"
#cd /opt
#wget https://alpha.gnu.org/pub/pub/gnu/emacs/pretest/emacs-${EMACS_VERSION}.tar.xz
#tar -xf emacs-${EMACS_VERSION}.tar.xz
#dnf install -y gtk3-devel gtk3-immodules gtk3-tests motif-devel giflib giflib-devel libtiff libtiff-devel libtiff-tools gnutls-devel gnutls libXpm-devel ncurses ncurses-devel
#cd emacs-${EMACS_VERSION} || exit 1
#./configure --with-mailutils --with-x --with-sound=no # optional: --with-imagemagick --with-xwidgets
#make
#make install

## FD. For fedora < 28, need to install from the GitHub page
#FD_VERSION="8.1.1"
#wget https://github.com/sharkdp/fd/releases/download/v8.1.1/fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
#tar -xf fd-v${FD_VERSION}-x86_64-unknown-linux-gnu.tar.gz
#cp fd-v${FD_VERSION}-x86_64-unknown-linux-gnu/fd /usr/bin/fd
## TODO add bash completion
## For fedora >= 28, just install from dnf
## dnf install fd-find

## (Optional) use up-to-date version of git
#GIT_VERSION="2.29.2"
## install belows to enable remote-https
#dnf install curl-devel expat-devel gettext-devel openssl-devel zlib-devel
#wget https://mirrors.edge.kernel.org/pub/software/scm/git/git-${GIT_VERSION}.tar.gz
#tar -xf git-${GIT_VERSION}.tar.gz
#cd git-${GIT_VERSION}
#./configure
#make
#make install
# Doom
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
