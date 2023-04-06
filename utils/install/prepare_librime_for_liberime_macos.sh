#!/bin/sh
# Install librime from the release package and build liberime
# Tested for Doom Emacs, liberime version 0.0.6
librime_install_dir=/opt/librime
SUDO=sudo  # set to empty if librime_install_dir is accessible without sudo permission
liberime_dir=$HOME/.config/emacs/.local/straight/build-28.2/liberime

librime_ver=1.8.5
librime_name=rime-08dd95f-macOS.tar.bz2

SED=gsed

# download the liberime package, install and symlink directories to company liberime makefile
if [ ! -d "$librime_install_dir" ]; then
  wget https://github.com/rime/librime/releases/download/$librime_ver/$librime_name
  $SUDO mkdir "$librime_install_dir" \
    || { echo "fail to create $librime_install_dir" && exit 1; }
  $SUDO tar -C "$librime_install_dir" -jxf $librime_name \
    || { echo "fail to extract rime release tarball $librime_name to $librime_install_dir" && exit 1; }
  rm -f $librime_name
  $SUDO mv "$librime_install_dir/dist" "$librime_install_dir/build" \
    || { echo "failt to rename dist -> build under $librime_install_dir" && return 1; }
  cd "$librime_install_dir" || return
  for d in lib share bin include; do
    $SUDO ln -s build/$d
  done
  $SUDO ln -s build/include src
  echo "librime initialized under $librime_install_dir"
else
  echo "found $librime_install_dir, skip"
fi

cd "$liberime_dir" || { echo "fail to open $liberime_dir" && exit 1; }
if [ -f src/liberime-core.dylib ]; then
  echo "src/liberime-core.dylib is already built"
else
  RIME_PATH="$librime_install_dir" emacs -Q -batch -eval "(add-to-list 'load-path \"$(pwd)\")" -load liberime.el -f liberime-build
  # remove the Release library dir in rpath to avoid run time error
  # this might be redundant if one builds librime from source
  $SED -i "s|:/opt/librime/build/lib/Release\(/\)\{0,1\}||g" Makefile-liberime-build
  make -f Makefile-liberime-build
fi

# check if liberime is workable
emacs -Q -batch -eval "(add-to-list 'load-path \"$(pwd)\")" -load liberime.el \
  -eval "(if (liberime-workable-p) (message \"liberime workable\") (message \"liberime not workable\"))"
