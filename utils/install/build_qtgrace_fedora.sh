#!/usr/bin/env bash
sudo dnf -y install qconf qt5-qtsvg qt5-qtsvg-devel
wget https://jaist.dl.sourceforge.net/project/qtgrace/qtgrace_v026_src.zip
unzip qtgrace_v026_src.zip

cd qtgrace_v026_src && \
  sed 's/qmake -unix/qmake-qt5 -unix/' Makefile > Makefile.qt5 && \
  make -f Makefile.qt5
# sed -i 's/PREFIX = \/usr/PREFIX = \/usr\/local/' Make.conf
