#!/usr/bin/env bash
# This file included th names and URLs of external repositories
# to download

repos_names=(
  "VESTA"
  "XCrySDen"
  "Zotero"
  "JabRef"
)

declare -A repos_urls
repos_urls=(
  ["VESTA"]="https://jp-minerals.org/vesta/archives/3.5.7/VESTA-gtk3.tar.bz2"
  ["XCrySDen"]="http://www.xcrysden.org/download/xcrysden-1.6.2-linux_x86_64-shared.tar.gz"
  ["Zotero"]="https://www.zotero.org/download/client/dl?channel=release&platform=linux-x86_64"
  ["JabRef"]="https://www.fosshub.com/JabRef.html?dwl=jabref-5.2-1.x86_64.rpm"
)

# optional array to set the name of the downloaded file
# if not set, the basename of url will be used.
declare -A repos_outputs
repos_outputs=(
  ["Zotero"]="zotero.tar.bz2"
  ["JabRef"]="jabref-5.2-1.x86_64.rpm"
)

# directory to store downloaded repositories. No need to change, basically
REPOS_DIR="repos"

