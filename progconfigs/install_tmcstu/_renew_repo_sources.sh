#!/usr/bin/env bash
yumrepos="/etc/yum.repos.d"

function backup_fedora_repo() {
  if [[ -f $yumrepos/$1.repo ]]; then
    res="$1.repo"
    dest="$1.repo_backup_$(date +"%y%m%d-%H")"
    echo "backup $yumrepos/{$res => $dest}"
    cp "$yumrepos/$res" "$yumrepos/$dest"
  fi
}

function replace_repo_sources() {
  # add repository sources to make full use of network
  # see https://mirrors.ustc.edu.cn
  #     https://mirrors.tuna.tsinghua.edu.cn
  #     https://mirrors.pku.edu.cn
  REPO_SOURCES="$1"
  if [[ "$REPO_SOURCES" == "THU" ]]; then
    echo "Replacing with TUNA mirror"
    backup_fedora_repo fedora
    backup_fedora_repo fedora-updates
    backup_fedora_repo fedora-modular
    backup_fedora_repo fedora-updates-modular
    cat > $yumrepos/fedora.repo << EOF
[fedora]
name=Fedora \$releasever - \$basearch
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/releases/\$releasever/Everything/\$basearch/os/
metadata_expire=28d
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
    cat > $yumrepos/fedora-updates.repo << EOF
[updates]
name=Fedora \$releasever - \$basearch - Updates
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/updates/\$releasever/Everything/\$basearch/
enabled=1
gpgcheck=1
metadata_expire=6h
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
    cat > $yumrepos/fedora-modular.repo << EOF
[fedora-modular]
name=Fedora Modular \$releasever - \$basearch
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/releases/\$releasever/Modular/\$basearch/os/
enabled=1
metadata_expire=7d
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
    cat > $yumrepos/fedora-updates-modular.repo << EOF
[updates-modular]
name=Fedora Modular \$releasever - \$basearch - Updates
failovermethod=priority
baseurl=https://mirrors.tuna.tsinghua.edu.cn/fedora/updates/\$releasever/Modular/\$basearch/
enabled=1
gpgcheck=1
metadata_expire=6h
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-\$releasever-\$basearch
skip_if_unavailable=False
EOF
  elif [[ "$REPO_SOURCES" == "USTC" ]]; then
    echo "Replacing with USTC mirror (not implemented)"
  elif [[ -z "$REPO_SOURCES" ]]; then
    echo "Empty repo source, skip."
    return
  else
    # unknown 
    echo "Warning!! Unknown repo source, skip."
    return
  fi

  dnf makecache
}

replace_repo_sources "$@"

