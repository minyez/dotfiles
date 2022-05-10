#!/usr/bin/env bash
if (( $# != 1 )); then
  echo "Usage: $0 hwdbfile"
  exit 1
fi

hwdbf="$1"
if [[ ! -f "$hwdbf" ]]; then
  echo "hwdb file not exist, exit"
  exit 2
fi

hwdbdir=/usr/lib/udev/hwdb.d/
echo "Copying $hwdbf to $hwdbdir, ask for sudo"
sudo cp "$hwdbf" $hwdbdir
sudo systemd-hwdb update
sudo udevadm trigger
