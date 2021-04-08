#!/usr/bin/env bash
source tmcws_pkgs.sh

function ssh_connection_check() {
  ssh -q -o BatchMode=yes  -o StrictHostKeyChecking=no -o ConnectTimeout=5 "$1" 'exit 0'
  return "$?"
}

function retrieve_packages_from_tmcws () {
  TMCWS_CONNECTION=$1
  # retrieve scientific packages from tmc workstation
  # note:
  #  1. you must first ensure that connecting to tmcws is password-free. This will be checked by ssh_connection_check
  #  2. info=progress2 to give a summary of current speed and progress
  #  3. Caveat: may break if TMCWS is behind some route such that a port number may be required
  #     A workaround: add `-p` option in rsync
  rsync_cmd="rsync --exclude=*.([ao]|pyc|mod|out) --exclude=vasp_* -azru --info=progress2 "
  # associated array: "local name"="remote url"
  # URL can either be the path to the root directory of the pacakge (!!!must end with a backslash!!!)
  #         or a path to single file.
  # using -a may raise syntax error: invalid arithmetic operator for names having dots
  mkdir -p pkgs
  cd pkgs || exit 2
  if (ssh_connection_check "${TMCWS_CONNECTION}"); then
    for name in "${!pkgs_names[@]}"; do
      if [[ -f "$name" ]]; then
        echo "Warning: file $name already retrieved, skip"
        continue
      fi
      if [[ -z "${pkgs_urls[$name]}" ]]; then
        echo "Warning: URL on TMCWS of $name not set, skip"
        continue
      fi
      echo "Syncing $name with:"
      echo "    $rsync_cmd ${TMCWS_CONNECTION}:${pkgs_urls[$name]} $name"
      if ($rsync_cmd "${TMCWS_CONNECTION}:${pkgs_urls[$name]}" "$name"); then
        echo "Success: $name synced"
      else
        echo "Error: $name"
      fi
    done
  else
    echo "Warning!! Fail to connect to TMCWS under ${TMCWS_CONNECTION}. Check IP and SSH setup."
  fi
  cd ..
}

if [[ $# != 1 ]]; then
  echo "Usage: $0 [TMCWS_CONNECTION]"
fi

retrieve_packages_from_tmcws "$@"

