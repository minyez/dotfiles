#!/usr/bin/env bash
source _common.sh
source tmcws_pkgs.sh

function help() {
  echo "Retrieve packages from TMC workstation"
  echo ""
  echo "To add a new target, you only have to add the following to arrays in tmcws_pkgs.sh"
  echo ""
  echo "  1. package name as an identifier \`pkgs_names\`"
  echo "  2. path to the package on the TMC workstation \`pkgs_urls\`"
  echo "  3. (optional) output path \`pkgs_outputs\`"
  echo ""
  echo "Check existing examples before you want to write one."
  echo ""
  echo "Note that GitHub extracting may fail due to network problem"
}

function retrieve_tmcws_pkgs() {
  # retrieve packages from tmc workstation
  if (ssh_connection_check "${TMCWS_CONNECTION}"); then
    for name in "${pkgs_names[@]}"; do
      url="${pkgs_urls[$name]}"
      output=${pkgs_outputs[$name]}
      if [[ -z "$output" ]]; then
        output="$name"
      fi
      if [[ -z "$url" ]]; then
        echo "Warning: URL of $name not set, skip"
        continue
      fi
      sync_pkg 0 "$PKGS_DIR" "$rsync_opts" "$TMCWS_CONNECTION" \
        "$name" "$url" "$output"
    done
  else
    echo "Error: fail to connect to TMCWS under ${TMCWS_CONNECTION}. Please check IP and SSH setup."
  fi
}

if (( $# == 0 )); then
  help
  exit 0
fi

retrieve_tmcws_pkgs

