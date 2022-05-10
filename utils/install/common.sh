cecho() {
  # colorize echo
  colors=""
  colore=""
  if [[ -t 1 ]]; then
    case $1 in
      "s" | "success" ) colors="\e[1;32m";;
      "e" | "error" ) colors="\e[1;31m";;
      "i" | "info" ) colors="\e[1;34m";;
      "w" | "warn" ) colors="\e[38;2;234;80;3m";;
      *) colors="\e[0m"
    esac
  colore="\e[0m"
  fi
  shift 1
  echo -e "$colors$*$colore"
}
