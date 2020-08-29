# Original theme https://github.com/agnoster zsh theme
# original from gist https://gist.github.com/smileart/3750104
# TODO:
#     change to monokai colorscheme

ZSH_THEME_GIT_PROMPT_DIRTY='±'
SEGMENT_SEPARATOR=$'\ue0b0'

function _git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || ref="➦ $(git show-ref --head -s --abbrev |head -n1 2> /dev/null)"
  echo "${ref/refs\/heads\//⭠ }$(parse_git_dirty)"
}

function _git_info() {
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    local BG_COLOR=green
    if [[ -n $(parse_git_dirty) ]]; then
      BG_COLOR=yellow
      FG_COLOR=black
    fi

    if [[ ! -z $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
        BG_COLOR=red
        FG_COLOR=white
    fi
    echo "%{%K{$BG_COLOR}%}{$SEGMENT_SEPARATOR}%{%F{$FG_COLOR}%} $(_git_prompt_info) %{%F{$BG_COLOR}%K{blue}%}$SEGMENT_SEPARATOR"
  else
    echo "%{%K{blue}%}$SEGMENT_SEPARATOR"
  fi
}

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('`basename $VIRTUAL_ENV`') '
}

PROMPT_HOST="%{%b%F{gray}%K{black}%} %(?.%{%F{green}%}✔.%{%F{red}%}✘)%{%F{yellow}%} %n %{%F{black}%}"
PROMPT_DIR="%{%F{white}%} %~%  "
PROMPT_SU="%(!.%{%k%F{blue}%K{black}%}$SEGMENT_SEPARATOR%{%F{yellow}%} ⚡ %{%k%F{black}%}.%{%k%F{blue}%})$SEGMENT_SEPARATOR%{%f%k%b%}"

PROMPT="%{%f%b%k%}$PROMPT_HOST$(_git_info)$PROMPT_DIR$PROMPT_SU
$(virtualenv_info)~ "
# ❯
RPROMPT="%{$fg[green]%}[%*]%{$reset_color%}"
