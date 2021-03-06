case "$0" in
          -sh|sh|*/sh)	modules_shell=sh ;;
       -ksh|ksh|*/ksh)	modules_shell=ksh ;;
       -zsh|zsh|*/zsh)	modules_shell=zsh ;;
    -bash|bash|*/bash)	modules_shell=bash ;;
esac
# module-4.0.0
#module() { eval `/usr/local/opt/tcl-tk/bin/tclsh /Users/stevezhang/software/devtools/modules-4.0.0/libexec/modulecmd.tcl $modules_shell $*`; }
# module-4.2.1
#module() { eval `/usr/local/Cellar/tcl-tk/8.7a1/bin/tclsh /Users/stevezhang/software/devtools/modules-4.2.1/libexec/modulecmd.tcl $modules_shell $*`; }
# module-4.5.3
module() { eval `/usr/local/Cellar/tcl-tk/8.7a1/bin/tclsh /Users/stevezhang/software/devtools/modules-4.5.3/libexec/modulecmd.tcl $modules_shell $*`; }

# 重设库路径
#unset DYLD_LIBRARY_PATH
unset MANPATH
export MANPATH

# set the color of terminal
export LS_OPTIONS='--color=auto' # 如果没有指定，则自动选择颜色
export LC_ALL="en_US.UTF-8"
export CLICOLOR='Yes' #是否输出颜色
export HOMEBREW_NO_AUTO_UPDATE=1 # 禁用Homebrew自动更新
#export EMACS="*term*"

export MODULE_HOME="$HOME/software/devtools/modules-4.5.3"
export PATH="$MODULE_HOME/bin:$PATH"
source $MODULE_HOME/init/zsh
module use $HOME/.modulefiles/projects
module use $HOME/.modulefiles/sci
module use $HOME/.modulefiles/mathlib
module use $HOME/.modulefiles/compiler
module use $HOME/.modulefiles/devtools
module use $HOME/.modulefiles/visual
module use $HOME/.modulefiles/alias
# general library
module load general-lib
# aliases
module load general-alias
module load ssh-alias
module load git-alias
#module load qt-5.15.0 
# Tcl/Tk 8.7
#module load tcltk-8.7
# osxfuse
#module load osxfuse-3.8.2
#module load e2fsprogs-1.45.6
## JAVA
export JAVA_HOME="$(/usr/libexec/java_home)"
# VASP POTCARs
module load vasp/vasppot-5.4
module load vasp/utils

# 自定义项目，为保证放在顶端而置于底层
module load custom-libbin
# F2PY GNU test
##export DYLD_LIBRARY_PATH="/usr/lib:$DYLD_LIBRARY_PATH"
# 自定义环境变量名
eval $(thefuck --alias)

# 加载开发个人python项目所需模块
#module load py-readmana
#module load py-mykit
module load py-mushroom

# pyenv initialization
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  if command - v pyenv-virtualenv-init > /dev/null 2>&1; then
    eval "$(pyenv virtualenv-init -)"
  fi
fi
# rbenv initialization
if command -v rbenv 1>/dev/null 2>&1; then
  eval "$(rbenv init -)"
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/stevezhang/code/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/stevezhang/code/google-cloud-sdk/path.zsh.inc'; fi
# The next line enables shell command completion for gcloud.
if [ -f '/Users/stevezhang/code/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/stevezhang/code/google-cloud-sdk/completion.zsh.inc'; fi

# If you come from bash you might have to change your $PATH.
#     export PATH=$HOME/bin:/usr/local/bin:$PATH

# Ruby Version Management
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
#export PATH="$PATH:$HOME/.rvm/bin"
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 


# ===== ZSH setup =====
# Path to your oh-my-zsh installation.
export ZSH="/Users/stevezhang/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
#ZSH_THEME="robbyrussell"
#ZSH_THEME="avit"
#ZSH_THEME="xxf"
ZSH_THEME="agnoster-my"
#ZSH_THEME="agnoster"
#ZSH_THEME="candy"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='nvim'
else
   export EDITOR='/usr/bin/vim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
# reload module to load aliases
#module reload

# only username appear for local machine for agnoster theme
prompt_context() {
  if [[ "$USER" != "$DEFAULT_USER" || -n "$SSH_CLIENT" ]]; then
    #prompt_segment black default "%(!.%{%F{yellow}%}.)$USER"
  fi
}

#compaudit | xargs chmod g-w
fpath=(/usr/local/share/zsh-completions $fpath)
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/stevezhang/.zshrc'
autoload -Uz compinit
compinit
# End of lines added by compinstall
 
zstyle -e ':completion::complete:-command-::executables' ignored-patterns '
  [[ "$PREFIX" == ./* ]] && {
    local -a tmp; set -A tmp ./*(/)
    : ${(A)tmp::=${tmp// /\\ }}
    reply=(${(j:|:)tmp})
  }
'
# Here, with vi/vim/nvim, ignore files with particular suffixes
# typically one don't read bib manually
zstyle ':completion:*:*:(vi|vim|nvim|git):*' file-patterns \
	'^*.(aux|pdf|dvi|bbl|mod|pyc|o|exe|fdb_latexmk|synctex.gz|blg|run.xml|bib|toc|fls|bcf|gif|png|jpg|jpeg|eps):source-files' \
	'*:all-files'
zstyle ':completion:*:*:(open):*' file-patterns \
	'^*.(aux|mod|pyc|o|exe|bib|bbl|fdb_latexmk|synctex.gz|blg|toc|fls|bcf|):source-files' \
	'*:all-files'
zstyle ':completion:*:*:(python|python2|python3|cp|mv):*' file-patterns \
	'^*.(mod|pyc|o):source-files' '*:all-files'
# Show up menu
zstyle ':completion:*' menu yes select

# lunchy completion
LUNCHY_DIR=$(dirname `gem which lunchy`)/../extras
if [ -f $LUNCHY_DIR/lunchy-completion.zsh ]; then
  . $LUNCHY_DIR/lunchy-completion.zsh
fi
# pandoc completion
eval "$(pandoc --bash-completion)"
# suppress error when no regex match is found
unsetopt nomatch

cd ~

## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('/Users/stevezhang/.pyenv/versions/miniconda3-4.7.12/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "/Users/stevezhang/.pyenv/versions/miniconda3-4.7.12/etc/profile.d/conda.sh" ]; then
#        . "/Users/stevezhang/.pyenv/versions/miniconda3-4.7.12/etc/profile.d/conda.sh"
#    else
#        export PATH="/Users/stevezhang/.pyenv/versions/miniconda3-4.7.12/bin:$PATH"
#    fi
#fi
#unset __conda_setup
## <<< conda initialize <<<


[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
