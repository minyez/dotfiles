case "$0" in
          -sh|sh|*/sh)	modules_shell=sh ;;
       -ksh|ksh|*/ksh)	modules_shell=ksh ;;
       -zsh|zsh|*/zsh)	modules_shell=zsh ;;
    -bash|bash|*/bash)	modules_shell=bash ;;
esac
#module() { eval `/usr/local/opt/tcl-tk/bin/tclsh /Users/stevezhang/software/devtools/modules-4.0.0/libexec/modulecmd.tcl $modules_shell $*`; }
#module() { eval `/usr/local/Cellar/tcl-tk/8.7a1/bin/tclsh /Users/stevezhang/software/devtools/modules-4.2.1/libexec/modulecmd.tcl $modules_shell $*`; }
module() { eval `/usr/local/Cellar/tcl-tk/8.7a1/bin/tclsh /Users/stevezhang/software/devtools/modules-4.5.3/libexec/modulecmd.tcl $modules_shell $*`; }
#!/usr/bin/env bash
# load alias
#source ~/.bashrc_alias
# load source public to all shells
#source ~/.public_source

# dump core. Not necessary if GDB is not available
# ulimit -c unlimited

# ================================
# 重设库路径
#unset DYLD_LIBRARY_PATH
#export DYLD_LIBRARY_PATH

#export LIBRARY_PATH="/usr/local/lib:/usr/lib:$LIBRARY_PATH"
#export DYLD_LIBRARY_PATH="/usr/local/lib:$DYLD_LIBRARY_PATH"

# set the color of terminal
export LS_OPTIONS='--color=auto' # 如果没有指定，则自动选择颜色
export CLICOLOR='Yes' #是否输出颜色
export HOMEBREW_NO_AUTO_UPDATE=1 # 禁用Homebrew自动更新

export MODULE_HOME="$HOME/software/devtools/modules-4.5.3"
export SD="$HOME/Documents/SelfDevelopment/"
export PATH="$MODULE_HOME/bin:$PATH"
source $MODULE_HOME/init/bash
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
module load qt-5.15.0 
module load poppler-20.90.0
# Tcl/Tk 8.7
module load tcltk-8.7
## JAVA
export JAVA_HOME="$(/usr/libexec/java_home)"
# visual tools
module load vesta-v3
# ================================
# GNU GCC,G++,GFORTRAN
#module load gnu/10.1.0

#VASP POTCARs
module load vasp/vasppot-5.4
module load vasp/utils

# 由于environment module的问题，这里手动加入DYLD_LIBRARY_PATH动态库路径
# 各HOME和ROOT在对应modulefile内定义
#export DYLD_LIBRARY_PATH="$MPI_ROOT/lib:$DYLD_LIBRARY_PATH"
#export DYLD_LIBRARY_PATH="$FFTW3_HOME/lib:$DYLD_LIBRARY_PATH"
#export DYLD_LIBRARY_PATH="$LIBXC_HOME/lib:$DYLD_LIBRARY_PATH"

# ================================
# 自定义项目，为保证放在顶端而置于底层
module load custom-libbin
#export DYLD_LIBRARY_PATH="$HOME/lib:$DYLD_LIBRARY_PATH"
# 自定义环境变量名
eval $(thefuck --alias)

# 加载开发个人python项目所需模块
module load py-readmana
module load py-mykit
# ================================
# 重载模块使得alias可用
#module reload

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
if [ -f '/Users/stevezhang/code/google-cloud-sdk/path.bash.inc' ]; then . '/Users/stevezhang/code/google-cloud-sdk/path.bash.inc'; fi
# The next line enables shell command completion for gcloud.
if [ -f '/Users/stevezhang/code/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/stevezhang/code/google-cloud-sdk/completion.bash.inc'; fi

# Turn on fish when loading bash.
# Fish can share the environment variables in bash in this way,
# while turning on profile command in iTerm2 cannot make it.
# Remember to update .public_source.
# Howver, this might be problematic in remote connection by ssh. Let's see :)
# 在Git文件夹里，Mac上的fish有些太卡了...感觉是因为环境变量太长了
# 在非Git文件夹里没有任何问题
# 下面这个命令可以用来检查fish_prompt的耗时，第一列数字是单个操作消耗时间，第二个应该是累计时间
# fish --profile prompt.prof -ic 'fish_prompt; exit'; sort -nk 2 prompt.prof

#exec /usr/local/bin/fish

# pandoc completion
eval "$(pandoc --bash-completion)"

