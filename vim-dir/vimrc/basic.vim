" 移除由于新版VIM使用Python3导致的DeprecationWarning
" see https://github.com/vim-vdebug/vdebug/issues/366
if has('python3')
  silent! python3 1
endif

" 设置相对行号
set rnu
set nu

" 设置光标接近底部或上部第三行时开始移屏
set so=3

" 设置语法检测
syntax on

" TAB键长度在indent文件夹中，以适用不同类型文件
set tabstop=8
set smarttab

" 自动缩进
set ai
" 智慧缩进
set si
set wrap

" 显示光标所在行列
" set cursorline
" set cursorcolumn

" 设置unix文件格式
"set fileformat=uinix


"选择colorscheme
"colorscheme wombat256mod
"colorscheme slate
"colorscheme vividchalk
"colorscheme desert
"colorscheme neon
"colorscheme molokai

"设置tmux
set term=xterm-256color

"去除错误时发出的杂音
set noerrorbells
set novisualbell
set t_vb=
set tm=500
"语法高亮
syntax on
"设置终端配色
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif
if $COLORTERM == 'truecolor'
    set t_Co=256
endif
" always show the status line
set laststatus=2

" 忽略编译文件
set wildignore=*.o,*~,*.pyc,*.mod
set anti enc=utf-8

set shell=/bin/bash

set ruler                       " 显示标尺  
autocmd InsertEnter * se cul    " 用浅色高亮当前行  
"打开文件类型检测, 加了这句才可以用智能补全
set completeopt=longest,menu

set foldmethod=syntax

" Disable arrows
" habbit destroying
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" 寄存器复制粘贴
noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>Y "+y
noremap <Leader>P "+p

" 普通模式下，方向键改为切换窗口
noremap <Up>    <C-w>k
noremap <Down>  <C-w>j
noremap <Left>  <C-w>h
noremap <Right> <C-w>l

" 普通模式下，用tab键导航标签页切换
noremap <tab> gt
noremap <s-tab> gT


" 映射ESC
imap fj <esc>
vmap fj <esc>

" 命令模式启动
nnoremap : ;
nnoremap ; :

" 命令模式下，映射到:lclose以关闭编译结果
nnoremap nm :lclose<CR>
nnoremap mn :lclose<CR>
