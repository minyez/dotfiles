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
set tabstop=4
set smarttab

" 自动缩进
set ai
" 智慧缩进
set si
set wrap

" search case
set smartcase
set ignorecase

" Disable insert paste mode
set nopaste

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
" following terminal transparency
highlight Normal ctermbg=NONE guibg=NONE

" always show the status line
set laststatus=2

" 忽略编译文件
set wildignore=*.o,*~,*.pyc,*.mod
set anti enc=utf-8

set shell=/bin/bash

"set ruler                       " 显示标尺  
"autocmd InsertEnter * se cul    " 用浅色高亮当前行  
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

" 切换窗口
noremap <C-k> <C-w>k
noremap <C-j> <C-w>j
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l

" 普通模式下，用tab键导航标签页切换
noremap <tab> gt
noremap <s-tab> gT


" 映射ESC
imap jk <esc>
vmap mm <esc>

" 命令模式启动
nnoremap : ;
nnoremap ; :

" 搜索结果跳转至屏幕中央
nnoremap n nzz
nnoremap N Nzz

" 命令模式下，映射到:lclose以关闭编译结果
nnoremap nm :lclose<CR>
nnoremap mn :lclose<CR>

" 记住代码折叠状态
augroup remember_folds
  autocmd!
  autocmd BufWinLeave * mkview
  autocmd BufWinEnter * silent! loadview
augroup END

augroup auto_language_selection
    autocmd!
    autocmd Filetype c          setlocal ts=4 sw=4 expandtab
    autocmd Filetype latex      setlocal ts=2 sw=2 expandtab
    autocmd Filetype plaintex   setlocal ts=2 sw=2 expandtab
    autocmd Filetype tex        setlocal ts=2 sw=2 expandtab
    autocmd Filetype markdown   setlocal ts=4 sw=4 expandtab
    autocmd Filetype sh         setlocal ts=2 sw=2 expandtab
    autocmd Filetype matlab     setlocal ts=4 sw=4 expandtab
    autocmd Filetype cpp        setlocal ts=4 sw=4 expandtab
    autocmd Filetype xml        setlocal ts=2 sw=2 expandtab
    autocmd Filetype html       setlocal ts=2 sw=2 expandtab
    autocmd Filetype ruby       setlocal ts=4 sw=4 expandtab
    autocmd Filetype vim        setlocal ts=4 sw=4 expandtab
    autocmd Filetype python     setlocal ts=4 sw=4 expandtab
    autocmd Filetype fortran    setlocal ts=2 sw=2 expandtab
    autocmd Filetype javascript setlocal ts=4 sw=4 expandtab
    autocmd FileType json       setlocal ts=2 sw=2 expandtab
    autocmd Filetype scss       setlocal ts=2 sw=2 expandtab
    autocmd Filetype css        setlocal ts=2 sw=2 expandtab
    autocmd Filetype php        setlocal ts=4 sw=4 expandtab
    autocmd Filetype sql        setlocal ts=4 sw=4 expandtab
    autocmd Filetype java       setlocal ts=4 sw=4 expandtab
    autocmd Filetype lisp       setlocal ts=2 sw=2 expandtab
    autocmd Filetype java       colorcolumn=121
augroup END

