" Use vim-plug to manage plugins
" Use the following command to profile the startup of vim/neovim
"     vim --cmd 'profile start vim.profile' --cmd 'profile func *' --cmd 'profile file *'
call plug#begin('~/.local/share/nvim/plugged')
    Plug 'flazz/vim-colorschemes'
    Plug 'sirver/ultisnips' | Plug 'honza/vim-snippets'
    Plug 'tpope/vim-surround'
    Plug 'kien/ctrlp.vim'
    Plug 'salsifis/vim-transpose'
    Plug 'ervandew/supertab'
    Plug 'Yggdroot/indentLine'
    Plug 'godlygeek/tabular'
    Plug 'majutsushi/tagbar'
    Plug 'itchyny/lightline.vim'
    Plug 'scrooloose/nerdtree'
    Plug 'scrooloose/nerdcommenter'
    Plug 'neomake/neomake'
    Plug 'tpope/vim-sensible'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-commentary'
    Plug 'airblade/vim-gitgutter'
    Plug 'tommcdo/vim-exchange'
    Plug 'vim-scripts/VisInCr'
    "Plug 'benmills/vimux'
    "Plug 'lervag/vimtex'
    Plug 'myusuf3/numbers.vim'
"   " Plug 'yuttie/comfortable-motion.vim'
    Plug 'henrik/vim-indexed-search'
    "Plug 'mhinz/vim-startify'
    "Plug 'bkad/camelcasemotion'
"    Plug 'python-mode/python-mode'
"    Plug 'romainl/vim-qf'
"    Plug 'vim-pandoc/vim-pandoc'
"    Plug 'vim-pandoc/vim-pandoc-syntax'
    Plug 'tomtom/tcomment_vim'
"    Plug 'raimondi/delimitmate'
    Plug 'jiangmiao/auto-pairs'
"    Plug 'scrooloose/syntastic'
"   Comment YCM due to slow startup ~430ms
"    Plug 'valloric/youcompleteme'
    Plug 'editorconfig/editorconfig-vim'
""   Use deoplete instead
"    if has('nvim')
"        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"        Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
"    else
"        Plug 'Shougo/deoplete.nvim'
"        Plug 'roxma/nvim-yarp'
"        Plug 'roxma/vim-hug-neovim-rpc'
"    endif
"    Plug 'zchee/deoplete-jedi',
"    Plug 'davidhalter/jedi-vim'
    Plug 'machakann/vim-highlightedyank'
    Plug 'tmhedberg/SimpylFold'
    Plug 'sbdchd/neoformat'
    "Always load the vim-devicons as the very last one.
    Plug 'ryanoasis/vim-devicons'
    "orgmode emulator
    "Plug 'jceb/vim-orgmode'
    Plug 'axvr/org.vim'
    Plug 'cespare/vim-toml'
"   Another async compelete plugin to accompany vim-lsp
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
    Plug 'prabirshrestha/asyncomplete-ultisnips.vim'
    Plug 'prabirshrestha/vim-lsp'
    Plug 'mattn/vim-lsp-settings'
call plug#end()

let mapleader=","
filetype plugin on
filetype plugin indent on

" =========================================================
" neovim platform-dependent settings
" =========================================================
if has("unix")
    let s:uname = system("uname")
    " macOS
    if s:uname == "Darwin\n"
        let g:python_host_prog = "/Users/stevezhang/.pyenv/versions/miniconda2-4.7.12/bin/python"
        let g:python3_host_prog = "/Users/stevezhang/.pyenv/versions/miniconda3-4.7.12/bin/python"
    endif
endif

" =========================================================
" Plugin-specific configurations
" =========================================================

" ===== numbers.vim =====
let g:numbers_exclude = ['qf', 'nerdtree', 'unite', 'startify', 'gundo', 'vimshell', 'w3m']

" ===== camelcasemotion =====
"call camelcasemotion#CreateMotionMappings('<leader>')
" ===== pymode =====
"let g:pymode_python = 'python3'

" ===== colorschemes =====
"Select colorscheme
"colorscheme wombat256mod "a darkmode
"colorscheme github
"colorscheme molokai
colorscheme woju

" ===== lsp client: vim-lsp and related =====
" https://github.com/prabirshrestha/vim-lsp#registering-servers
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gs <plug>(lsp-document-symbol-search)
    nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
    nmap <buffer> gr <plug>(lsp-references)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> <leader>rn <plug>(lsp-rename)
    nmap <buffer> [g <plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <plug>(lsp-next-diagnostic)
    nmap <buffer> K <plug>(lsp-hover)
    "nnoremap <buffer> <expr><c-f> lsp#scroll(+4)
    "nnoremap <buffer> <expr><c-d> lsp#scroll(-4)
    let g:lsp_format_sync_timeout = 200
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
    " refer to doc to add more commands
endfunction

" supress inline diagnostics, i.e. virtual text
let g:lsp_diagnostics_virtual_text_enabled = 0
" instead use a float window
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_diagnostics_float_delay = 100
" suppress A> code action marker, which does not quite make sense for me
let g:lsp_document_code_action_signs_enabled = 0

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
" async completion, including lsp and ultisnips
" register ultisnip auto completion
if has('python3')
    "let g:UltiSnipsExpandTrigger="<c-e>"
    call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
        \ 'name': 'ultisnips',
        \ 'allowlist': ['*'],
        \ 'completor': function('asyncomplete#sources#ultisnips#completor'),
        \ }))
endif
" ===== end vim-lsp related =====

"" ===== deoplete =====
"let g:deoplete#enable_at_startup = 1
"" Pass a dictionary to set multiple options
" " ===== deoplete =====
" let g:deoplete#enable_at_startup = 1
" " Pass a dictionary to set multiple options
" call deoplete#custom#source('ultisnips', 'matchers', ['matcher_fuzzy'])
" call deoplete#custom#option({
" \ 'smart_case' : v:true,
" \ })
" let g:deoplete#auto_complete=1

" ===== org.vim =====
let g:org_state_keywords = ['TODO', 'WIP', 'DONE', 'CANCELED']

" ===== jedi =====
" disable autocompletion, cause we use deoplete for completion
let g:jedi#completions_enabled = 0
" open the go-to function in split, not another buffer
let g:jedi#use_splits_not_buffers = "winwidth"

" ===== Tagbar =====
let g:tagbar_width = 35
nnoremap <Leader>tb :TagbarOpen<CR>
nnoremap <Leader>tc :TagbarClose<CR>
"" trigger Tagbar at start
"autocmd VimEnter * nested :call tagbar#autoopen(0)
"autocmd BufEnter * nested :call tagbar#autoopen(0)
"let g:tagbar_autopreview = 1
" Add support for markdown files in tagbar.
let g:tagbar_type_markdown = {
    \ 'ctagstype': 'markdown',
    \ 'ctagsbin' : '/Users/stevezhang/code/from_GitHub/markdown2ctags/markdown2ctags.py',
    \ 'ctagsargs' : '-f - --sort=yes',
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
    \ }

" ===== UltiSnips =====
" If custom directory is used, you need to add it to rtp
"set runtimepath+=~/code/snippets
"let g:UltiSnipsSnippetsDir = '~/code/snippets/UltiSnips'
let g:UltiSnipsEditSplit = 'vertical'
let g:UltiSnipsUsePythonVersion = 3
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
" if trigger not working, add these lines
" make YCM compatible with UltiSnips (using supertab)
"let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
"let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" " ===== neomake =====
" " For default maker, check neomake wiki
" " https://github.com/neomake/neomake/wiki/Makers
" let g:neomake_serialize = 1
" let g:neomake_serialize_abort_on_error = 1
" "disabled open error list in a window. used to be 2 to show error in location window
" let g:neomake_open_list = 0
" let g:neomake_list_height = 6
" let g:neomake_python_enabled_makers = ['pylint']
" let g:neomake_latex_enabled_makers = ['']
" let g:neomake_python_pylint_maker = {
"     \ 'args' : [ '-d', 'R0913,R0904,C1801,C0102,C0103,C0415,W1201,W1401,C0302,C0305,C0312,R0902,W0612,W0212,E1101',
"     \            '-r', 'n'],
"     \}
" let g:neomake_c_enabled_makers = ['gcc']
" let g:neomake_cpp_enabled_makers = ['g++']
" let g:neomake_sh_enabled_makers = ['shellcheck']
" let g:neomake_fortran_enabled_makers = ['gfortran']
" let g:neomake_fortran_gfortran_maker = {
"             \'args': ['-fsyntax-only', '-Wall', '-Wextra', '-cpp']
"             \}
" " Custom warning sign
" let g:neomake_warning_sign = {
"     \   'text': '!',
"     \   'texthl': 'NeomakeWarningSign',
"     \ }
" let g:neomake_error_sign = {
"     \ 'text': 'X',
"     \ 'texthl': 'NeomakeErrorSign',
"     \ }
" Automatically trigger neomake only when the laptop is charging 
function! MyOnBattery()
    if has("unix")
        let s:uname = system("uname")
        if s:uname == "Darwin\n"
            return system("pmset -g batt | awk '/charg/ {print $4}'") != "discharging;\n"
        elseif s:uname == "Linux\n"
            if filereadable('/sys/class/power_supply/AC/online')
                return readfile('/sys/class/power_supply/AC/online') == ['0']
            else
                return 1
            endif
        endif
    endif
endfunction
" if MyOnBattery()
"   call neomake#configure#automake('rnw', 100)
" endif
" also use key mapping in normal mode
" noremap <Leader>nm :Neomake<CR>

" ===== lightline =====
set noshowmode
let g:lightline = {
        \ 'colorscheme': 'powerline',
        \ 'active': {
        \     'left': [['mode', 'paste'], ['gitbranch','filename', 'modified']],
        \     'right': [['lineinfo'], ['fileencoding', 'filetype'], ['percent'], ['readonly', 'linter_warnings', 'linter_errors']]
        \ },
        \ 'component_expand': {
        \     'linter_warnings': 'LightlineLinterWarnings',
        \     'linter_errors': 'LightlineLinterErrors'
        \ },
        \ 'component_type': {
        \     'readonly': 'error',
        \     'linter_warnings': 'warning',
        \     'linter_errors': 'error'
        \ },
        \ 'component_function': {
        \     'gitbranch': 'fugitive#head',
        \ },
        \ }
" " add neomake support
" function! LightlineLinterWarnings() abort
"     let l:counts = neomake#statusline#LoclistCounts()
"     let l:warnings = get(l:counts, 'W', 0)
"     return l:warnings == 0 ? '' : printf('%d ◆', l:warnings)
" endfunction
"
" function! LightlineLinterErrors() abort
"     let l:counts = neomake#statusline#LoclistCounts()
"     let l:errors = get(l:counts, 'E', 0)
"     return l:errors == 0 ? '' : printf('%d ✗', l:errors)
" endfunction
" " Ensure lightline update after neomake is done
" autocmd! User NeomakeFinished call lightline#update()

" ===== Surrounding =====
"nnoremap <Leader>q yss

" ===== Comfortable motion =====
"let g:comfortable_motion_friction = 80.0
"let g:comfortable_motion_air_drag = 1.0

" ===== vim-qf =====
"let g:qf_nowrap = 0
" I disable vim-qf for the quickfix window appear in the bottom of
" Vim+NERDTree+Tagbar, instead of the main vim window only.
" Instead, the function below is defined to automatically close the location 
" window if it is the last window
" from https://github.com/neomake/neomake/issues/1455
augroup my_neomake_qf
    autocmd!
    autocmd QuitPre * if &filetype !=# 'qf' | lclose | endif
augroup END
" ==================

" ===== NerdTree =====
let NERDTreeChDirMode = 1
let NERDTreeShowBookmarks = 1
let NERDTreeNaturalSort = 1
let NERDTreeWinSize = 25
let NERDTreeShowHidden = 1
let NERDTreeIgnore = ['\~$',  '\.pyc$', '\.swp$', '\.git$','\.o','\.mod$',
            \ '__pycache__$', '.pytest_cache', '.python-version', '.coverage$',
            \ '.DS_Store', '\.eps$', '\.png', '\.jpg', '\.jpeg']
let NERDTreeQuitOnOpen = 0
let NERDTreeStatusline = "%{matchstr(getline('.'), '\\s\\zs\\w\\(.*\\)')}"
nnoremap <Leader>nt :NERDTreeFind<CR><C-w>l
nnoremap <Leader>nc :NERDTreeClose<CR>

" functions from https://gist.github.com/avesus/1954d9384d86cc1e39cb2b2eff7017b7
" calls NERDTreeFind iff NERDTree is active, current window contains a modifiable file, and we're not in vimdiff
function! s:syncTree()
  let s:curwnum = winnr()
  NERDTreeFind
  exec s:curwnum . "wincmd w"
endfunction

function! s:syncTreeIf()
  if (winnr("$") > 1)
    call s:syncTree()
  endif
endfunction
"" Shows NERDTree on start and synchronizes the tree with opened file when switching between opened windows
"autocmd BufEnter * call s:syncTreeIf()
"" Focus on opened view after starting (instead of NERDTree)
"autocmd VimEnter * call s:syncTree()
"autocmd VimEnter * :wincmd w
"" Auto refresh NERDTree files
"autocmd CursorHold,CursorHoldI * if (winnr("$") > 1) 
"    \ | call NERDTreeFocus() 
"    \ | call g:NERDTree.ForCurrentTab().getRoot().refresh() 
"    \ | call g:NERDTree.ForCurrentTab().render() 
"    \ | wincmd w | endif

" ===== Tabular =====
" align in normal and visual mode
" by using a= to align equal mark
nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
" by using a: to align after colon mark
nmap <Leader>a: :Tabularize /:\zs<CR>
vmap <Leader>a: :Tabularize /:\zs<CR>
" by using a\, (escape) to align comma mark
nmap <Leader>a\, :Tabularize /,<CR>
vmap <Leader>a\, :Tabularize /,<CR>

" ===== indentLine =====
let g:indentLine_char = "┆"
let g:indentLine_enabled = 1
let g:autopep8_disable_show_diff=1
let g:indentLine_concealcursor = ""

" =========================================================
" General
" =========================================================
" use lazyredraw if the scroll is very slow
set nolazyredraw
" relative number is managed by the number plugin
set number
"set rnu
set so=1
set autoindent
set smartindent
set smartcase
set ignorecase
set nowrap
" Disable insert paste mode
set nopaste
" 忽略编译文件
set wildignore=*.o,*~,*.pyc,*.mod
"set shell=/bin/bash
"set foldmethod=syntax

" Restore cursor position
au BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" |
    \ execute("normal `\"") |
  \ endif

" =========================================================
" Key Mappings
" =========================================================
" Disable arrows, habbit destroying
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
imap jk <esc>
vmap mm <esc>
" 命令模式启动
nnoremap : ;
nnoremap ; :
" Faster resizing
" vertical
nnoremap <silent> =w <C-w>4+
nnoremap <silent> -w <C-w>4-
" horizontal
nnoremap <silent> ==w <C-w>4<
nnoremap <silent> --w <C-w>4>

" search result in the middle of screen
nnoremap n nzz
nnoremap N Nzz

" =========================================================
" Tab and indentation control
" =========================================================
set noexpandtab " Insert tabs rather than spaces for <Tab>.
set smarttab " Tab respects 'tabstop', 'shiftwidth', and 'softtabstop'.
set tabstop=4 " The visible width of tabs.
set softtabstop=4 " Edit as if the tabs are 4 characters wide.
set shiftwidth=4 " Number of spaces to use for indent and unindent.
set shiftround " Round indent to a multiple of 'shiftwidth'.

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
    autocmd FileType go         setlocal ts=2 sw=2 expandtab
    autocmd Filetype scss       setlocal ts=2 sw=2 expandtab
    autocmd Filetype css        setlocal ts=2 sw=2 expandtab
    autocmd Filetype php        setlocal ts=4 sw=4 expandtab
    autocmd Filetype sql        setlocal ts=4 sw=4 expandtab
    autocmd Filetype java       setlocal ts=4 sw=4 expandtab
    autocmd Filetype lisp       setlocal ts=2 sw=2 expandtab
    autocmd Filetype {yaml,toml} setlocal ts=2 sw=2 expandtab
    autocmd Filetype java       colorcolumn=121
augroup END

" remember folding state
augroup remember_folds
  autocmd!
  "autocmd BufWinLeave * mkview
  " exclude quickfix to avoid error:
  " Error detected while processing BufWinLeave Autocommands for "*"
  autocmd BufWinLeave * if &filetype !=# 'qf' | mkview | endif
  autocmd BufWinEnter * silent! loadview
augroup END

" always use free-format fortran
let fortran_free_source=1

" =========================================================
" Header part
function! HeaderPython()
    call setline(1, "#!/usr/bin/env python3")
    call append(1, "# -*- coding: utf-8 -*-")
    normal G
    normal o
endf
function! HeaderBash()
    call setline(1, "#!/usr/bin/env bash")
    normal G
    normal o
endf
function! HeaderTCSH()
    call setline(1, "#!/usr/bin/env tcsh")
    normal G
    normal o
endf
function! HeaderORG()
    call setline(1, "#+title: ".expand('%:r:t'))
    call setline(2, "#+created: [".strftime('%Y-%m-%d %a %H:%M')."]")
    call setline(3, "#+author: Min-Ye Zhang")
    normal G
    normal o
endf
function! HeaderC()
    call setline(1, "/*")
    call append(1,  ' * Date  : '.strftime('%Y-%m-%d %H:%M:%S'))
    call append(2,  " * Author: Min-Ye Zhang")
    call append(3,  " * Usage : ")
    call append(4,  " * TODO  : ")
    call append(5,  " */")
    normal G
    normal o
endf
function! HeaderTeX()
    call setline(1, "% !TeX root = ")
    call append(1, "\\documentclass{article}")
    call append(2, "\\usepackage{mwe}")
    call append(3, "\\begin{document}")
    call append(4, "\\author{Min-Ye Zhang}")
    call append(5, "")
    call append(6, "\\end{document}")
    normal G
    normal o
endf

augroup my_fileheaders
  autocmd!
  autocmd bufnewfile *.sh call HeaderBash()
  autocmd bufnewfile *.{tsh,tcsh,csh} call HeaderTCSH()
  autocmd bufnewfile *.py call HeaderPython()
  autocmd bufnewfile *.{c,cpp} call HeaderC()
  autocmd bufnewfile *.{tex,latex} call HeaderTeX()
  autocmd bufnewfile *.org call HeaderORG()
augroup END

" =========================================================
" remove trailing space
"autocmd BufWritePre *.{f90,py,c,cpp,tex,md,rst} %s/\s\+$//e
" do not apply to Fortran/C/CPP source code since it will make git dirty
autocmd BufWritePre *.{py,tex,md,rst} %s/\s\+$//e

" per-project vimrc
" from https://blog.binaryminer.com/2018/03/29/Per-project-configuration-in-Vim/
let vimrc_filename = ".vimrc"
let tags_filename = "tags"
" Start looking for .vimrc files from the root dir
let local_path = "/"
let current_path = getcwd()
" If the current path is a child of $HOME directory, start looking from $HOME instead
if current_path =~ $HOME
    let local_path = $HOME . local_path
    let current_path = substitute(current_path, $HOME, '', '')
endif
let path_parts = split(current_path, "/")
for path_part in path_parts
    let local_path = local_path . path_part . "/"
    if filereadable(local_path . vimrc_filename)
        exe ":so " . local_path . vimrc_filename
    endif
    if filereadable(local_path . tags_filename)
        exe ":set tags+=" . local_path . tags_filename
    endif
endfor
unlet path_parts current_path local_path tags_filename vimrc_filename
