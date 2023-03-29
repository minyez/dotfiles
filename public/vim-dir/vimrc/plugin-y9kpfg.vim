set nocompatible
filetype off

" ============================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

" ============================
call vundle#begin()
" 安装所需插件
Plugin 'VundleVim/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'kien/ctrlp.vim'
Plugin 'salsifis/vim-transpose'
Plugin 'editorconfig/editorconfig-vim'
"Plugin 'tpope/vim-sensible'
""Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
""Plugin 'tpope/vim-commentary'
Plugin 'jiangmiao/auto-pairs'
Plugin 'ervandew/supertab'
"Plugin 'bling/vim-airline'
"Plugin 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'godlygeek/tabular'
""Plugin 'airblade/vim-gitgutter'
""Plugin 'raimondi/delimitmate'
"Plugin 'scrooloose/nerdtree'
"Plugin 'wincent/command-t'
""" syntax related
"Plugin 'scrooloose/syntastic'
""Plugin 'justinmk/vim-syntax-extra'
"Plugin 'majutsushi/tagbar'
""Plugin 'octol/vim-cpp-enhanced-highlight'
""" Snippets related
Plugin 'sirver/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Yggdroot/indentLine'
Plugin 'neomake/neomake'
"" auto-compeletion
"Plugin 'Shougo/neocomplete.vim'
"Plugin 'Shougo/neocomplcache'
"Plugin 'ajh17/VimCompletesMe'
"Plugin 'valloric/youcompleteme'
"Plugin 'tomtom/tcomment_vim'
"Plugin 'plasticboy/vim-markdown'
"" deoplete and jedi
"if has('nvim')
"  Plugin 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"else
"  Plugin 'Shougo/deoplete.nvim'
"  Plugin 'roxma/nvim-yarp'
"  Plugin 'roxma/vim-hug-neovim-rpc'
"endif
"Plugin 'zchee/deoplete-jedi',
Plugin 'davidhalter/jedi-vim'

call vundle#end()
" ============================
filetype plugin indent on
filetype plugin on

" ===== vim-colorschemes =====
"colorscheme molokai
colorscheme jellybeans
" alternatively, use the color of terminal GUI
" require ViM 8.0
set termguicolors

" ===== deoplete =====
"let g:deoplete#enable_at_startup = 1
"" Pass a dictionary to set multiple options
"call deoplete#custom#source('ultisnip', 'matchers', ['matcher_fuzzy'])
"call deoplete#custom#option({
"\ 'smart_case' : v:true,
"\ })
"let g:deoplete#auto_complete=1
" ===== jedi =====
" disable autocompletion, cause we use deoplete for completion
let g:jedi#completions_enabled = 0
" open the go-to function in split, not another buffer
let g:jedi#use_splits_not_buffers = "right"

" ==== UltiSnips ====
set runtimepath+=~/.vim/bundle/ultisnips
let g:UltiSnipsSnippetDirectories = [$HOME.'/snippets/UltiSnips']
"let g:UltiSnipsEditSplit = 'vertical'
"let g:UltiSnipsUsePythonVersion=3
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

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

" ==== Tabular ====
" align : in normal and visual mode
" by using a= to align equal mark
nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
" by using a: to align after colon mark
nmap <Leader>a: :Tabularize /:\zs<CR>
vmap <Leader>a: :Tabularize /:\zs<CR>
" by using a\, (escape) to align comma mark
nmap <Leader>a\, :Tabularize /,<CR>
vmap <Leader>a\, :Tabularize /,<CR>

" ==== Plugin indentLine settings. ====
let g:indentLine_char = "┆"
let g:indentLine_enabled = 1
let g:autopep8_disable_show_diff=1

"""""""""""""""""""""""
" Doxygen Plugin
"""""""""""""""""""""""
"doxygen toolkit 
let g:DoxygenToolkit_briefTag_pre="@brief " 
let g:DoxygenToolkit_paramTag_pre="@param " 
let g:DoxygenToolkit_returnTag="@returns " 
let g:DoxygenToolkit_fileTag="@file " 
let g:DoxygenToolkit_blockHeader="-----------------------------------" 
let g:DoxygenToolkit_blockFooter="-----------------------------------" 
let g:DoxygenToolkit_authorName="Min-Ye Zhang, stevezhang@pku.edu.cn" 
let s:licenseTag = "\<enter>" 
let s:licenseTag = s:licenseTag . "TODO:\<enter>" 
let s:licenseTag = s:licenseTag . "\<enter>" 
let g:DoxygenToolkit_licenseTag = s:licenseTag 

let g:DoxygenToolkit_briefTag_funcName="no" 
let g:doxygen_enhanced_color=1 
let g:DoxygenToolkit_commentType="C" 
let g:DoxygenToolkit_classTag = "@class " 

"自定义快捷键
"vmap <C-S-P>    dO#endif<Esc>PO#if 0<Esc>
"map <F12> <Esc>:Dox<cr>
"map <F10> <Esc>:DoxAuthor<cr>
"map <F11> <Esc>:DoxLic<cr>
"map <F4>b :DoxBlock<CR>
"map <F4>l :DoxLic<CR>
"map <F4>c odocClass<C-B>
"map <F4>m odocMember<C-B>
"map <F9> :DoxBlock<CR>
