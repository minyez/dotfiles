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
colorscheme woju
" alternatively, use the color of terminal GUI
" require ViM 8.0
"set termguicolors

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
let g:UltiSnipsSnippetsDir = '~/.vim/UltiSnips'
"let g:UltiSnipsEditSplit = 'vertical'
"let g:UltiSnipsUsePythonVersion=3
" if trigger not working, add these lines
" make YCM compatible with UltiSnips (using supertab)
"let g:ycm_key_list_select_completion = ['<C-n>','<Down>']
"let g:ycm_key_list_previous_completion = ['<C-p>','<Up>']
"let g:SuperTabDefaultCompletionType = '<C-n>'
"let g:ycm_path_to_python_interpreter = '/usr/local/Cellar/python@2/2.7.14_3/bin/python2'
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" ==== powerline ====
"set laststatus=2


"" ==== neomake =====
"let g:neomake_open_list = 0
"let g:neomake_serialize = 1
"let g:neomake_list_height = 6
"let g:neomake_serialize_abort_on_error = 1
""disabled open error list in a window. used to be 2 to show error in location window
"let g:neomake_python_enabled_makers = ['pylint']
"let g:neomake_python_pylint_maker = {
"    \ 'args' : [ '-d', 'R0913, C1801, C0103, W1401, C0303, C0305, R0902, W0612, W0212, E1101', '-r', 'n' ],
"    \}
"let g:neomake_sh_enabled_makers = ['shellcheck']

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
"if MyOnBattery()
"  call neomake#configure#automake('rnw', 1000)
"endif

" ==== YCM ====
"let g:ycm_server_python_interpreter = '/usr/local/Cellar/python@2/2.7.14_3/bin/python2'
"set completeopt=longest,menu
"let g:ycm_min_num_of_chars_for_completion=2
"let g:ycm_cache_omnifunc=0
"let g:ycm_seed_identifiers_with_syntax=1
"let g:ycm_complete_in_comments = 1
"let g:ycm_complete_in_strings = 1
"let g:ycm_collect_identifiers_from_comments_and_strings = 0
"let g:ycm_global_ycm_extra_conf = '~/.vim/vimrc/.ycm_extra_conf.py'

" ==== NerdTree ====
"let NERDTreeChDirMode=1
"let NERDTreeShowBookmarks=1
"let NERDTreeWinSize=25
"let NERDTreeShowHidden=1
"let NERDTreeIgnore=['\~$', '\.pyc$', '\.swp$', '\.git$','\.o','\.mod']
"let NERDTreeQuitOnOpen=1

" ==== syntastic ====
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 0
"let g:syntastic_check_on_wq = 1
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_enable_signs=1
"let g:syntastic_cpp_check_header = 1
"let g:syntastic_cpp_remove_include_errors = 1
"let g:syntastic_mode_map = {'mode': 'passive', 'active_filetypes': [], 'passive_filetypes': []}
""" Use pylint to check python files.
"let g:syntastic_python_checkers = ['pylint']
"map <F5> :SyntasticToggleMode<CR> :SyntasticCheck<CR>
""" Ignore warnings about newlines trailing.
"let g:syntastic_quiet_messages = { 'regex': ['trailing-newlines', 'invalid-name',
"    \'too-many-lines', 'too-many-instance-attributes', 'too-many-public-methods',
"    \'too-many-locals', 'too-many-branches'] }
" 关闭警告
"let g:syntastic_quiet_messages = { 'level': 'warnings' }


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
