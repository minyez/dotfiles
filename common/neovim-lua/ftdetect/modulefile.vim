" adapted from modules/share
" must put into an autogroup
augroup det_modulefile
  autocmd!
  autocmd BufNewFile,BufRead *
      \ if (getline(1) =~? "^#%Module") |
      \     set filetype=modulefile |
      \ endif
augroup END
