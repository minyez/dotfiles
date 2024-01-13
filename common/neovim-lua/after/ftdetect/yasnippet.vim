" adapted from modules/share
" must put into an autogroup
augroup filetype_yasnippet
  autocmd!
  autocmd BufNewFile,BufRead *
      \ if (getline(1) =~? "mode: snippet") |
      \     set filetype=yasnippet |
      \ endif
augroup END
