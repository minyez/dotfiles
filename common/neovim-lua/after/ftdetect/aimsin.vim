" must put into an autogroup
augroup filetype_aimsin
  autocmd!
  autocmd BufNewFile,BufRead geometry*.in,geometry.in[_.]*,control*.in,control.in[_.]* set filetype=aimsin
  autocmd BufNewFile,BufRead *
      \ if (getline(1) =~? "^#%FHI-aims") || (getline(3) =~? "^#  FHI-aims code project") |
      \     set filetype=aimsin |
      \ endif
augroup END
