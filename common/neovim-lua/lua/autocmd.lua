-- autocommands setup
vim.cmd [[
" remember fold state on exit
" https://stackoverflow.com/questions/37552913/vim-how-to-keep-folds-on-save
" Note: use silent mkview to get rid of E32: no file name when using telescope,
"       or use a *.* pattern instead of *. The later is used here
  let fold_f_pattern = '*.*'
  augroup _remember_folds
    autocmd!
    autocmd BufWinLeave fold_f_pattern silent! mkview
    autocmd BufWinEnter fold_f_pattern silent! loadview
  augroup END
" fix no folding when open with telescope
" https://github.com/nvim-telescope/telescope.nvim/issues/559
" from https://github.com/ravenxrz/dotfiles/blob/master/nvim/lua/user/conf/autocommands.lua
  augroup _fold_bug_solution
    autocmd!
    autocmd BufRead fold_f_pattern autocmd BufWinEnter fold_f_pattern ++once normal! zx
  augroup end
]]
