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
  augroup end
" fix no folding when open with telescope
" https://github.com/nvim-telescope/telescope.nvim/issues/559
" from https://github.com/ravenxrz/dotfiles/blob/master/nvim/lua/user/conf/autocommands.lua
  augroup _fold_bug_solution
    autocmd!
    autocmd BufRead fold_f_pattern autocmd BufWinEnter fold_f_pattern ++once normal! zx
  augroup end
" set filetype of .ext.in file to ext
" but will lead to lsp error: unable to handle compilation, expected exactly one compiler job in '' clang
" check https://neovim.discourse.group/t/how-to-make-clangd-support-more-file-types/1907/4
" problem with: BufEnter, BufRead, BufNewFile. either set or setlocal
" not work: BufReadPre, BufAdd, BufWinEnter
" TODO: check whether it is universal or only on macos
"  augroup _set_filetype_in
"    autocmd!
"    autocmd BufEnter *.{cpp,hpp}.in set filetype=cpp
"    autocmd BufEnter *.{c,h}.in set filetype=c
"    autocmd BufEnter *.{f,f90}.in set filetype=fortran
"  augroup end

" Use fprettify to format fortran code, if the executable exists
"  if executable('fprettify')
"    augroup _format_fortran
"      autocmd!
"      autocmd Filetype fortran setlocal formatprg=fprettify\ --silent
"    augroup end
"  endif
]]

-- TODO: use lua api for autocmd, see eg
-- https://github.com/Jarmos-san/dotfiles/blob/main/dotfiles/.config/nvim/lua/autocmds.lua
-- vim.api.nvim_create_autocmd
-- vim.api.nvim_create_augroup("augroup name", { clear = true })
