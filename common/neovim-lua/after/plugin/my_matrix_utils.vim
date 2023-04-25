if exists('g:loaded_my_matrix_utils') | finish | endif " prevent loading file twice

" TODO:
" remove too small values in matrix-market exchange format, reset number of
" elements
" g/e-\(09\|[1-9][0-9]\) -\?[0-9.]\+e-\(09\|[1-9][0-9]\)/d

let g:loaded_my_matrix_utils = 1
