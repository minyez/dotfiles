-- https://fortls.fortran-lang.org/editor_integration.html#neovim
local M = {
  cmd = {
    'fortls',
    '--lowercase_intrinsics',
    '--hover_signature',
    '--hover_language=fortran',
    '--use_signature_help'
  }
}

return M
