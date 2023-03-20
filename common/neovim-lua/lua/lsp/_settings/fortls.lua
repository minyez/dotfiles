local M = {
  settings = {
    fortls = {
      cmd = {
        'fortls',
        -- '--lowercase_intrisics',
        '--hover_signature',
        '--hover_language=fortran',
        '--use_signature_help'
      }
    }
  }
}

return M
