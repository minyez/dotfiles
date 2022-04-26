local M = {}

M.lsp = {
  -- sumneko_lua lsp configurations
  diagnostics = {
  -- https://www.reddit.com/r/neovim/comments/khk335/lua_configuration_global_vim_is_undefined/
    globals = { 'vim', 'use' }
  }
}

return M
