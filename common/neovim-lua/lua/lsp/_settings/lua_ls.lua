local M = {}

M.settings = {
  -- sumneko_lua lsp configurations
  Lua = {
    diagnostics = {
    -- https://www.reddit.com/r/neovim/comments/khk335/lua_configuration_global_vim_is_undefined/
      globals = { 'vim', 'use', 'USER_DIR' }
    }
  }
}

return M
