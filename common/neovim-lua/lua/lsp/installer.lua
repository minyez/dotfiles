local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not status_ok then
  vim.notify("nvim-lsp-installer not found!")
  return
end

-- Register a handler that will be called for all installed servers.
-- Alternatively, you may also register handlers on specific server instances instead (see example below).
lsp_installer.on_server_ready(function(server)
  local opts = {
    on_attach = require(USER_DIR .. ".lsp.handlers").on_attach,
    capabilities = require(USER_DIR .. ".lsp.handlers").capabilities,
    settings = {}
  }

  -- server dependent
  -- for reference see https://youtu.be/6F3ONwrCxMg?t=23m20s
  local status, opts_extra = pcall(require, USER_DIR .. ".lsp.server." .. server.name)
  if status then
    opts = vim.tbl_deep_extend("force", opts_extra, opts)
  end

  -- This setup() function is exactly the same as lspconfig's setup function.
  -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
  server:setup(opts)
end)
