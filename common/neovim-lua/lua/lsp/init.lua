-- original from
local status_ok, lspconfig = pcall(require, "lspconfig")
if not status_ok then
  vim.notify("lspconfig is not found!")
  return
end

-- require(USER_DIR .. ".lsp.installer") -- deprecated 2022-12-09, use mason
require(USER_DIR .. ".lsp.mason")

local handler = require(USER_DIR .. ".lsp.handlers")
handler.setup()

require(USER_DIR .. ".lsp.signature")
-- require(USER_DIR .. '.lsp.null-ls') -- not work

-- function for toggling LSP diagnostics per buffer
-- use
--   lua.toggle_diagnostics()
--     or
--   :lua toggle_diagnostics()
-- https://github.com/neovim/neovim/issues/14825#issuecomment-1017482249
vim.g.diagnostics_visible = true
function _G.toggle_diagnostics()
  if vim.g.diagnostics_visible then
    vim.g.diagnostics_visible = false
    vim.diagnostic.disable()
  else
    vim.g.diagnostics_visible = true
    vim.diagnostic.enable()
  end
end
