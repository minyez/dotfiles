-- original from
local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  vim.notify("lspconfig is not found!")
  return
end

require(USER_DIR .. ".lsp.installer")
require(USER_DIR .. ".lsp.handlers").setup()
require(USER_DIR .. ".lsp.signature")
-- require(USER_DIR .. '.lsp.null-ls') -- not work

