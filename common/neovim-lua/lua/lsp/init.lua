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

