-- original from
local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

require(USER_DIR .. ".lsp.installer")
require(USER_DIR .. ".lsp.handlers").setup()
