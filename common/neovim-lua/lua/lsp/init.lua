-- original from
local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

local dir = ""

require(dir .. "lsp.installer")
require(dir .. "lsp.handlers").setup()
