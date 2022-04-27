-- original from
local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end

local dir = "lsp"

require(dir .. ".installer")
require(dir .. ".handlers").setup()
