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

local function get_server_settings(name)
  local status, setting = pcall(require, USER_DIR .. ".lsp._settings." .. name)
  if status then
    return setting.settings
  end
  return {}
end

local servers = {
  "sumneko_lua",
  "pylsp",
  "clangd",
  "texlab",
  "fortls",
}

for _, name in ipairs(servers) do
  lspconfig[name].setup({
    on_attach = handler.on_attach,
    capabilities = handler.capabilities,
    settings = get_server_settings(name),
  })
end

require(USER_DIR .. ".lsp.signature")
-- require(USER_DIR .. '.lsp.null-ls') -- not work

