local status_ok, mason = pcall(require, "mason")
if not status_ok then
  vim.notify("mason not found!")
  return
end

local status_ok, mason_lsp_config = pcall(require, "mason-lspconfig")
if not status_ok then
  vim.notify("mason-lspconfig not found!")
  return
end

mason.setup({})
mason_lsp_config.setup({})

local lspconfig = require("lspconfig")
local handler = require(USER_DIR .. ".lsp.handlers")

-- global setup
mason_lsp_config.setup_handlers {
  function (server_name) -- default handler (optional)
    lspconfig[server_name].setup {
      on_attach = handler.on_attach,
      capabilities = handler.capabilities,
    }
  end,
}

local server_settings = require(USER_DIR .. ".lsp._settings")

-- renew for local
for name, settings in pairs(server_settings) do
  mason_lsp_config.setup_handlers {
    [name] = function()
      lspconfig[name].setup(
      {
        on_attach = handler.on_attach,
        capabilities = handler.capabilities,
        settings = settings,
      })
    end
  }
end
