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

-- local status_ok, mason_tool_installer = pcall(require, "mason-tool-installer")
-- if status_ok then
--   mason_tool_installer.setup({
--     ensure_installed =
--     {
--       'bash-debug-adapter',
--       'bash-language-server',
--       'clangd',
--       'clang-format',
--       'cmakelang',
--       'fortls',
--       'json-lsp',
--       'lua-language-server',
--       'marksman',
--       'neocmakelsp',
--       'python-lsp-server',
--       'rust-analyzer',
--       'rustfmt',
--       'shellcheck',
--       'shellharden',
--       'stylua',
--       'yamlfmt',
--       'yaml-language-server',
--       'yapf',
--     }})
-- end

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
      local setup = {
        on_attach = handler.on_attach,
        capabilities = handler.capabilities,
      }
      setup = vim.tbl_deep_extend("force", settings, setup)
      lspconfig[name].setup(setup)
    end
  }
end
