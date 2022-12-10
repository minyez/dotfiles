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
-- mason_lsp_config.setup({})

-- mason_lsp_config.setup_handlers {
  -- The first entry (without a key) will be the default handler
  -- and will be called for each installed server that doesn't have
  -- a dedicated handler.
  -- function (server_name) -- default handler (optional)
  --   lspconfig[server_name].setup {}
  -- end,
  -- Next, you can provide a dedicated handler for specific servers.
  -- For example, a handler override for the `rust_analyzer`:
  -- ["sumneko_lua"] = function ()
  --   lspconfig.sumneko_lua.setup(servers["sumneko_lua"])
  -- end,
  -- ["pylsp"] = function ()
  --   lspconfig.pylsp.setup(servers["pylsp"])
  -- end
-- }
