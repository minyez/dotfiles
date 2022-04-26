-- https://github.com/folke/which-key.nvim
local pkg = "which-key"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({})

-- register keymapping
-- configs.register(mappings, opts)
