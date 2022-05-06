-- https://github.com/stevearc/aerial.nvim
local pkg = "aerial"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({

})

