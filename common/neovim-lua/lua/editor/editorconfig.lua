-- https://github.com/gpanders/editorconfig.nvim
local pkg = "editorconfig"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end
