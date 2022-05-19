local pkg = "cmake"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({})
