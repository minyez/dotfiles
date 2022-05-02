-- https://github.com/karb94/neoscroll.nvim
local pkg = "neoscroll"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({
  easing_function = "quintic", -- quadratic, cubic, quartic, quintic, circular, sine
})

