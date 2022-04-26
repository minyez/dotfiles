local status_ok, onedark = pcall(require, "neon")
if not status_ok then
  vim.notify("neon theme not found!")
  return
end

vim.g.neon_style = "default"
vim.g.neon_italic_keyword = true
vim.g.neon_italic_function = true
vim.g.neon_transparent = true

vim.cmd[[colorscheme neon]]

