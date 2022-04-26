local colorscheme = "neon"
local colorscheme = "onedark"
local colorscheme = "kanagawa"

local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
if not status_ok then
  vim.notify("colorscheme " .. colorscheme .. " not found!")
  return
end

require ("_colorschemes." .. colorscheme)