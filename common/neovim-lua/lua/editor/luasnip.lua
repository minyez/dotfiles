-- luasnip: code snippets control
-- https://github.com/L3MON4D3/LuaSnip

local snip_status_ok, luasnip = pcall(require, "luasnip")
if not snip_status_ok then
  vim.notify("luasnip not found!")
  return
end

require("luasnip.loaders.from_vscode").lazy_load()
require("luasnip.loaders.from_vscode").lazy_load({ paths = { -- load custom snippets
  vim.fn.stdpath("config") .. "/snippets"
} }) -- Load snippets from ~/.config/nvim/snippets folder
