local pkg = "ibl"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({
  indent = {
   char = "┆",
  },
  scope = { enabled = true },
  -- char = { "┆", },
  -- char_list = { "┆", },
  -- -- for example, context is off by default, use this to turn it on
  -- show_current_context = true,
  -- show_current_context_start = true,
  -- space_char_blankline = " ",
  -- show_end_of_line = true,
})
