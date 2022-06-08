local pkg = "indent_blankline"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({
  -- use solid line to further highlight current block
  char_list = { "┆", "|" },
  -- for example, context is off by default, use this to turn it on
  show_current_context = true,
  show_current_context_start = true,
})
