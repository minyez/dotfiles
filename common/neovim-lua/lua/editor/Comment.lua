-- https://github.com/numToStr/Comment.nvim
local status_ok, config = pcall(require, "Comment")
if not status_ok then
  vim.notify("Comment not found!")
  return
end

config.setup({})
