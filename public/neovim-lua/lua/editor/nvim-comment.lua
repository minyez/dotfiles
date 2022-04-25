local status_ok, nvim_comment = pcall(require, "nvim_comment")
if not status_ok then
  vim.notify("nvim_comment not found!")
  return
end

nvim_comment.setup({})
