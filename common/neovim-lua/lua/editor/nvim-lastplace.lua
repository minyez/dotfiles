local cmp_status_ok, configs = pcall(require, "nvim-lastplace")
if not cmp_status_ok then
  vim.notify("cmp not found!")
  return
end

configs.setup {
  lastplace_ignore_buftype = {"quickfix", "nofile", "help"},
  lastplace_ignore_filetype = {"gitcommit", "gitrebase", "svn", "hgcommit"},
  lastplace_open_folds = true
}
