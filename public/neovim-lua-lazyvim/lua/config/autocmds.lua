-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- ==================
-- Autoformat
-- ==================
-- Enable global config (the default in LazyVim)
-- vim.g.autoformat = true

-- Buffer-local config
-- from https://github.com/LazyVim/LazyVim/discussions/141#discussioncomment-9266704
local set_autoformat = function(pattern, bool_val)
  vim.api.nvim_create_autocmd({ "FileType" }, {
    pattern = pattern,
    callback = function()
      vim.b.autoformat = bool_val
    end,
  })
end

set_autoformat({ "c" }, false)
set_autoformat({ "cpp" }, false)
set_autoformat({ "fortran" }, false)
set_autoformat({ "lua" }, true)
set_autoformat({ "python" }, false)

-- ===========
-- Indentation
-- ===========
vim.cmd([[
augroup auto_language_selection
    autocmd!
    autocmd Filetype c          setlocal ts=4 sw=4 expandtab
    autocmd Filetype latex      setlocal ts=2 sw=2 expandtab
    autocmd Filetype plaintex   setlocal ts=2 sw=2 expandtab
    autocmd Filetype cpp        setlocal ts=4 sw=4 expandtab
    autocmd Filetype fortran    setlocal ts=3 sw=3 expandtab
    autocmd Filetype python     setlocal ts=4 sw=4 expandtab
augroup END
]])
