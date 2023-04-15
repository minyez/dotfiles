-- https://github.com/kyazdani42/nvim-tree.lua
-- 
-- <CR> or o on the root folder will cd in the above directory
-- <C-]> will cd in the directory under the cursor
-- <BS> will close current opened directory or parent
-- type a to add a file. Adding a directory requires leaving a leading / at the end of the path.
-- you can add multiple directories by doing foo/bar/baz/f and it will add foo bar and baz directories and f as a file
-- 
-- type r to rename a file
-- type <C-r> to rename a file and omit the filename on input
-- type x to add/remove file/directory to cut clipboard
-- type c to add/remove file/directory to copy clipboard
-- type y will copy name to system clipboard
-- type Y will copy relative path to system clipboard
-- type gy will copy absolute path to system clipboard
-- type p to paste from clipboard. Cut clipboard has precedence over copy (will prompt for confirmation)
-- type d to delete a file (will prompt for confirmation)
-- type D to trash a file (configured in setup())
-- type ]c to go to next git item
-- type [c to go to prev git item
-- type - to navigate up to the parent directory of the current file/directory
-- type s to open a file with default system application or a folder with default file manager (if you want to change the command used to do it see :h nvim-tree.setup under system_open)
-- if the file is a directory, <CR> will open the directory otherwise it will open the file in the buffer near the tree
-- if the file is a symlink, <CR> will follow the symlink (if the target is a file)
-- <C-v> will open the file in a vertical split
-- <C-x> will open the file in a horizontal split
-- <C-t> will open the file in a new tab
-- <Tab> will open the file as a preview (keeps the cursor in the tree)
-- I will toggle visibility of hidden folders / files
-- H will toggle visibility of dotfiles (files/folders starting with a .)
-- R will refresh the tree
-- Double left click acts like <CR>
-- Double right click acts like <C-]>
-- W will collapse the whole tree
-- S will prompt the user to enter a path and then expands the tree to match the path
-- . will enter vim command mode with the file the cursor is on
-- C-k will toggle a popup with file infos about the file under the cursor
--
-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`
local status_ok, nvim_tree = pcall(require, "nvim-tree")
if not status_ok then
  vim.notify("nvim-tree not found!")
  return
end

nvim_tree.setup {
  update_focused_file = {
    enable = true,
    update_root = false,
    ignore_list = {},
  },
}

-- Default mapping defined in nvim-tree.lua/lua/actions/init.lua
-- local M = {
--   mappings = {
--     { key = { "<CR>", "o", "<2-LeftMouse>" }, action = "edit" },
--     { key = "<C-e>", action = "edit_in_place" },
--     { key = "O", action = "edit_no_picker" },
--     { key = { "<2-RightMouse>", "<C-]>" }, action = "cd" },
--     { key = "<C-v>", action = "vsplit" },
--     { key = "<C-x>", action = "split" },
--     { key = "<C-t>", action = "tabnew" },
--     { key = "<", action = "prev_sibling" },
--     { key = ">", action = "next_sibling" },
--     { key = "P", action = "parent_node" },
--     { key = "<BS>", action = "close_node" },
--     { key = "<Tab>", action = "preview" },
--     { key = "K", action = "first_sibling" },
--     { key = "J", action = "last_sibling" },
--     { key = "I", action = "toggle_git_ignored" },
--     { key = "H", action = "toggle_dotfiles" },
--     { key = "R", action = "refresh" },
--     { key = "a", action = "create" },
--     { key = "d", action = "remove" },
--     { key = "D", action = "trash" },
--     { key = "r", action = "rename" },
--     { key = "<C-r>", action = "full_rename" },
--     { key = "x", action = "cut" },
--     { key = "c", action = "copy" },
--     { key = "p", action = "paste" },
--     { key = "y", action = "copy_name" },
--     { key = "Y", action = "copy_path" },
--     { key = "gy", action = "copy_absolute_path" },
--     { key = "[c", action = "prev_git_item" },
--     { key = "]c", action = "next_git_item" },
--     { key = "-", action = "dir_up" },
--     { key = "s", action = "system_open" },
--     { key = "q", action = "close" },
--     { key = "g?", action = "toggle_help" },
--     { key = "W", action = "collapse_all" },
--     { key = "S", action = "search_node" },
--     { key = ".", action = "run_file_command" },
--     { key = "<C-k>", action = "toggle_file_info" },
--     { key = "U", action = "toggle_custom" },
--   },
--   custom_keypress_funcs = {},
-- }

-- with relative path
-- require"nvim-tree.events".on_file_created(function(file) vim.cmd("edit "..file.fname) end)
-- -- with absolute path
-- require"nvim-tree.events".on_file_created(function(file) vim.cmd("edit "..vim.fn.fnamemodify(file.fname, ":p")) end)
