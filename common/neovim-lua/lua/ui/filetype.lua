-- https://github.com/nathom/filetype.nvim
local pkg = "filetype"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({
  overrides = {
    extensions = {
        -- Set the filetype of *.cpp.in files to cpp (fail to overwrite the default, which is make)
        ["cpp.in"] = "cpp",
    },
    literal = {
      -- Set the filetype of files named "MyBackupFile" to lua
      -- MyBackupFile = "lua",
    },
    complex = {
        -- Set the filetype of any full filename matching the regex to gitconfig
        -- [".*git/config"] = "gitconfig", -- Included in the plugin
    },
  }
})
