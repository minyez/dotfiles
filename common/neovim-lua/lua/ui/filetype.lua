local pkg = "filetype"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup({
  overrides = {
    extensions = {
        -- Set the filetype of *.pn files to potion
        -- pn = "potion",
    },
    literal = {
      -- Set the filetype of files named "MyBackupFile" to lua
      -- MyBackupFile = "lua",
    },
  }
})
