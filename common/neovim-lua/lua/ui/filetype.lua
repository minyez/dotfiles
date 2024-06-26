-- https://github.com/nathom/filetype.nvim
-- Deprecated due to unmaintained, use built-in filetype
-- local pkg = "filetype"
-- local status_ok, configs = pcall(require, pkg)
-- if not status_ok then
--   vim.notify(pkg .. " not found!")
--   return
-- end
--
-- configs.setup({
--   overrides = {
--     extensions = {
--         -- Set the filetype of *.cpp.in files to cpp (fail to overwrite the default, which is make)
--         ["cpp.in"] = "cpp",
--         ["pwtk"] = "tcl",
--     },
--     literal = {
--       [".gitignore"] = "config",
--       [".vibesrc"] = "toml",
--       [".envrc"] = "bash",
--       [".clangd"] = "yaml",
--       ["INCAR"] = "bash",
--       -- Set the filetype of files named "make.inc" to make (Makefile filetype)
--       ["make.inc"] = "make",
--       ["makefile.include"] = "make",
--       ["Doxyfile.in"] = "make", -- doxygen template
--       ["cmake.arch.inc"] = "cmake",
--       ["cmake.inc"] = "cmake",
--     },
--     complex = {
--         -- Set the filetype of any full filename matching the regex to gitconfig
--         -- [".*git/config"] = "gitconfig", -- Included in the plugin
--       ["control.*.in"] = "bash",
--       ["INCAR*"] = "bash",
--     },
--   }
-- })

-- https://neovim.discourse.group/t/how-to-add-custom-filetype-detection-to-various-env-files/4272/2
-- https://github.com/davidosomething/dotfiles/blob/be22db1fc97d49516f52cef5c2306528e0bf6028/nvim/lua/dko/filetypes.lua
-- Unfortunately does not seem to work
-- vim.filetype.add({
--   -- Detect and assign filetype based on the extension of the filename
--   extension = {
--     ["cpp.in"] = "cpp",
--     ["pwtk"] = "tcl",
--   },
--   -- Detect and apply filetypes based on the entire filename
--   filename = {
--     [".gitignore"] = "config",
--     [".vibesrc"] = "toml",
--     [".envrc"] = "bash",
--     [".clangd"] = "yaml",
--     ["INCAR"] = "bash",
--     -- Set the filetype of files named "make.inc" to make (Makefile filetype)
--     ["make.inc"] = "make",
--     ["makefile.include"] = "make",
--     ["Doxyfile.in"] = "make", -- doxygen template
--     ["cmake.arch.inc"] = "cmake",
--     ["cmake.inc"] = "cmake",
--   },
--   -- Detect and apply filetypes based on certain patterns of the filenames
--   pattern = {
--     -- INFO: Match filenames like - ".env.example", ".env.local" and so on
--     -- ["%.env%.[%w_.-]+"] = "dotenv",
--     ["control%.*%.in"] = "bash",
--     ["INCAR*"] = "bash",
--   },
-- })


-- autocmd works
vim.api.nvim_create_autocmd({ 'BufEnter', 'BufNewFile' }, {
  pattern = '.vibesrc',
  command = 'set filetype=toml',
})

vim.api.nvim_create_autocmd({ 'BufEnter', 'BufNewFile' }, {
  pattern = '.envrc',
  command = 'set filetype=bash',
})
