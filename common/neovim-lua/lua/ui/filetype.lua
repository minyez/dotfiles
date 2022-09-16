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
        ["pwtk"] = "tcl",
    },
    literal = {
      [".gitignore"] = "config",
      [".envrc"] = "bash",
      [".clangd"] = "yaml",
      ["INCAR"] = "bash",
      -- Set the filetype of files named "make.inc" to make (Makefile filetype)
      ["make.inc"] = "make",
      ["makefile.include"] = "make",
      ["Doxyfile.in"] = "make", -- doxygen template
      ["cmake.arch.inc"] = "cmake",
      ["cmake.inc"] = "cmake",
    },
    complex = {
        -- Set the filetype of any full filename matching the regex to gitconfig
        -- [".*git/config"] = "gitconfig", -- Included in the plugin
      ["control.*.in"] = "bash",
    },
  }
})
