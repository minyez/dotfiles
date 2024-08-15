local pkg = "cmake-tools"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

local osys = require("cmake-tools.osys")

configs.setup({
  cmake_build_directory = function()
    if osys.iswin32 then
      return "build\\${variant:buildType}"
    end
    return "build/${variant:buildType}"
  end, -- this is used to specify generate directory for cmake, allows macro expansion, can be a string or a function returning the string, relative to cwd.
})
