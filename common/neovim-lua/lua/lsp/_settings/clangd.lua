local M = {}

M.settings = {
  cmd = {
    'clangd',
    "-j=8",
    "--malloc-trim",
    "--background-index",
    "--pch-storage=memory",
  }
}
