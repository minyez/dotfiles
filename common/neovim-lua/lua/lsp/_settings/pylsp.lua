-- pylsp configuration:
-- https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
local M = {
  settings = {
    pylsp = {
      plugins = {
        pycodestyle = {
          maxLineLength = 120,
          ignore = { "E741", "W292", "W391", "W504", "E241", "E201", "E221"},
        },
        pyflakes = {
          enabled = false
        },
        autopep8 = {
          enabled = false
        }
      }
    }
  }
}

return M
