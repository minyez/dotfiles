local pkg = "nvim-autopairs"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

local check_ts, _ = pcall(require, "nvim-treesitter.configs")

-- add custom rules if you need
-- see https://github.com/windwp/nvim-autopairs#rule
-- local Rule = require(pkg .. ".rule")

configs.setup({
  -- break_line_filetype = nil,
  disable_filetype = { "TelescopePrompt", },
  -- ignored_next_char = "[%w%.]", -- will ignore alphanumeric and `.` symbol
  check_ts = check_ts,
  -- <A-e> to use fast_wrap
  fast_wrap = {
    check_comma = false,
    highlight = 'Search',
    highlight_grey='Comment',
  },
})
