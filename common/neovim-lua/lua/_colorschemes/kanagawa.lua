local status_ok, kanagawa = pcall(require, "kanagawa")
if not status_ok then
  vim.notify("kanagawa theme not found!")
  return
end

-- local overrides = function(colors)
--   return {}
-- end

local dark = "wave"
local light = "lotus"

kanagawa.setup({
  compile = true,             -- enable compiling the colorscheme
  undercurl = true,            -- enable undercurls
  commentStyle = { italic = true },
  functionStyle = {},
  keywordStyle = { italic = true},
  statementStyle = { bold = true },
  typeStyle = {},
  transparent = false,         -- do not set background color
  dimInactive = false,         -- dim inactive window `:h hl-NormalNC`
  terminalColors = true,       -- define vim.g.terminal_color_{0,17}
  colors = {                   -- add/modify theme and palette colors
      palette = {
        -- switch the default and dark background
        sumiInk0 = "#1F1F28",
        sumiInk1 = "#16161D",
        fujiWhite = "#FFFFFF"
      },
      theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
  },
  -- overrides = overrides,
  background = {           -- map the value of 'background' option to a theme
      dark = dark,         -- try "dragon" !
      light = light
  },
  variablebuiltin = { italic = true },
  specialReturn = true,       -- special highlight for the return keyword
  specialException = true,    -- special highlight for exception handling keywords
  globalStatus = false,       -- adjust window separators highlight for laststatus=3
})

-- setup must be called before loading
vim.cmd("colorscheme kanagawa")

