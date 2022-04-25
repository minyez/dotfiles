local status_ok, kanagawa = pcall(require, "kanagawa")
if not status_ok then
  vim.notify("kanagawa theme not found!")
  return
end

local colors = {
-- switch the default and dark background
  sumiInk0 = "#1F1F28",
  sumiInk1 = "#16161D",
}

local overrides = {
    -- create a new hl-group using default palette colors and/or new ones
--    MyHlGroup1 = { fg = default_colors.waveRed, bg = "#AAAAAA", style="underline,bold", guisp="blue" },

    -- override existing hl-groups, the new keywords are merged with existing ones
--    VertSplit  = { fg = default_colors.bg_dark, bg = "NONE" },
--    TSError    = { link = "Error" },
--    TSKeywordOperator = { style = 'bold'},
--    StatusLine = { fg = my_colors.new_color }
}

kanagawa.setup({
  undercurl = true,           -- enable undercurls
  commentStyle = "italic",
  functionStyle = "NONE",
  keywordStyle = "italic",
  statementStyle = "bold",
  typeStyle = "NONE",
  variablebuiltinStyle = "italic",
  specialReturn = true,       -- special highlight for the return keyword
  specialException = true,    -- special highlight for exception handling keywords
  transparent = false,        -- do not set background color
  dimInactive = false,        -- dim inactive window `:h hl-NormalNC`
  globalStatus = false,       -- adjust window separators highlight for laststatus=3
  colors = colors,
  overrides = overrides,
})

-- setup must be called before loading
vim.cmd("colorscheme kanagawa")

