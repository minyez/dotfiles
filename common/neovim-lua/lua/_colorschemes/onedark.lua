local status_ok, onedark = pcall(require, "onedark")
if not status_ok then
  vim.notify("onedark theme not found!")
  return
end

local style = 'darker'

-- NOTE: if use 'light' theme, you should change backgournd and style to 'light'
if style == 'light' then
  vim.o.background = 'light'
else
  vim.o.background = 'dark'
end

onedark.setup {
  -- Main options --
  style = style, -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
  transparent = false, -- Show/hide background
  term_colors = true, -- Change terminal color as per the selected theme style
  ending_tildes = false, -- Show the end-of-buffer tildes. By default they are hidden
  -- toggle theme style ---
  toggle_style_list = { 'light', 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' }, -- List of styles to toggle between
  toggle_style_key = '<leader>ts', -- Default keybinding to toggle

  -- Change code style ---
  -- Options are italic, bold, underline, none
  -- You can configure multiple style with comma seperated, For e.g., keywords = 'italic,bold'
  code_style = {
    comments = 'italic',
    keywords = 'none',
    functions = 'bold',
    strings = 'none',
    variables = 'none'
  },

  -- Custom Highlights --
  colors = {}, -- Override default colors
  highlights = {}, -- Override highlight groups
  -- Plugins Config --
  diagnostics = {
    darker = true, -- darker colors for diagnostic
    undercurl = true, -- use undercurl instead of underline for diagnostics
    background = true, -- use background color for virtual text
  },
}

onedark.load()
