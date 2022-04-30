local dir = ""

require(dir .. 'utils')
require(dir .. 'plugins')
require('impatient').enable_profile()
require(dir .. 'keymaps')
require(dir .. 'options')

-- NOTE: need to figure out a light lualine, otherwise stick to the dark theme
-- if IS_DAYTIME() then
--   require(dir .. 'colorscheme')("github-theme")
-- else
--   require(dir .. 'colorscheme')("kanagawa")
-- end
require(dir .. 'colorscheme')("kanagawa")

require(dir .. 'lsp')

require(dir .. 'editor.whichkey')
require(dir .. 'editor.nvim-comment')
require(dir .. 'editor.nvim-autopairs')
require(dir .. 'editor.cmp')
require(dir .. 'editor.nvim-lastplace')
--
require(dir .. 'ui.notify')
require(dir .. 'ui.gitsigns')
require(dir .. 'ui.nvim-tree')
require(dir .. 'ui.nvim-treesitter')
require(dir .. 'ui.orgmode') -- seems conflict default TODO/FIXME colorizing on macOS
require(dir .. 'ui.nvim-telescope')
require(dir .. 'ui.colorizer')
require(dir .. 'ui.lualine')
require(dir .. 'ui.todo-comments')
require(dir .. 'ui.trouble')
