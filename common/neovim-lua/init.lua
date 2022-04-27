local dir = ""

require(dir .. 'plugins')
require(dir .. 'impatient').enable_profile()
require(dir .. 'keymaps')
require(dir .. 'options')
require(dir .. 'colorscheme')

require(dir .. 'lsp')

require(dir .. 'editor.whichkey')
require(dir .. 'editor.nvim-comment')
require(dir .. 'editor.nvim-autopairs')
require(dir .. 'editor.cmp')
require(dir .. 'editor.nvim-lastplace')

require(dir .. 'ui.notify')
require(dir .. 'ui.nvim-tree')
require(dir .. 'ui.nvim-treesitter')
require(dir .. 'ui.orgmode')
require(dir .. 'ui.nvim-telescope')
require(dir .. 'ui.colorizer')
require(dir .. 'ui.lualine')
require(dir .. 'ui.trouble')
