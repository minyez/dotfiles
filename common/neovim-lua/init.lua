-- User directory
USER_DIR = ""

require(USER_DIR .. '.utils')
require(USER_DIR .. '.plugins')
require(USER_DIR .. '.keymaps')
require(USER_DIR .. '.options')
require(USER_DIR .. '.autocmd')

require(USER_DIR .. '.colorscheme')("kanagawa")

require(USER_DIR .. '.lsp')

require(USER_DIR .. '.editor.whichkey')
-- require(USER_DIR .. '.editor.nvim-comment')
require(USER_DIR .. '.editor.Comment')
require(USER_DIR .. '.editor.nvim-autopairs')
require(USER_DIR .. '.editor.cmp')
-- require(USER_DIR .. '.editor.formatter')
require(USER_DIR .. '.editor.nvim-lastplace')
require(USER_DIR .. '.editor.editorconfig')

require(USER_DIR .. '.ui.notify')
-- require(USER_DIR .. '.ui.nvim-lightbulb') -- bulb not shown on Y9KP
require(USER_DIR .. '.ui.aerial')
require(USER_DIR .. '.ui.indent-blankline')
-- require(USER_DIR .. '.ui.nvim-ufo')
require(USER_DIR .. '.ui.filetype')
require(USER_DIR .. '.ui.gitsigns')
require(USER_DIR .. '.ui.nvim-tree')
require(USER_DIR .. '.ui.nvim-treesitter')
require(USER_DIR .. '.ui.orgmode') -- seems conflict default TODO/FIXME colorizing
require(USER_DIR .. '.ui.nvim-telescope')
require(USER_DIR .. '.ui.colorizer')
require(USER_DIR .. '.ui.lualine')
require(USER_DIR .. '.ui.neogit')
require(USER_DIR .. '.ui.todo-comments')
require(USER_DIR .. '.ui.trouble')
-- require(USER_DIR .. '.ui.neoscroll')

-- require(USER_DIR .. '.tool.cmake-tools')
