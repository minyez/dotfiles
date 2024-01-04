local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- in vanilla vim CTRL-Q same as CTRL-V. remap to quit
keymap("", "<C-q>", "<cmd>q<cr>", opts)
-- save file
keymap("", "<C-s>", "<cmd>w<cr>", opts)

-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Move text up and down
keymap("n", "<A-j>", ":m .+1<CR>", opts)
keymap("n", "<A-k>", ":m .-2<CR>", opts)
keymap("n", "<A-l>", ">>", opts)
keymap("n", "<A-h>", "<<", opts)

-- Insert --
-- Press jk fast to enter ESC
keymap("i", "jk", "<ESC>", opts)
-- keymap("i", "<C-g>", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "<A-l>", ">gv", opts)
keymap("v", "<A-h>", "<gv", opts)
keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
-- keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
-- keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

vim.cmd[[
  xmap ga <Plug>(EasyAlign)
  nmap ga <Plug>(EasyAlign)
]]


-- Remap direction keys to window manipulation
-- keymap("n", "<UP>", "<C-w>R", opts) -- rotate upwards/leftwards
-- keymap("n", "<DOWN>", "<C-w>r", opts) -- rotate downwards/rightwards
-- keymap("n", "<RIGHT>", "<C-w>x", opts) -- switch with next
-- keymap("n", "<LEFT>", "<C-w>x", opts) -- switch with next
-- keymap("n", "<LEFT>", ":echo winnr()-1<CR>", opts) -- switch with previous
-- keymap("n", "<LEFT>", ":exe 1 \"wincmd x\"<CR>", opts) -- switch with previous

-- keymap('v', '<leader>sg', 'y<ESC>:Telescope live_grep default_text=<c-r>0<CR><ESC>', opts)
-- keymap('v', '<leader>sg', '"+y:Telescope live_grep default_text=<c-r>+<CR><ESC>', opts)

-- Terminal --
-- Better terminal navigation
-- keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
-- keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
-- keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
-- keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Use Ctrl-/ to Comment, supported by Comment.nvim
-- https://vi.stackexchange.com/questions/26611/is-it-possible-to-map-control-forward-slash-with-vim
keymap("n", "<C-_>", "<Plug>(comment_toggle_linewise_current)", opts)
keymap("v", "<C-_>", "<Plug>(comment_toggle_linewise_visual)gv-gv", opts)

