local pkg = "which-key"
local status_ok, wk = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

wk.add({
  mode = "n",

  -- Symbols
  -- { "<leader>]", "<cmd>AerialNext<cr>", desc = "(Aerial) Next Symbol" },
  -- { "<leader>[", "<cmd>AerialPrev<cr>", desc = "(Aerial) Prev Symbol" },

  { "<leader>l", "<cmd>nohlsearch<cr>", desc = "Stop the highlighting" },
  -- File search
  -- { "<leader><leader>", "<cmd>Telescope frecency workspace=CWD<cr>", desc = "Find File" },
  { "<leader>f", group = "+file/find" },
  { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File" },
  { "<leader>fe", "<cmd>Telescope find_files hidden=true<cr>", desc = "Find File (including hidden)" },
  -- { "<leader>fp", "<cmd>Telescope git_files<cr>", desc = "Find File (Git project)" },
  -- { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Open Recent File" },
  -- { "<leader>fN", "<cmd>enew<cr>", desc = "New File" },
  -- { "<leader>fn", "<cmd>NvimTreeFocus<cr>a", desc = "New File (From NvimTree)" },
  { "<leader>fs", "<cmd>w<cr>", desc = "Save File" },
  { "<leader>fa", "<cmd>wa<cr>", desc = "Save All Files" },

  -- Toggle
  { "<leader>t", group = "+toggle" },
  { "<leader>tv", "<cmd>ToggleAlternate<cr>", desc = "Alternate Boolean Value" },
  -- { "<leader>ta", "<cmd>AerialToggle!<cr>", desc = "Aerial" },
  -- { "<leader>tt", "<cmd>NvimTreeToggle<cr>", desc = "NvimTree" },
  { "<leader>tl", "<cmd>set number! relativenumber!<cr>", desc = "Line Numbers" },
  { "<leader>tL", "<cmd>set number!<cr>", desc = "Absolute Line Number" },
  { "<leader>tR", "<cmd>set relativenumber!<cr>", desc = "Relative Line Number" },
  { "<leader>ti", "<cmd>IndentBlanklineToggle<cr>", desc = "indent-blankline" },
  { "<leader>ts", "<cmd>SymbolsOutline<cr>", desc = "Symbols Outline" },
  { "<leader>tg", "<cmd>Gitsigns toggle_linehl<cr><cmd>Gitsigns toggle_deleted<cr>", desc = "Toggle git diff inline" },
  -- -- Trouble was updated but seems not work for now. Open diagnostics will give many "attempt to index field 'icons'" error
  -- -- So comment out until I have time to fix it.
  -- { "<leader>tb", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
  -- { "<leader>td", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer Diagnostics (Trouble)" },
  -- { "<leader>tr", "<cmd>Trouble lsp toggle focus=false win.position=right<cr>", desc = "LSP Def./Ref./... (Trouble)" },
  -- -- { "<leader>tb", "<cmd>TroubleToggle<cr>", desc = "Trouble" },
  -- -- { "<leader>tw", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Trouble Workspace" },
  -- -- { "<leader>td", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Trouble Document" },
  -- -- { "<leader>tr", "<cmd>TroubleToggle lsp_references<cr>", desc = "Trouble References" },

  -- LSP
  -- { "<leader>L",  group = "+lsp" },
  -- { "<leader>Lt", "<cmd>lua toggle_diagnostics()<cr>", desc = "Toggle diagnostics" },

  -- Paste
  { "<leader>p", group = "+paste" },
  { "<leader>pp", "a<c-r>+<esc> ", desc = "Paste from system clipboard" },
  { "<leader>pi", "<cmd>PasteImage<cr>", desc = "Paste clipboard image" },
  { "<leader>p1", "a<c-r>*<esc> ", desc = "Paste from mouse clip" },

  -- Definition
  { "<leader>d", group = "+definition" },
  { "<leader>dd", "<cmd>Telescope lsp_definitions<cr>", desc = "LSP Definitions" },
  { "<leader>ds", "<c-w><c-]>", desc = "Open Definitions in vertical split" },
  { "<leader>dv", "<c-w>v<c-]>", desc = "Open Definitions in horizontal split" },

  -- Definition
  { "<leader>e", group = "+edit" },
  { "<leader>es", "<cmd>%s/\\s\\+$//g<cr>", desc = "Remove trailing spaces" },
  { "<leader>eS", "<cmd>sort<cr>", desc = "Sort" },

  -- Make
  { "<leader>m", group = "+make" },
  { "<leader>mm", "<cmd>make<cr>", desc = "Make" },
  -- { "<leader>mc", group = "+CMake" },
  -- { "<leader>mcc", "<cmd>:CMake configure<cr>", desc = "Configure" },
  -- { "<leader>mcs", "<cmd>:CMake select_target<cr>", desc = "Select Target" },
  -- { "<leader>mca", "<cmd>:CMake build_all<cr>", desc = "Build all" },

  -- -- Refactor
  -- { "<leader>r", group = "+refactor" },
  -- { "<leader>rp", "<cmd>lua vim.lsp.buf.format { async = true }<cr>", desc = "Prettify by LSP Format" },

  -- help
  { "<leader>h", group = "+help" },

  -- -- quit
  -- { "<leader>q", group = "+quit" },
  -- { "<leader>qq", "<cmd>wqa<cr>", desc = "Save All and Quit" },
  -- { "<leader>qs", "<cmd>wq<cr>", desc = "Quit After Save" },
  -- { "<leader>qf", "<cmd>q!<cr>", desc = "Force Quit" },
  -- { "<leader>qa", "<cmd>qa!<cr>", desc = "Force Quit All" },

  -- window
  { "<leader>w", group = "+window" },
  { "<leader>wv", "<cmd>vsplit<cr>", desc = "Vertical split" },
  { "<leader>w-", "<cmd>vsplit<cr>", desc = "Vertical split" },
  { "<leader>w\\", "<cmd>split<cr>", desc = "Horizontal split" },
  { "<leader>ws", "<cmd>split<cr>", desc = "(Horizontal) split" },
  { "<leader>w.", "<cmd>:vertical resize +5<cr>", desc = "increase width by 5" },
  { "<leader>w,", "<cmd>:vertical resize -5<cr>", desc = "decrease width by 5" },

  -- tab operations
  { "<leader>b", group = "+tab" },
  { "<leader>bx", "<cmd>tabclose<cr>", desc = "tabclose" },
  { "<leader>bn", "<cmd>tabnew<cr>", desc = "tabnew" },

  -- git
  { "<leader>g", group = "+git" },
  { "<leader>gg", "<cmd>Neogit<cr>", desc = "Neogit" },
  { "<leader>gu", "<cmd>Gitsigns undo_stage_hunk<cr>", desc = "Undo Stage Hunk" },
  { "<leader>gR", "<cmd>Gitsigns reset_buffer<cr>", desc = "Reset buffer" },
  { "<leader>gr", "<cmd>Gitsigns reset_hunk<cr>", desc = "Reset hunk" },
  { "<leader>gS", "<cmd>Gitsigns stage_hunk<cr>", desc = "Stage Hunk" },
  { "<leader>ga", "<cmd>Gitsigns stage_buffer<cr>", desc = "Stage Buffer" },
  -- { "<leader>gv", "<cmd>DiffviewOpen<cr>", desc = "DiffviewOpen" },
  { "<leader>g[", "<cmd>Gitsigns prev_hunk<cr>", desc = "Previous Hunk" },
  { "<leader>g]", "<cmd>Gitsigns next_hunk<cr>", desc = "Next Hunk" },
  -- { "<leader>gd", group = "+diff" },
  -- { "<leader>gdd", "<cmd>Gitsigns diffthis<cr>", desc = "Diff This" },
  -- { "<leader>gdw", "<cmd>Gitsigns toggle_word_diff<cr>", desc = "Toggle word diff" },

  -- Searching
  { "<leader>s", group = "+search" },
  { "<leader>sa", "<cmd>lua require('actions-preview').code_actions()<cr>", desc = "Code Actions" },
  -- { "<leader>sc", "<cmd>Telescope commands<cr>", desc = "Search Commands" },
  -- { "<leader>sS", "<cmd>Telescope grep_string<cr>", desc = "Search Cursor String" },
  { "<leader>sb", "<cmd>Telescope vim_bookmarks<cr>", desc = "Search Bookmarks" },
  { "<leader>sr", "<cmd>Telescope lsp_references<cr>", desc = "Search LSP References" },
  -- { "<leader>sg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep" },
  -- { "<leader>ss", "<cmd>Telescope lsp_workspace_symbols<cr>", desc = "LSP Workspace Symbols" },
  -- { "<leader>st", "<cmd>TodoTelescope<cr>", desc = "Search TODOs" },
  -- { "<leader>sw", 'bve"zy:Telescope live_grep default_text=<c-r>z<cr>', desc = "Live Grep Cursor Word" },
  -- { "<leader>/", "<cmd>CommentToggle<cr>", desc = "Comment" }, -- nvim-comment, old
  -- { "<leader>/", "<Plug>(comment_toggle_linewise_current)", desc = "Comment" }, -- Comment-nvim, new

  -- Open at cursor
  { "<leader>o", group = "+open" },
  -- { "<leader>od", 'yiW:!open https://doi.org/<C-r>"<cr>', desc = "DOI"},
})

-- visual mapping of leader
wk.add({
  mode = "v",
  { "<leader>s", group = "+search" },
  { "<leader>sg", "\"zy:exec 'Telescope grep_string default_text=' .escape(@z, ' ')<cr>", desc = "Grep Selection" },
  { "<leader>y", group = "+yank" },
  { "<leader>y1", '"*y', desc = "Yank to mouse clip" },
  -- ["/"] = { "<cmd>'<,'>CommentToggle<cr>", "Comment" }, -- nvim-comment, old
  -- { "<leader>/", "<Plug>(comment_toggle_linewise_visual)", desc = "Comment" }, -- Comment-nvim, new
})

-- wk.add({
--   { "g", group = "+goto" },
--   { "gd", "<cmd>Telescope lsp_definitions<cr>", desc = "LSP Definitions (Telescope)" },
--   { "gt", "<cmd>Telescope lsp_type_definitions<cr>", desc = "LSP Type Definitions (Telescope)" },
-- })
