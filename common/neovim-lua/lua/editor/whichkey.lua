-- https://github.com/folke/which-key.nvim
local pkg = "which-key"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

-- register keymapping
-- configs.register(mappings, opts)

-- normal mapping with leader
configs.register({
  -- files
  ["<leader>"] = { "<cmd>Telescope frecency workspace=CWD<cr>", "Find File" },
  [']'] = { "<cmd>AerialNext<cr>", "(Aerial) Next Symbol" },
  ['['] = { "<cmd>AerialPrev<cr>", "(Aerial) Prev Symbol" },
  l = { "<cmd>nohlsearch<cr>", "Stop the highlighting"},
  p = {
    name = "+paste",
    p     = { "a<c-r>+<esc> ", "Paste from system clipboard"},
    ["1"] = { "a<c-r>*<esc> ", "Paste from mouse clip"},
  },
  d = {
    name = "+definition",
    d = { "<cmd>Telescope lsp_definitions<cr>", "LSP Definitions" },
    s = { "<c-w><c-]>", "Open Definitions in vertical split" },
    v = { "<c-w>v<c-]>", "Open Definitions in horizontal split" },
  },
  e = {
    name = "+edit",
    ["s"] = { "<cmd>%s/\\s\\+$//g<cr>", "Remove trailing spaces" },
  },
  f = {
    name = "+file",
    f = { "<cmd>Telescope find_files<cr>", "Find File" },
    e = { "<cmd>Telescope find_files hidden=true<cr>", "Find File (including hidden)" },
    p = { "<cmd>Telescope git_files<cr>", "Find File (Git project)" },
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
    N = { "<cmd>enew<cr>", "New File" },
    -- n = { "<cmd>NvimTreeFocus<cr>a", "New File (From NvimTree)" },
    s = { "<cmd>w<cr>", "Save File" },
    a = { "<cmd>wa<cr>", "Save All Files" },
  },
  h = {
    name = "+help",
    b = { "<cmd>Gitsigns blame_line<cr>", "Blame line" },
  },
  q = {
    name = "+quit",
    q = { "<cmd>wqa<cr>", "Save All and Quit" },
    s = { "<cmd>wq<cr>", "Quit After Save" },
    f = { "<cmd>q!<cr>", "Force Quit" },
    a = { "<cmd>qa!<cr>", "Force Quit All" },
  },
  m = {
    name = "+make",
    m = { "<cmd>make<cr>", "Make" },
    c = {
      name = "+CMake",
      c = { "<cmd>:CMake configure<cr>", "Configure" },
      s = { "<cmd>:CMake select_target<cr>", "Select Target" },
      a = { "<cmd>:CMake build_all<cr>", "Build all" },
    },
  },
  -- n = {
  --   name = "+tree",
  -- },
  g = {
    name = "+git",
    d = {
      name = "+diff",
      d = { "<cmd>Gitsigns diffthis<cr>", "Diff This" },
      w = { "<cmd>Gitsigns toggle_word_diff<cr>", "Toggle word diff" },
    },
    u = { "<cmd>Gitsigns undo_stage_hunk<cr>", "Undo Stage Hunk" },
    R = { "<cmd>Gitsigns reset_buffer<cr>", "Reset buffer" },
    r = { "<cmd>Gitsigns reset_hunk<cr>", "Reset hunk" },
    s = { "<cmd>Gitsigns stage_hunk<cr>", "Stage Hunk" },
    a = { "<cmd>Gitsigns stage_buffer<cr>", "Stage Buffer" },
    v = { "<cmd>DiffviewOpen<cr>", "DiffviewOpen" },
    ["["] = { "<cmd>Gitsigns prev_hunk<cr>", "Previous Hunk" },
    ["]"] = { "<cmd>Gitsigns next_hunk<cr>", "Next Hunk" },
    g = { "<cmd>Neogit<cr>", "Neogit" },
  },
  w = {
    name = "+window",
    v = { "<cmd>vsplit<cr>", "Vertical split" },
    ["-"] = { "<cmd>vsplit<cr>", "Vertical split" },
    ["\\"] = { "<cmd>split<cr>", "Horizontal split" },
    s = { "<cmd>split<cr>", "(Horizontal) split" },
    ["."] = { "<cmd>:vertical resize +5<cr>", "increase width by 5" },
    [","] = { "<cmd>:vertical resize -5<cr>", "decrease width by 5" },
  },
  -- tab operations
  b = {
    name = "+tab",
    x = { "<cmd>tabclose<cr>", "tabclose" },
    n = { "<cmd>tabnew<cr>", "tabnew" },
  },
  -- Refactor
  r = {
    name = "+refactor",
    p = { "<cmd>lua vim.lsp.buf.format { async = true }<cr>", "Prettify by LSP Format" },
  },
  -- toggle things
  t = {
    name = "+toggle",
    v = { "<cmd>ToggleAlternate<cr>", "Alternate Boolean Value" },
    a = { "<cmd>AerialToggle!<cr>", "Aerial" },
    t = { "<cmd>NvimTreeToggle<cr>", "NvimTree" },
    l = { "<cmd>set number! relativenumber!<cr>", "Line Numbers" },
    L = { "<cmd>set number!<cr>", "Absolute Line Number" },
    R = { "<cmd>set relativenumber!<cr>", "Relative Line Number" },
    b = { "<cmd>TroubleToggle<cr>", "Trouble" },
    i = { "<cmd>IndentBlanklineToggle<cr>", "indent-blankline"},
    w = { "<cmd>TroubleToggle workspace_diagnostics<cr>", "Trouble Workspace" },
    d = { "<cmd>TroubleToggle document_diagnostics<cr>", "Trouble Document" },
    r = { "<cmd>TroubleToggle lsp_references<cr>", "Trouble References" },
    s = { "<cmd>SymbolsOutline<cr>", "Symbols Outline" },
    g = { "<cmd>Gitsigns toggle_linehl<cr><cmd>Gitsigns toggle_deleted<cr>", "Toggle git diff inline" },
  },
  -- search
  s = {
    name = "+search",
    -- a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Actions" },
    a = { "<cmd>CodeActionMenu<cr>", "Code Actions" },
    c = { "<cmd>Telescope commands<cr>", "Search Commands" },
    S = { "<cmd>Telescope grep_string<cr>", "Search Cursor String" },
    b = { "<cmd>Telescope vim_bookmarks<cr>", "Search Bookmarks" },
    r = { "<cmd>Telescope lsp_references<cr>", "Search LSP References" },
    g = { "<cmd>Telescope live_grep<cr>", "Live Grep" },
    s = { "<cmd>Telescope lsp_workspace_symbols<cr>", "LSP Workspace Symbols" },
    t = { "<cmd>TodoTelescope<cr>", "Search TODOs" },
    w = { 'bve"zy:Telescope live_grep default_text=<c-r>z<cr>', 'Live Grep Cursor Word' },
  },
  -- ["/"] = { "<cmd>CommentToggle<cr>", "Comment" }, -- nvim-comment, old
  ["/"] = { "<Plug>(comment_toggle_linewise_current)", "Comment" }, -- Comment-nvim, new
}, { mode = "n", prefix = "<leader>" })

-- visual mapping of leader
configs.register({
  s = {
    name = "+search",
    -- from https://www.reddit.com/r/neovim/comments/p8wtmn/telescopenvim_how_to_take_what_i_selected_in
    g = { '"zy:exec \'Telescope grep_string default_text=\' .escape(@z, \' \')<cr>', 'Grep Selection' },
    -- c = { "y<cmd>Telescope commands default_text=<cr>", "Search Commands" },
    -- r = { "<cmd>Telescope lsp_references<cr>", "Search LSP References" },
    -- s = { "<cmd>Telescope lsp_workspace_symbols<cr>", "LSP Workspace Symbols" },
    -- o = { "<cmd>SymbolsOutline<cr>", "Symbols Outline" },
  },
  y = {
    name = "+yank",
    ["1"] = { "\"*y", "Yank to mouse clip"},
  },
  -- ["/"] = { "<cmd>'<,'>CommentToggle<cr>", "Comment" }, -- nvim-comment, old
  ["/"] = { "<Plug>(comment_toggle_linewise_visual)", "Comment" }, -- Comment-nvim, new
}, { mode = "v", prefix = "<leader>" })

configs.register({
  g = {
    name = "+goto",
    d = { "<cmd>Telescope lsp_definitions<cr>", "LSP Definitions (Telescope)" },
    -- D = { "<cmd>Telescope lsp_implementations<cr>", "LSP Implementations (Telescope)" },
    t = { "<cmd>Telescope lsp_type_definitions<cr>", "LSP Type Definitions (Telescope)" },
  },
})
