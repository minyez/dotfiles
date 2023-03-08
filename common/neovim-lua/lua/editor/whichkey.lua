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
  ["<leader>"] = { "<cmd>Telescope find_files<cr>", "Find File" },
  [']'] = { "<cmd>AerialNext<cr>", "(Aerial) Next Symbol" },
  ['['] = { "<cmd>AerialPrev<cr>", "(Aerial) Prev Symbol" },
  l = { "<cmd>nohlsearch<cr>", "Stop the highlighting"},
  p = { "a<c-r>+<esc> ", "Paste from system clipboard"},
  P = { "a<c-r>*<esc> ", "Paste from mouse clip"},
  f = {
    name = "+file",
    f = { "<cmd>Telescope find_files<cr>", "Find File" },
    p = { "<cmd>lua vim.lsp.buf.format { async = true }<cr>", "LSP Format" },
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
    N = { "<cmd>enew<cr>", "New File" },
    -- n = { "<cmd>NvimTreeFocus<cr>a", "New File (From NvimTree)" },
    s = { "<cmd>w<cr>", "Save File" },
    q = { "<cmd>wq<cr>", "Save And Quit" },
    a = { "<cmd>wa<cr>", "Save All Files" },
  },
  h = {
    name = "+help",
    b = { "<cmd>Gitsigns blame_line<cr>", "Blame line" },
  },
  q = {
    name = "+quit",
    q = { "<cmd>qa<cr>", "Quit All" },
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
    b = { "<cmd>Gitsigns blame_line<cr>", "Blame line" },
    d = { "<cmd>Gitsigns diffthis<cr>", "Diff This" },
    u = { "<cmd>Gitsigns undo_stage_hunk<cr>", "Undo Stage Hunk" },
    a = { "<cmd>Gitsigns stage_hunk<cr>", "Stage Hunk" },
    S = { "<cmd>Gitsigns stage_buffer<cr>", "Stage Buffer" },
    ["["] = { "<cmd>Gitsigns prev_hunk<cr>", "Previous Hunk" },
    ["]"] = { "<cmd>Gitsigns next_hunk<cr>", "Next Hunk" },
  },
  w = {
    name = "+window",
    v = { "<cmd>vsplit<cr>", "Vertical split" },
    ["-"] = { "<cmd>vsplit<cr>", "Vertical split" },
    ["="] = { "<cmd>vsplit<cr>", "Vertical split" },
    s = { "<cmd>split<cr>", "(Horizontal) split" },
  },
  -- toggle things
  t = {
    name = "+toggle",
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
  -- ["/"] = { "<cmd>'<,'>CommentToggle<cr>", "Comment" }, -- nvim-comment, old
  ["/"] = { "<Plug>(comment_toggle_linewise_visual)", "Comment" }, -- Comment-nvim, new
}, { mode = "v", prefix = "<leader>" })

configs.register({
  g = {
    name = "+goto",
    d = { "<cmd>Telescope lsp_definitions<cr>", "LSP Definitions" },
  },
})
