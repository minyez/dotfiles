-- https://github.com/folke/which-key.nvim
local pkg = "which-key"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

-- register keymapping
-- configs.register(mappings, opts)

configs.register({
-- files
  ["<leader>"] = { "<cmd>Telescope find_files<cr>", "Find File" },
  f = {
    name = "+file",
    f = { "<cmd>Telescope find_files<cr>", "Find File" },
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
    N = { "<cmd>enew<cr>", "New File" },
    -- n = { "<cmd>NvimTreeFocus<cr>a", "New File (From NvimTree)" },
    s = { "<cmd>w<cr>", "Save File" },
    q = { "<cmd>wq<cr>", "Save And Quit" },
    a = { "<cmd>wa<cr>", "Save All Files" },
  },
  q = {
    name = "+quit",
    q = { "<cmd>q<cr>", "Quit" },
    f = { "<cmd>q!<cr>", "Force Quit" },
    a = { "<cmd>qa<cr>", "Quit All" },
  },
  n = {
    name = "+tree",
  },
  g = {
    name = "+git",
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
    s = { "<cmd>split<cr>", "(Horizontal) split" },
  },
-- toggle things
  t = {
    name = "+toggle",
    t = { "<cmd>NvimTreeToggle<cr>", "NvimTree" },
    n = { "<cmd>set number! relativenumber!<cr>", "Line Numbers" },
    L = { "<cmd>set number!<cr>", "Absolute Line Number" },
    R = { "<cmd>set relativenumber!<cr>", "Relative Line Number" },
    T = { "<cmd>TroubleToggle<cr>", "Trouble" },
    w = { "<cmd>TroubleToggle workspace_diagnostics<cr>", "Trouble Workspace" },
    d = { "<cmd>TroubleToggle document_diagnostics<cr>", "Trouble Document" },
    r = { "<cmd>TroubleToggle lsp_references<cr>", "Trouble References" },
  },
-- search
  s = {
    name = "+search",
    c = { "<cmd>Telescope commands<cr>", "Search Commands" },
    r = { "<cmd>Telescope lsp_references<cr>", "Search LSP References" },
    g = { "<cmd>Telescope live_grep<cr>", "Live Grep" },
    s = { "<cmd>Telescope lsp_workspace_symbols<cr>", "LSP Workspace Symbols" },
    o = { "<cmd>SymbolsOutline<cr>", "Symbols Outline" },
  },
}, { prefix = "<leader>"})


configs.register({
  g = {
    name = "+goto",
    d = { "<cmd>Telescope lsp_definitions<cr>", "LSP Definitions" },
  },
})
