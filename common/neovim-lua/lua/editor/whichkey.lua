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
  f = {
    name = "+file",
    f = { "<cmd>Telescope find_files<cr>", "Find File" },
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
    n = { "<cmd>enew<cr>", "New File" },
    s = { "<cmd>w<cr>", "Save File" },
    q = { "<cmd>wq<cr>", "Save And Quit" },
    a = { "<cmd>wa<cr>", "Save All Files" },
  },
  q = {
    name = "+quit",
    q = { "<cmd>q<cr>", "Quit" },
    a = { "<cmd>qa<cr>", "Quit All" },
  },
-- toggle things
  t = {
    name = "+toggle",
    t = { "<cmd>NvimTreeToggle<cr>", "NvimTree" },
    f = { "<cmd>Trouble quickfix<cr>", "Trouble Quickfix" },
  },
-- search
  s = {
    name = "+search",
    c = { "<cmd>Telescope commands<cr>", "Search Commands" },
    r = { "<cmd>Telescope lsp_references<cr>", "Search LSP References" },
  },
}, { prefix = "<leader>"})


configs.register({
  -- in vanilla vim CTRL-Q same as CTRL-V. remap to quit
  ["<C-q>"] = { "<cmd>q<cr>", "Quit" },
})
