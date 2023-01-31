local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Use a protected call so we don't error out on first use
local status_ok, lazy = pcall(require, "lazy")
if not status_ok then
  return
end

local plugins = {
  "nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
  "nvim-lua/plenary.nvim", -- Useful lua functions used in lots of plugins
  -- ====== start UI
  "rcarriga/nvim-notify", -- notify
  "folke/trouble.nvim",
  "nvim-lualine/lualine.nvim", -- status line
  "folke/which-key.nvim",
  "folke/todo-comments.nvim",
  "numToStr/Comment.nvim",
  "lewis6992/gitsigns.nvim",
  -- nvim-tree: directory browser
  {
    "kyazdani42/nvim-tree.lua",
    dependencies = {
      "kyazdani42/nvim-web-devicons", -- optional, for file icon
    }
  },
  -- ====== end UI
  -- ====== start colorschemes
  "rebelot/kanagawa.nvim",
  -- ====== end colorschemes

  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      "nvim-telescope/telescope-ui-select.nvim",
      "nvim-telescope/telescope-live-grep-raw.nvim",
      { "tom-anders/telescope-vim-bookmarks.nvim", dependencies = "MattesGroeger/vim-bookmarks" }
    }
  },

  -- "nvim-orgmode/orgmode", -- for Emacs orgmode file, disable as it will BREAK ENTER key
  "norcalli/nvim-colorizer.lua", -- show color
  "folke/trouble.nvim",
  "lukas-reineke/indent-blankline.nvim", -- indent line
  "gpanders/editorconfig.nvim", -- respect editorconfig
  "ethanholz/nvim-lastplace", -- auto return back to the last modified positon when open a file
  "nathom/filetype.nvim",

  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects", -- enhance texetobject selection
      "romgrk/nvim-treesitter-context", -- show class/function at top
      "JoosepAlviste/nvim-ts-context-commentstring", -- help with comment string
    },
    build = ":TSUpdate",
  },

  {
    'stevearc/aerial.nvim', -- outline view
    dependencies = "nvim-treesitter/nvim-treesitter"
  },

  -- ====== start LSP
  "neovim/nvim-lspconfig", -- enable LSP
  -- use "williamboman/nvim-lsp-installer" -- simple to use language server installer
  "williamboman/mason.nvim",
  "williamboman/mason-lspconfig.nvim",
  "ray-x/lsp_signature.nvim", -- show function signature when typing
  -- ====== end LSP
  -- ====== start cmp
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      -- {"quangnguyen30192/cmp-nvim-ultisnips"}, -- ultisnips
      { "hrsh7th/cmp-buffer" }, -- buffer completions
      { "hrsh7th/cmp-path" }, -- path completions
      { "hrsh7th/cmp-cmdline" }, -- cmdline completions
      { "hrsh7th/cmp-nvim-lua" },
      { "hrsh7th/cmp-nvim-lsp" }, -- lsp completions
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      { "saadparwaiz1/cmp_luasnip" }, -- snippet completions
      { "bydlw98/cmp-env" }, -- environment variables
      { "paopaol/cmp-doxygen" }, -- doxygen. require treesitter
      { "amarakon/nvim-cmp-lua-latex-symbols" }, -- latex symbols
      { "lukas-reineke/cmp-rg" }, -- ripgrep
    }
  },
  -- ====== end cmp
  -- ====== start snippets
  "L3MON4D3/LuaSnip", -- new snippets engine
  "rafamadriz/friendly-snippets",
  -- ====== end snippets

  "andymass/vim-matchup",
  "simrat39/symbols-outline.nvim", -- symbols overview
  'karb94/neoscroll.nvim', -- smooth scroll

  { 'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu', }
}

local opts = {}

lazy.setup(plugins, opts)
