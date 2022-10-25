-- original file from https://github.com/ravenxrz/dotfiles/blob/master/nvim/lua/user/plugins.lua
local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  print "Installing packer close and reopen Neovim..."
  vim.cmd [[packadd packer.nvim]]
end

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init {
  display = {
    -- open_fn = function()
    --   return require("packer.util").float { border = "rounded" }
    -- end,
  },
}

return require("packer").startup(function()
  -- =======================
  use { 'lewis6991/impatient.nvim',
    config = function() require('impatient').enable_profile() end }
  use "wbthomason/packer.nvim" -- package manager
  use "rcarriga/nvim-notify" -- notify
  -- =======================
  -- nvim-tree: directory browser
  use {
    "kyazdani42/nvim-tree.lua",
    requires = {
      "kyazdani42/nvim-web-devicons", -- optional, for file icon
    }
  }
  use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
  use "nvim-lua/plenary.nvim" -- Useful lua functions used in lots of plugins
  -- ============
  -- colorschemes
  use "navarasu/onedark.nvim" -- one dark colorscheme
  use "rafamadriz/neon" -- another dark scheme
  use "rebelot/kanagawa.nvim"
  use "projekt0n/github-nvim-theme"
  -- ===================================
  -- treesitter: language parser
  use { "nvim-treesitter/nvim-treesitter",
    requires = {
      "nvim-treesitter/nvim-treesitter-textobjects", -- enhance texetobject selection
      "romgrk/nvim-treesitter-context", -- show class/function at top
      "JoosepAlviste/nvim-ts-context-commentstring", -- help with comment string
    },
    run = ":TSUpdate",
  }
  use "andymass/vim-matchup"
  use {"nvim-orgmode/orgmode",
    -- config = function() require('orgmode').setup_ts_grammar{} require('orgmode').setup{} end
  }
  -- ===========================
  -- nvim-telescope fuzzy finder
  use { "nvim-telescope/telescope.nvim",
    requires = {
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
      "nvim-telescope/telescope-ui-select.nvim",
      "nvim-telescope/telescope-live-grep-raw.nvim",
      { "tom-anders/telescope-vim-bookmarks.nvim", requires = "MattesGroeger/vim-bookmarks" }
    }
  }
  -- ========
  -- other UI
  use "norcalli/nvim-colorizer.lua" -- show color
  use "folke/trouble.nvim"
  use "nvim-lualine/lualine.nvim" -- status line
  use "folke/which-key.nvim"
  use { "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
  }
  use "lewis6991/gitsigns.nvim"
  -- use "https://tpope.io/vim/fugitive.git" -- for blame?
  -- use { 'TimUntersberger/neogit', -- magit in neovim
  --   requires = 'nvim-lua/plenary.nvim'
  -- }
  -- use "mtdl9/vim-log-highlighting"
  use { 'stevearc/aerial.nvim', -- outline view
    requires = "nvim-treesitter/nvim-treesitter"
  }
  use "simrat39/symbols-outline.nvim" -- symbols overview
  use 'karb94/neoscroll.nvim' -- smooth scroll
  use 'kosayoda/nvim-lightbulb' -- code actions
  use { 'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu', }
  use "lukas-reineke/indent-blankline.nvim" -- indent line
  -- ===
  -- LSP
  use "neovim/nvim-lspconfig" -- enable LSP
  use "williamboman/nvim-lsp-installer" -- simple to use language server installer
  -- use "kosayoda/nvim-lightbulb" -- code action
  use "ray-x/lsp_signature.nvim" -- show function signature when typing
  -- =====
  -- Editor
  -- use "honza/vim-snippets" -- snippets
  -- use "SirVer/ultisnips" -- snippets engine that I have been using
  use "L3MON4D3/LuaSnip" -- new snippets engine
  use "rafamadriz/friendly-snippets"
  use "windwp/nvim-autopairs" -- Autopairs, integrates with both cmp and treesitter
  use "terrortylor/nvim-comment"
  use "numToStr/Comment.nvim"
  use { "hrsh7th/nvim-cmp",
    requires = {
      -- {"quangnguyen30192/cmp-nvim-ultisnips"}, -- ultisnips
      { "hrsh7th/cmp-buffer" }, -- buffer completions
      { "hrsh7th/cmp-path" }, -- path completions
      { "hrsh7th/cmp-cmdline" }, -- cmdline completions
      { "hrsh7th/cmp-nvim-lua" },
      { "hrsh7th/cmp-nvim-lsp"}, -- lsp completions
      { "saadparwaiz1/cmp_luasnip" }, -- snippet completions
      { "bydlw98/cmp-env" }, -- environment variables
      { "paopaol/cmp-doxygen" }, -- doxygen. require treesitter
      { "amarakon/nvim-cmp-lua-latex-symbols" }, -- latex symbols
      { "lukas-reineke/cmp-rg" }, -- ripgrep
    }
  }
  use "gpanders/editorconfig.nvim" -- respect editorconfig
  use "ethanholz/nvim-lastplace" -- auto return back to the last modified positon when open a file
  use "nathom/filetype.nvim"
  use { "jose-elias-alvarez/null-ls.nvim", -- code formatting
    requires = "nvim-lua/plenary.nvim",
  }
  -- =====
  -- tools
  use { "Shatur/neovim-cmake", -- cmake convenient functions
    requires = {
      "nvim-lua/plenary.nvim",
      "mfussenegger/nvim-dap"
    },
  }
  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end)
