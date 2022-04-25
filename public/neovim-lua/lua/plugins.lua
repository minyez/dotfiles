-- original file from https://github.com/ravenxrz/dotfiles/blob/master/nvim/lua/user/plugins.lua

return require("packer").startup(function()
-- =======================
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
-- ===================================
-- treesitter: language parser
  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  }
  use "nvim-treesitter/nvim-treesitter-textobjects"  -- enhance texetobject selection
  use "romgrk/nvim-treesitter-context"  -- show class/function at top
  use "andymass/vim-matchup"
-- ===========================
-- nvim-telescope fuzzy finder
  use "nvim-telescope/telescope.nvim"
  use {"nvim-telescope/telescope-fzf-native.nvim", run = "make" }
  use "nvim-telescope/telescope-ui-select.nvim"
  use "nvim-telescope/telescope-live-grep-raw.nvim"
-- =====
-- Editor
  use "honza/vim-snippets" -- snippets
  use "SirVer/ultisnips" -- snippets engine that I have been using
  use "windwp/nvim-autopairs" -- Autopairs, integrates with both cmp and treesitter
  use "terrortylor/nvim-comment"
  use { "hrsh7th/nvim-cmp",
    requires = { {"quangnguyen30192/cmp-nvim-ultisnips"}, -- ultisnips
                 {"hrsh7th/cmp-buffer"}, -- buffer completions
                 {"hrsh7th/cmp-path"}, -- path completions
                 {"hrsh7th/cmp-cmdline"}, -- cmdline completions
                 {"hrsh7th/cmp-nvim-lua"},
                 -- {"saadparwaiz1/cmp_luasnip"}, -- snippet completions
    }
  }
--  use "hrsh7th/cmp-nvim-lsp"
end)
