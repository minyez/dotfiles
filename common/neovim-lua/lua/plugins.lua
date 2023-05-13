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
  {
    "folke/noice.nvim",
    config = function()
      require("noice").setup({
        cmdline = {
          view = "cmdline",
        },
        lsp = {
          signature = {
            enabled = false
          },
        }
      })
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify", -- notify
    }
  },
  "wellle/context.vim", -- showing meaningful code context (condition, loop, ...) at the top
  "folke/trouble.nvim",
  {
    "nvim-lualine/lualine.nvim", -- status line
    dependencies = {
      "linrongbin16/lsp-progress.nvim",
      "SmiteshP/nvim-navic",
    }
  },
  {
    "linrongbin16/lsp-progress.nvim", -- display a progress icon of LSP indexing, TODO not working
                                      -- might try https://github.com/j-hui/fidget.nvim later
    event = { 'VimEnter' },
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
        require('lsp-progress').setup()
    end
  },
  "folke/which-key.nvim",
  "folke/todo-comments.nvim",
  "numToStr/Comment.nvim",
  "lewis6991/gitsigns.nvim",
  {
    "TimUntersberger/neogit",
    dependencies = { "nvim-lua/plenary.nvim" },
  },
  -- nvim-tree: directory browser
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = {
      "nvim-tree/nvim-web-devicons", -- optional, for file icon
    }
  },
  -- dashboard
  {
    'glepnir/dashboard-nvim',
    event = 'VimEnter',
    dependencies = {
      "nvim-tree/nvim-web-devicons", -- optional, for file icon
    }
  },
  {
    'sindrets/diffview.nvim',
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- optional, for file icon
    }
  },
  "rickhowe/diffchar.vim",
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
  {
    'nvim-orgmode/orgmode', -- for Emacs orgmode file
    ft = {'org'},
  },
  "norcalli/nvim-colorizer.lua", -- show color
  "folke/trouble.nvim",
  "lukas-reineke/indent-blankline.nvim", -- indent line
  "gpanders/editorconfig.nvim", -- respect editorconfig
  "ethanholz/nvim-lastplace", -- auto return back to the last modified positon when open a file
  -- "nathom/filetype.nvim",

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
  {
    "stevearc/stickybuf.nvim",
    config = function()
      require("stickybuf").setup()
    end
  },
  "windwp/nvim-autopairs",
  "mg979/vim-visual-multi",

  -- ====== start LSP
  "neovim/nvim-lspconfig", -- enable LSP
  {
    "SmiteshP/nvim-navic",
    dependencies = {"neovim/nvim-lspconfig"},
  },
  -- use "williamboman/nvim-lsp-installer" -- simple to use language server installer
  "williamboman/mason.nvim",
  "williamboman/mason-lspconfig.nvim",
  "WhoIsSethDaniel/mason-tool-installer.nvim",
  "ray-x/lsp_signature.nvim", -- show function signature when typing
  -- ====== end LSP
  -- ====== start cmp
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "quangnguyen30192/cmp-nvim-ultisnips"}, -- ultisnips completion frontend
      { "hrsh7th/cmp-buffer" }, -- buffer completions
      { "hrsh7th/cmp-path" }, -- path completions
      { "hrsh7th/cmp-cmdline" }, -- cmdline completions
      { "hrsh7th/cmp-nvim-lua" },
      { "hrsh7th/cmp-nvim-lsp" }, -- lsp completions
      { "hrsh7th/cmp-nvim-lsp-signature-help" },
      -- { "saadparwaiz1/cmp_luasnip" }, -- snippet completions
      { "bydlw98/cmp-env" }, -- environment variables
      { "paopaol/cmp-doxygen" }, -- doxygen. require treesitter
      -- { "amarakon/nvim-cmp-lua-latex-symbols" }, -- latex symbols
      { "lukas-reineke/cmp-rg" }, -- ripgrep
    }
  },
  -- ====== end cmp
  -- ====== start snippets
  {
    "SirVer/ultisnips", -- snippets engine that I have been using
    dependencies = {"honza/vim-snippets"}, -- default snippets
    config = function()
      -- add local snippet directory to search and set as the edit path
      vim.cmd [[ let g:UltiSnipsSnippetDirectories=[$HOME.'/snippets/UltiSnips'] ]]
      vim.cmd [[ let g:UltiSnipsSnippetStorageDirectoryForUltiSnipsEdit=$HOME.'/snippets/UltiSnips' ]]
    end
  },
  -- {"L3MON4D3/LuaSnip", build = "make install_jsregexp" }, -- new snippets engine
  "rafamadriz/friendly-snippets",
  -- ====== end snippets

  -- Modern folding
  -- {'kevinhwang91/nvim-ufo', dependencies = 'kevinhwang91/promise-async'},
  -- folding preview
  {"anuvyklack/fold-preview.nvim", dependencies = "anuvyklack/keymap-amend.nvim", config = true },

  "andymass/vim-matchup",
  "simrat39/symbols-outline.nvim", -- symbols overview
  'karb94/neoscroll.nvim', -- smooth scroll

  { 'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu', },

  "fladson/vim-kitty",
  "junegunn/vim-easy-align",
  "echasnovski/mini.align",
  "rmagatti/alternate-toggler", -- quick toggle between true/false
  -- comment out hologram for some error on macos
  -- {
  --   "edluffy/hologram.nvim",
  --   -- lazy = true,
  --   config = function()
  --       require("hologram").setup({
  --         auto_display = true -- WIP automatic markdown image display, may be prone to breaking
  --       })
  --   end
  -- },
}

local opts = {}

lazy.setup(plugins, opts)
