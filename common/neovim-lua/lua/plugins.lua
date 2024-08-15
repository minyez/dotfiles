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
  "rcarriga/nvim-notify", -- notify
  -- ====== start UI
  -- {
  --   "folke/noice.nvim",
  --   config = function()
  --     require("noice").setup({
  --       cmdline = {
  --         view = "cmdline",
  --       },
  --       lsp = {
  --         signature = {
  --           enabled = false
  --         },
  --       }
  --     })
  --   end,
  --   dependencies = {
  --     "MunifTanjim/nui.nvim",
  --     "rcarriga/nvim-notify", -- notify
  --   }
  -- },
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
  {
    "kylechui/nvim-surround", -- for quick modification of text surroundings
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
        require("nvim-surround").setup({
            -- Configuration here, or leave empty to use defaults
        })
    end
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer Local Keymaps (which-key)",
      },
    },
  },
  "folke/todo-comments.nvim",
  "numToStr/Comment.nvim",
  -- {
  --   'lewis6991/spaceless.nvim', -- remove additional space when editing, not touch those unedited
  --   -- might have issue with numToStr/Comment, see https://github.com/lewis6991/spaceless.nvim/issues/9
  --   config = function()
  --     require('spaceless').setup()
  --   end
  -- },
  {
    "HakonHarnes/img-clip.nvim", -- Effortlessly embed images into any markup language
    event = "BufEnter",
    opts = {
      -- add options here
      -- or leave it empty to use the default settings
    },
  },
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
      { "tom-anders/telescope-vim-bookmarks.nvim", dependencies = "MattesGroeger/vim-bookmarks" },
      { -- mintelligent prioritization when selecting files from editing history
        "nvim-telescope/telescope-frecency.nvim",
        dependencies = {
          "kkharji/sqlite.lua",
        },
      },
    }
  },
  "salsifis/vim-transpose",
  {
    'nvim-orgmode/orgmode', -- for Emacs orgmode file
    ft = {'org'},
  },
  "norcalli/nvim-colorizer.lua", -- show color
  {
    "lukas-reineke/indent-blankline.nvim", -- indent line
    main = "ibl", opts = {}
  },
  "gpanders/editorconfig.nvim", -- respect editorconfig
  "tpope/vim-sleuth", -- respect tab and indentation in current file
  "ethanholz/nvim-lastplace", -- auto return back to the last modified positon when open a file
  -- "nathom/filetype.nvim",

  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects", -- enhance texetobject selection
      "nvim-treesitter/nvim-treesitter-context", -- show class/function at top
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
    config = function()
      require("nvim-navic").setup({
        lsp = {
          auto_attach = true,
        }
      })
    end
  },
  -- use "williamboman/nvim-lsp-installer" -- simple to use language server installer
  "williamboman/mason.nvim",
  "williamboman/mason-lspconfig.nvim",
  "WhoIsSethDaniel/mason-tool-installer.nvim",
  "mhartington/formatter.nvim",
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
      { "micangl/cmp-vimtex" }, -- vimtex support
    }
  },
  --  to allow completion in code snippets inside markdown (maybe org?)
  -- WARN: not working yet
  -- {
  --   "jmbuhr/otter.nvim",
  --   dependencies = {
  --     'hrsh7th/nvim-cmp',
  --     'neovim/nvim-lspconfig',
  --     'nvim-treesitter/nvim-treesitter'
  --   },
  -- },
  -- ====== end cmp
  -- {
  --   'quarto-dev/quarto-nvim',
  --   dependencies = {
  --     'hrsh7th/nvim-cmp',
  --     {
  --       'jmbuhr/otter.nvim',
  --       config = function()
  --         require 'otter.config'.setup {
  --           -- lsp = {
  --           --   hover = {
  --           --     border = require 'misc.style'.border
  --           --   }
  --           -- }
  --         }
  --       end,
  --     },
  --     'neovim/nvim-lspconfig',
  --     'nvim-treesitter/nvim-treesitter',
  --   },
  --   config = function()
		-- 	require'quarto'.setup({
  --       debug = false,
  --       closePreviewOnExit = true,
  --       lspFeatures = {
  --         enabled = true,
  --         languages = { 'r', 'python', 'julia', 'bash', 'lua' },
  --         chunks = 'curly', -- 'curly' or 'all'
  --         diagnostics = {
  --           enabled = true,
  --           triggers = { "BufWritePost" }
  --         },
  --         completion = {
  --           enabled = true,
  --         },
  --       },
  --       keymap = {
  --         hover = 'K',
  --         definition = 'gd'
  --       },
  --     })
		-- end
  -- },
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
  -- -- for inserting license in file. Commented out due to error on init.el:373
  -- {
  --   url = "https://git.sr.ht/~reggie/licenses.nvim",
  --   config = function()
  --     require('licenses').setup({
  --       copyright_holder = 'Min-Ye Zhang',
  --       email = 'minyez.physchem@gmail.com',
  --       license = 'MIT'
  --     })
  --   end
  -- },
  -- {"L3MON4D3/LuaSnip", build = "make install_jsregexp" }, -- new snippets engine
  "rafamadriz/friendly-snippets",
  -- ====== end snippets

  -- Modern folding
  -- {'kevinhwang91/nvim-ufo', dependencies = 'kevinhwang91/promise-async'},

  -- More sensible search and jump between matched instances
  -- https://github.com/kevinhwang91/nvim-hlslens
  {
    'kevinhwang91/nvim-hlslens',
     config = function()
       require("hlslens").setup()
       local kopts = {noremap = true, silent = true}
      vim.api.nvim_set_keymap('n', 'n',
        [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]],
        kopts)
      vim.api.nvim_set_keymap('n', 'N',
        [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]],
        kopts)
      vim.api.nvim_set_keymap('n', '*', [[*<Cmd>lua require('hlslens').start()<CR>]], kopts)
      vim.api.nvim_set_keymap('n', '#', [[#<Cmd>lua require('hlslens').start()<CR>]], kopts)
      vim.api.nvim_set_keymap('n', 'g*', [[g*<Cmd>lua require('hlslens').start()<CR>]], kopts)
      vim.api.nvim_set_keymap('n', 'g#', [[g#<Cmd>lua require('hlslens').start()<CR>]], kopts)
     end
  },
  -- folding preview
  {"anuvyklack/fold-preview.nvim", dependencies = "anuvyklack/keymap-amend.nvim", config = true },

  "andymass/vim-matchup",
  "simrat39/symbols-outline.nvim", -- symbols overview
  'karb94/neoscroll.nvim', -- smooth scroll

  -- { 'weilbith/nvim-code-action-menu',
  --   cmd = 'CodeActionMenu', },
  "aznhe21/actions-preview.nvim",

  "fladson/vim-kitty",
  "junegunn/vim-easy-align",
  "echasnovski/mini.align",
  {
    "rmagatti/alternate-toggler", -- quick toggle between true/false
    config = function()
    require("alternate-toggler").setup {
      alternates = {
        ["=="] = "!=",
        ["ON"] = "OFF",
        ["On"] = "Off",
      }
    }
    end,
    event = "VeryLazy"
  },
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
  -- Integration with cmake.
  -- I don't understand how the cache work.
  -- It seems I have to edit using the CMakeSetting command, but cannot edit the cache file directly.
  'Civitasv/cmake-tools.nvim',
  {
    "ekickx/clipboard-image.nvim", -- for pasting images to markdown
    config = function()
      require("clipboard-image").setup({
        org = {
          affix = "[[file:%s]]",
        }
      })
    end,
  },
  -- HACK: for latest neovim version, modify health.lua therein 
  -- local health = vim.health or require "health"
  -- instead of local health = require "health"
}

local opts = {}

lazy.setup(plugins, opts)
