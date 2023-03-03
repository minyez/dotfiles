-- original from https://ravenxrz-github-io-ravenxrz.vercel.app/archives/35a336e5.html
local status_ok, configs = pcall(require, "nvim-treesitter.configs")
if not status_ok then
  vim.notify("treesitter not found!")
  return
end

configs.setup {
  -- ensure_installed = {"cpp", "lua", "c", "python", "go"}, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = {"cpp", "lua", "c", "python", "fortran", "org",}, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  sync_install = false, -- install languages synchronously (only applied to `ensure_installed`)
  ignore_install = { "" }, -- List of parsers to ignore installing
  autopairs = {
    enable = true,
  },
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { "" }, -- list of language that will be disabled
    additional_vim_regex_highlighting = true,
  },
  indent = { enable = false, disable = { "yaml", "cpp", "toml" } },
  -- when cpp is not disable, there will be bad indentation after for {}
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
    -- for config, see https://github.com/JoosepAlviste/nvim-ts-context-commentstring
    config = {
      cpp = { __default = '// %s', }
    }
  },

  -- https://github.com/nvim-treesitter/nvim-treesitter#incremental-selection
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn", -- set to `false` to disable one of the mappings
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },

  -- textobjects extension settings
  -- https://github.com/nvim-treesitter/nvim-treesitter-textobjects
  textobjects = {
    select = {
      enable = true,
      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]]"] = "@function.outer",
        -- ["]]"] = "@class.outer",
      },
      -- goto_next_end = {
      --   ["jF"] = "@function.outer",
      --   ["]["] = "@class.outer",
      -- },
      goto_previous_start = {
        ["[["] = "@function.outer",
        -- ["[["] = "@class.outer",
      },
      -- goto_previous_end = {
      --   ["kF"] = "@function.outer",
      --   ["[]"] = "@class.outer",
      -- },
    },
    lsp_interop = {
      enable = true,
      border = 'none',
      peek_definition_code = {
        ["<leader>df"] = "@function.outer",
        ["<leader>dF"] = "@class.outer",
      },
    },
  },
  -- matchup plugins
  -- https://github.com/andymass/vim-matchup
  matchup = {
    enable = true,              -- mandatory, false will disable the whole extension
    -- disable = { "c", "ruby" },  -- optional, list of language that will be disabled
    -- [options]
  },
}
