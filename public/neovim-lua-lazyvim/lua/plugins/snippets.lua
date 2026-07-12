return {

  -- https://github.com/chrisgrieser/nvim-scissors for easy snippet modification
  {
    "chrisgrieser/nvim-scissors",
    opts = {
      snippetDir = vim.fn.expand("~/.config/snippets/vscode"),
    },
    keys = {
      {
        "<leader>se",
        function()
          require("scissors").editSnippet()
        end,
        desc = "Snippet: Edit",
      },
      {
        "<leader>sA",
        function()
          require("scissors").addNewSnippet()
        end,
        mode = { "n", "x" },
        desc = "Snippet: Add",
      },
    },
  },

  -- -- Use LazyVim's LuaSnip extra
  -- -- This imports is now in lazyvim.json
  -- { import = "lazyvim.plugins.extras.coding.luasnip" },

  {
    "saghen/blink.cmp",
    opts = {
      snippets = {
        preset = "luasnip",
      },
      sources = {
        per_filetype = {
          aimsin = { inherit_defaults = true, "omni" },
          librpain = { inherit_defaults = true, "omni" },
        },
      },
    },
  },

  -- Load VS Code-style snippets
  {
    "L3MON4D3/LuaSnip",
    opts = function(_, opts)
      require("luasnip.loaders.from_vscode").lazy_load({
        paths = {
          vim.fn.expand("~/.config/snippets/vscode"),
        },
      })
      return opts
    end,
  },
}
