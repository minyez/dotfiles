return {

  -- side signs for git changes
  { "lewis6991/gitsigns.nvim" },

  -- vim emulation for magit
  { "TimUntersberger/neogit" },

  -- show color
  { "norcalli/nvim-colorizer.lua" },

  -- better visual for matching text
  { "andymass/vim-matchup" },

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
}
