return {

  -- quick toggle between true/false
  {
    "rmagatti/alternate-toggler",
    opts = {
      alternates = {
        ["=="] = "!=",
        ["ON"] = "OFF",
        ["On"] = "Off",
        ["false"] = "true",
      },
    },
    event = "VeryLazy",
  },

  -- align code
  { "junegunn/vim-easy-align" },

  -- transpose text fields
  { "salsifis/vim-transpose" },

  -- respect editorconfig
  { "gpanders/editorconfig.nvim" },

  -- respect tab and indentation in current file
  { "tpope/vim-sleuth" },

  { "windwp/nvim-autopairs" },

  -- Snippets
  { "Sirver/ultisnips", event = { "InsertEnter" } },
}
