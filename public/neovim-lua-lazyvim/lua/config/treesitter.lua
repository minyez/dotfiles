-- https://github.com/nvim-treesitter/nvim-treesitter/issues/5751#issuecomment-2311310008
require("vim.treesitter.query").set(
  "markdown",
  "highlights",
  [[
;From MDeiml/tree-sitter-markdown
[
  (fenced_code_block_delimiter)
] @punctuation.delimiter
]]
)
