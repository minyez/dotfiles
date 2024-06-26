local pkg = "trouble"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup(
{
  position = "bottom", -- position of the list can be: bottom, top, left, right
  height = 7, -- height of the trouble list when position is top or bottom
  width = 50, -- width of the list when position is left or right
  icons = false, -- use devicons for filenames
  mode = "workspace_diagnostics", -- "workspace_diagnostics", "document_diagnostics", "quickfix", "lsp_references", "loclist"
  fold_open = "", -- icon used for open folds
  action_keys = {
    fold_closed = "", -- icon used for closed folds
    -- close = {},
    close = "q", -- close the list
    cancel = "<esc>", -- cancel the preview and get back to your last window / buffer / cursor
    refresh = "r", -- manually refresh
    jump = { "o", "<tab>" }, -- jump to the diagnostic or open / close folds
    open_split = { "<c-x>" }, -- open buffer in new split
    open_vsplit = { "<c-v>" }, -- open buffer in new vsplit
    open_tab = { "<c-t>" }, -- open buffer in new tab
    jump_close = { "<cr>" }, -- jump to the diagnostic and close the list
    toggle_mode = "m", -- toggle between "workspace" and "document" diagnostics mode
    toggle_preview = "p", -- toggle auto_preview
    hover = "K", -- opens a small popup with the full multiline message
    preview = "P", -- preview the diagnostic location
    close_folds = { "zM", "zm" }, -- close all folds
    open_folds = { "zR", "zr" }, -- open all folds
    toggle_fold = { "zA", "zo", "zc" }, -- toggle fold of current file
    previous = "k", -- preview item
    next = "j", -- next item
    },
  indent_lines = true, -- add an indent guide below the fold icons
  auto_open = false, -- automatically open the list when you have diagnostics
  auto_close = false, -- automatically close the list when you have no diagnostics
  auto_preview = true, -- automatically preview the location of the diagnostic. <esc> to close preview and go back to last window
  auto_fold = false, -- automatically fold a file trouble list at creation
  -- signs = {
  --   -- icons / text used for a diagnostic
  --   error = "",
  --   warning = "",
  --   hint = "",
  --   information = "",
  --   other = "﫠",
  -- },
  -- use_lsp_diagnostic_signs = true, -- enabling this will use the signs defined in your lsp client
  use_diagnostic_signs = true, -- enabling this will use the signs defined in your lsp client
}
)

