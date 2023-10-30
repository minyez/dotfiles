local pkg = "orgmode"
local status_ok, configs = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

configs.setup_ts_grammar()
configs.setup {
  org_default_notes_file = '~/Library/CloudStorage/Dropbox/org-roam/inbox.org',
  org_highlight_latex_and_related = 'native',
  -- disable indent after heading and list
  org_indent_mode = 'noindent',
}

local status_ok, tsconfigs = pcall(require, "nvim-treesitter.configs")
if status_ok then
  tsconfigs.setup {
    -- If TS highlights are not enabled at all,
    -- or disabled via `disable` prop,
    -- highlighting will fallback to default Vim syntax highlighting
    highlight = {
      enable = true,
      -- disable = {'org'}, -- Remove this to use TS highlighter for some of the highlights (Experimental)
      additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
    },
}
end

if vim.bo.filetype == 'org' then
  vim.cmd([[edit!]])
end
