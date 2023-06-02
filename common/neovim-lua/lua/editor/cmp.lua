-- original file from https://github.com/ravenxrz/dotfiles/blob/master/nvim/lua/user/conf/cmp.lua
-- the overall cmp config
local cmp_status_ok, cmp = pcall(require, "cmp")
if not cmp_status_ok then
  vim.notify("cmp not found!")
  return
end

local snip_status_ok, cmpulti = pcall(require, "cmp_nvim_ultisnips")
if not snip_status_ok then
  vim.notify("cmp_nvim_ultisnips not found!")
else
  cmpulti.setup {
    filetype_source = "treesitter",
    show_snippets = "all",
    documentation = function(snippet)
      return snippet.description
    end
  }
end

local check_backspace = function()
  local col = vim.fn.col "." - 1
  return col == 0 or vim.fn.getline("."):sub(col, col):match "%s"
end

--   פּ ﯟ   some other good icons
local kind_icons = {
  Text = "",
  Method = "m",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "",
  Interface = "",
  Module = "",
  Property = "",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = "",
}
-- find more here: https://www.nerdfonts.com/cheat-sheet

local cmpulti_mappings = require("cmp_nvim_ultisnips.mappings")
cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  mapping = {
    ["<C-k>"] = cmp.mapping.select_prev_item(),
    ["<C-j>"] = cmp.mapping.select_next_item(),
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    -- ["<C-e>"] = cmp.mapping {
    --   i = cmp.mapping.abort(),
    --   c = cmp.mapping.close(),
    -- },
    ["<C-e>"] = cmp.mapping.confirm { select = true },
    -- Accept currently selected item. If none selected, `select` first item.
    -- Set `select` to `false` to only confirm explicitly selected items.
    ["<CR>"] = cmp.mapping.confirm { select = false },
    ["<Tab>"] = cmp.mapping(function(fallback)
      cmpulti_mappings.expand_or_jump_forwards(fallback)
    end, {
      "i",
      "s",
      "c",
    }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      cmpulti_mappings.jump_backwards(fallback)
    end, {
      "i",
      "s",
      "c",
    }),
  },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      -- Kind icons
      vim_item.kind = string.format("%s ", kind_icons[vim_item.kind])
      -- vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
      vim_item.menu = ({
        otter = "[🦦]",
        nvim_lsp = "[LSP]",
        nvim_lua = "[NVIM_LUA]",
        ultisnips = "[Snip]",
        path = "[Path]",
        orgmode = "[ORG]",
        env = "[ENV]",
        doxygen = "[DOX]",
        rg = "[RG]",
      })[entry.source.name]
      return vim_item
    end,
  },
  sources = {
    -- { name = "luasnip" },
    { name = "ultisnips" },
    { name = "otter" },
    { name = "nvim_lsp" },
    { name = "nvim_lsp_signature_help" },
    { name = "nvim_lua" },
    { name = "buffer" },
    { name = "path" },
    -- { name = "cmdline" }, -- trigger cmdline here will make you tab once only
    { name = "orgmode" },
    { name = "env" },
    { name = "rg" },
  },
  confirm_opts = {
    behavior = cmp.ConfirmBehavior.Replace,
    select = false,
  },
  window = {
--    documentation = "native",
    documentation = {
      border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
    },
  },
  experimental = {
    ghost_text = false,
    -- native_menu = false,
  },
  -- view = {
  --   entries = "native", -- somehow will overwrite formatting
  -- },
}

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  sources = {
    { name = 'buffer' }
  }
})

-- Use doxygen source for `@`
cmp.setup.cmdline('@', {
  sources = {
    { name = 'doxygen' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  sources = cmp.config.sources({
    { name = 'cmdline' }
  }, {
    { name = 'path' }
  })
})

-- Only enable `lua-latex-symbols` for `tex` and `plaintex` file types
-- require "cmp".setup.filetype({ "tex", "plaintex", "markdown", "org" }, {
--     sources = {
--         { name = "lua-latex-symbols"}
--     }
-- })
