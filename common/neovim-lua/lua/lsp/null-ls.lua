local pkg = "null-ls"
local status_ok, null_ls = pcall(require, pkg)
if not status_ok then
  vim.notify(pkg .. " not found!")
  return
end

-- currently not work, due to pack error
-- Error detected while processing CursorHold Autocommands for "<buffer=1>":
-- E5108: Error executing lua ...te/pack/packer/start/null-ls.nvim/lua/null-ls/client.lua:39: bad argument #1 to 'unpack' (table expected, got string)
-- stack traceback:
--         [C]: in function 'unpack'
--         ...te/pack/packer/start/null-ls.nvim/lua/null-ls/client.lua:39: in function 'capability_is_disabled'
--         ...te/pack/packer/start/null-ls.nvim/lua/null-ls/client.lua:50: in function 'supports_method'
--         ....mount_nvimU2r5vK/usr/share/nvim/runtime/lua/vim/lsp.lua:1470: in function 'fn'
--         ....mount_nvimU2r5vK/usr/share/nvim/runtime/lua/vim/lsp.lua:164: in function 'for_each_buffer_client'
--         ....mount_nvimU2r5vK/usr/share/nvim/runtime/lua/vim/lsp.lua:1469: in function 'request'
--         ...nt_nvimU2r5vK/usr/share/nvim/runtime/lua/vim/lsp/buf.lua:474: in function 'document_highlight'
null_ls.setup({
  sources = {
    null_ls.builtins.formatting.clang_format,
  },
  -- https://github.com/jose-elias-alvarez/null-ls.nvim/discussions/355, not sure if corre here
  on_attach = function(client, bufnr)
    if client.resolved_capabilities.document_highlight then
      vim.cmd("autocmd CursorHold <buffer> silent! lua vim.lsp.buf.document_highlight()")
    end
  end
})
