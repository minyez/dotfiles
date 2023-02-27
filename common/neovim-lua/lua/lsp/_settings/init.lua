local servers = {
  -- "sumneko_lua",
  "lua_ls",
  "pylsp",
  "ltex",
}

local function get_server_settings(name)
  local status, setting = pcall(require, USER_DIR .. ".lsp._settings." .. name)
  if status then
    return setting.settings
  end
  return {}
end

local M = {}

for _, name in ipairs(servers) do
  M[name] = get_server_settings(name)
end

return M
