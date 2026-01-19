function M(colorscheme)
  -- local status_ok, _ = pcall(vim.cmd, "colorscheme " .. colorscheme)
  -- if not status_ok then
  --   vim.notify("colorscheme " .. colorscheme .. " not found!")
  --   return
  -- end

  local cs_avail = { "neon", "onedark", "kanagawa", "github-theme" }
  for _, v in pairs(cs_avail) do
    if v == colorscheme then
      require("config._colorschemes." .. colorscheme)
      return
    end
  end
  vim.notify("requested colorscheme " .. colorscheme .. " not found!")
end

return M
