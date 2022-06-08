-- ltex setting: https://valentjn.github.io/ltex/settings.html
local M = {
  settings = {
    ltex = {
      enabled= {"latex", "tex", "bib", "md"},
      -- enabled= {"latex", "bib", "org", "tex"},
      checkFrequency="save",
      language="en-US",
      additionalRules = {
        motherTongue = "zh-CN",
      }
    }
  }
}

return M
