# NeoVim Lua configuration

Directory structure

```
├── colorscheme.lua
├── keymaps.lua
├── options.lua
├── plugins.lua
├── editor
├── lsp
├── ui
├── _colorschemes
└── _lang
```

No `init.lua` under direcotries with `_` suffix.
The files therein are loaded by those in other directories.
For example, files in `_colorschemes` are selectively loaded by `colorscheme.lua`.