" Auto add head info
" .py file into add header
function HeaderPython()
    call setline(1, "#!/usr/bin/env python")
    call append(1, "# -*- coding: utf-8 -*-")
    normal G
    normal o
endf
autocmd bufnewfile *.py call HeaderPython()
" .sh file into add header
function HeaderSh()
    call setline(1, "#!/usr/bin/env bash")
    normal G
    normal o
endf
autocmd bufnewfile *.sh call HeaderSh()
