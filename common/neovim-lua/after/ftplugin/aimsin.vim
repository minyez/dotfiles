
if exists("b:did_aimsin_ftplugin")
  finish
endif
let b:did_aimsin_ftplugin = 1

set syntax=aimsin

setlocal comments=:#
setlocal commentstring=#%s
setlocal completefunc=aimsincomplete#CompleteOption
