let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
set shiftwidth=2
setlocal formatprg=hindent
