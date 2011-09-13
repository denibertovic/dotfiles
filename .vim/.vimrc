set sts=2 
set sw=2 
set et
syntax on
set number
set showmatch

" Switch on filetype detection and loads 
" indent file (indent.vim) for specific file types
filetype indent on
filetype on
set autoindent " Copy indent from the row above
set si " Smart indent

" Ignore case in searches
set ic

""""""""""""""""""""""""""""""""""
" Some other confy settings
""""""""""""""""""""""""""""""""""
" set nu " Number lines
set hls " highlight search
set lbr " linebreak


" Key mappigns

" set special keys
map  <Esc>[7~ <Home>
map  <Esc>[8~ <End>
imap <Esc>[7~ <Home>
imap <Esc>[8~ <End>

" exit and write key mappings
nmap <C-Q> :q!<CR>
nmap <C-W> :w<CR>


" tab manipulation
nnoremap <C-j> :tabprev<cr>
nnoremap <C-k> :tabnext<cr>

" copy paste bindinds
nnoremap <C-a> yG
nnoremap <C-p> p

" yw y$ - yank til end of word or file
