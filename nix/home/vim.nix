{
  config,
  pkgs,
  lib,
  ...
}: let
  black = pkgs.vimUtils.buildVimPlugin {
    pname = "black";
    version = "2023-02-14";
    src = pkgs.fetchFromGitHub {
      owner = "psf";
      repo = "black";
      rev = "d9b8a6407e2f46304a8d36b18e4a73d8e0613519";
      sha256 = "sha256-UKW40vnV6dEIFhlyQmPQPF7tJUvCDE/LdqzWrP8QiCQ=";
    };
  };
  ddc = pkgs.vimUtils.buildVimPlugin {
    pname = "ddc";
    version = "";
    src = pkgs.fetchFromGitHub {
      owner = "Shougo";
      repo = "ddc.vim";
      rev = "e9342a9b24fc7d5b08408ae5b01f15fb3d843f2f";
      sha256 = "sha256-jmvG0VSpqKhywlPybA7c48ziVk5HFyrT99NLdC6+SaE=";
    };
  };

  my-dpaste-fork = pkgs.vimUtils.buildVimPlugin {
    pname = "dpaste";
    version = "2020-03-10";
    src = pkgs.fetchFromGitHub {
      owner = "denibertovic";
      repo = "Dpaste.com-Plugin";
      rev = "95bf653e2f214fa4962ec848256789997785fa01";
      sha256 = "sha256-0PsTz45n5+oAyAcdJQaq0W5/1DudzkxpX3PgYuO+V/c=";
    };
  };
in {
  programs.vim = {
    enable = true;
    defaultEditor = true;
    extraConfig = ''
      set nocompatible              " be iMproved, required
      set showcmd
      filetype off                  " required
      " =============== Sensible defaults ===============
      " Whiste space: Show tabs
      set list
      set listchars=tab:\|\
      set linebreak

      " use systems primary clipboad
      set clipboard=unnamed

      filetype plugin indent on    " required

      " re-map leader key
      " let mapleader=","
      let maplocalleader = "\\"

      set completeopt=menu,longest

      " Quickly edit/reload the vimrc file
      " edit
      nmap <silent> <leader>ev :e $MYVIMRC<CR>
      " reload
      nmap <silent> <leader>sv :so $MYVIMRC<CR>

      " tab manipulation
      " nnoremap <C-j> :tabprev<cr>
      " nnoremap <C-k> :tabnext<cr>

      " buffer manipulation
      :noremap <C-j> :bprev<CR>
      :noremap <C-k> :bnext<CR>

      " window manipulation
      :map <F6> <C-W>w

      " Don't fold blocks
      let g:haskell_conceal = 0
      autocmd FileType haskell setlocal nofoldenable
      autocmd FileType cabal setlocal nofoldenable
      autocmd FileType markdown setlocal nofoldenable
      autocmd FileType yaml setlocal nofoldenable

      set pastetoggle=<F2>

      " markdown folding
      let g:vim_markdown_folding_disabled=1
      let g:vim_markdown_conceal = 0
      let g:vim_markdown_conceal_code_blocks = 0
      autocmd FileType markdown set tw=120

      " tabs and spaces and indent
      set tabstop=4
      set shiftwidth=4
      set expandtab
      set softtabstop=4
      set autoindent

      let g:indentLine_enabled = 0

      " line numbers
      set number

      " Close buffer
      nnoremap <C-d> :bd<CR>

      " Enable to switch between buffers without saving
      set hidden

      " highlight trailing whitespace
      highlight ExtraWhitespace ctermbg=red guibg=red
      match ExtraWhitespace /\s\+$/
      autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
      autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
      autocmd InsertLeave * match ExtraWhitespace /\s\+$/
      autocmd BufWinLeave * call clearmatches()
      " remove trailing whitespace on save
      autocmd BufWritePre * :%s/\s\+$//e

      " status line at all times
      set laststatus=2

      " Airline
      let g:airline_powerline_fonts = 1
      let g:airline#extensions#ale#enabled = 1


      " Spelling
      nnoremap <F7> :setlocal spell! spelllang=en_us<CR>
      autocmd FileType gitcommit setlocal spell

      " =============== PLUGINS ===============


      " =============== CTRLP ===============
      " Custom ctrlp ignore options
      let g:ctrlp_custom_ignore = {
        \ 'dir':  '\v[\/]\.(git|hg|svn|spago)|(dist|node_modules|build|output)$',
        \ 'file': '\v\.(pyc)$',
        \ }

      " =============== Color Theme ===============
      colorscheme busybee
      highlight SpellBad ctermbg=088

      " =============== Hoogle ===============
      au BufNewFile,BufRead *.hs map <buffer> <F1> :Hoogle
      " au BufNewFile,BufRead *.hs map <buffer> <S-F1> :HoogleLine<CR>

      " =============== Easymotion> =============>==

      " re-map default <Leader> <Leader> to just <Leader>
      " nmap s <Plug>(easymotion-sn)
      " nmap t <Plug>(easymotion-tn)
      " don't allow abuse of hjkl
      " map <localleader>l <Plug>(easymotion-lineforward)
      " map <localleader>j <Plug>(easymotion-j)
      " map <localleader>k <Plug>(easymotion-k)
      " map <localleader>h <Plug>(easymotion-linebackward)

      let g:EasyMotion_startofline = 0 " keep cursor colum when JK motion

      " =============== Yankring ===============
      nnoremap <silent> <F12> :YRShow<CR>

      " ================= ALE =====================
      "
      " show multi-line errors in vim by combining your <Leader> key + ?
      " autocmd FileType haskell nnoremap <buffer> <leader>a :call ale#cursor#ShowCursorDetail()<cr>
      let g:ale_linters = {
            \   'haskell': ['hlint'],
            \   'python': ['flake8']
            \}

      let g:ale_fixers = {
            \   'haskell': ['hlint'],
            \   'python': ['black']
            \}


      let g:ale_fix_on_save = 1

      " nmap <silent> <C-k> <Plug>(ale_previous_wrap)
      " nmap <silent> <C-j> <Plug>(ale_next_wrap)

      "let g:ale_lint_on_text_changed = 'never'

      highlight ALEWarning ctermbg=none

      " TEMPORARY FIX for issue: https://github.com/w0rp/ale/issues/1334
      " Seem to be fixed NOW....check for a while and then remove later
      " UPDATE: Updated plugin has fixed this error and the workaround is not needed
      " anymore
      "let g:ale_echo_cursor = 1

      " =============== Tabular ===============
      let g:haskell_tabular = 1

      vmap a= :Tabularize /=<CR>
      vmap a; :Tabularize /::<CR>
      vmap a- :Tabularize /-><CR>

      " =============== UltiSnips ===============
      let g:UltiSnipsExpandTrigger="<tab>"

      " ormolu
      let g:ormolu_disable = 1
      nnoremap tf :call RunOrmolu()<CR>
      nnoremap to :call ToggleOrmolu()<CR>
      nnoremap td :call DisableOrmolu()<CR>
      xnoremap tb :<c-u>call OrmoluBlock()<CR>

      " ========= Haskell specific ==========
      " au FileType haskell compiler ghc
      " au FileType haskell setlocal makeprg=stack

      " Neo format purescript
      let g:neoformat_enabled_purescript = ['purty']

      function! SaveWithTS(filename) range
          let l:extension = '.' . fnamemodify( a:filename, ':e' )
          if len(l:extension) == 1
              let l:extension = '.txt'
          endif

          let l:filename = escape( fnamemodify(a:filename, ':r') . strftime("-%Y-%m-%dT%H-%M-%S") . l:extension, ' ' )

          execute "write " . l:filename
      endfunction

      command! -nargs=1 SWT call SaveWithTS( <q-args> )

      " Black NOTE: We're using ALE for this but I'll leave this here just in case
      " augroup black_on_save
      "   autocmd!
      "   autocmd BufWritePre *.py Black
      " augroup end
    '';
    plugins = [
      pkgs.vimPlugins.vim-commentary
      pkgs.vimPlugins.tabular
      pkgs.vimPlugins.vim-sensible
      pkgs.vimPlugins.vim-surround
      pkgs.vimPlugins.vim-repeat
      pkgs.vimPlugins.YankRing-vim
      pkgs.vimPlugins.auto-pairs
      pkgs.vimPlugins.ultisnips
      pkgs.vimPlugins.vim-snippets
      pkgs.vimPlugins.vim-togglelist
      pkgs.vimPlugins.asyncrun-vim
      pkgs.vimPlugins.nerdtree
      pkgs.vimPlugins.vim-easymotion
      pkgs.vimPlugins.ctrlp-vim
      pkgs.vimPlugins.sparkup
      pkgs.vimPlugins.vim-ormolu
      pkgs.vimPlugins.ale
      pkgs.vimPlugins.haskell-vim
      black
      pkgs.vimPlugins.vimproc-vim
      pkgs.vimPlugins.purescript-vim
      pkgs.vimPlugins.vim-airline
      ddc
      pkgs.vimPlugins.denops-vim
      pkgs.vimPlugins.vim-signify
      pkgs.vimPlugins.indentLine
      pkgs.vimPlugins.vim-markdown
      pkgs.vimPlugins.vim-colorschemes
      my-dpaste-fork
      pkgs.vimPlugins.todo-txt-vim
      pkgs.vimPlugins.dhall-vim
      pkgs.vimPlugins.neoformat
      pkgs.vimPlugins.vim-nix
      pkgs.vimPlugins.webapi-vim
      pkgs.vimPlugins.vim-gist
    ];
  };
}
