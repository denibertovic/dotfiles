{
  config,
  pkgs,
  lib,
  ...
}: let
  utils = import ./utils.nix {inherit config pkgs lib;};
  dotfiles = "${config.home.homeDirectory}/dotfiles";
  tsComments = pkgs.vimUtils.buildVimPlugin {
    pname = "ts-comments";
    version = "2024-11-05";
    src = pkgs.fetchFromGitHub {
      owner = "folke";
      repo = "ts-comments.nvim";
      rev = "2002692ad1d3f6518d016550c20c2a890f0cbf0e";
      sha256 = "sha256-dMw+U1d4flSdr4XCQC8TqZdfHXTLnLrgN0R9hhtmmrs=";
    };
  };
in {
  programs.neovim = {
    enable = true;
    vimAlias = true;
    package = pkgs.neovim-unwrapped;
    plugins = with pkgs.vimPlugins; [
      # coding and lsp
      tsComments
      nvim-ts-context-commentstring
      nvim-lspconfig
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      (nvim-treesitter.withPlugins (p: [
        p.bash
        p.c
        p.css
        p.diff
        p.go
        p.haskell
        p.hcl
        p.html
        p.javascript
        p.jsdoc
        p.json
        p.jsonc
        p.lua
        p.luadoc
        p.luap
        p.markdown_inline
        p.markdown
        p.nix
        p.printf
        p.python
        p.query
        p.regex
        p.rust
        p.terraform
        p.toml
        p.tsx
        p.typescript
        p.vim
        p.vimdoc
        p.xml
        p.yaml
      ]))

      # colorscheme
      tokyonight-nvim
      catppuccin-nvim

      # ui
      nvim-notify
      # I'm not sure I need these "tabs"
      # bufferline-nvim
      lualine-nvim
      indent-blankline-nvim
      noice-nvim
      nui-nvim
      nvim-web-devicons

      # linting
      nvim-lint

      # formatting
      conform-nvim

      # editor
      gitsigns-nvim
      telescope-nvim
      trouble-nvim

      # utils
      mini-nvim
      # vim-commentary
      # vim-surround
      # vim-repeat
      plenary-nvim
      vim-signify
    ];
    extraPackages = with pkgs; [
      lua-language-server
      python312Packages.python-lsp-server
      python312Packages.python-lsp-black
      pyright
      nixd
      black
      stylua
      nodePackages.typescript-language-server
      haskell-language-server
      yaml-language-server
      terraform-ls
      fd
      ripgrep
      alejandra
    ];
    extraConfig = ''
      " For faster startup
      lua vim.loader.enable()
      luafile /home/deni/dotfiles/nix/home/nvim/init.lua
    '';
  };
  home.file = utils.linkHomeFiles {
    # set outOfStoreSymlink = true and recursive = true to recursively link all files within source
    ".config/nvim/lua" = {
      source = "${dotfiles}/nix/home/nvim/lua";
      outOfStoreSymlink = true;
      recursive = false;
    };
  };
}
