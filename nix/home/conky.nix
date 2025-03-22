{
  config,
  pkgs,
  lib,
  ...
}: let
  utils = import ./utils.nix {inherit config pkgs lib;};
  dotfiles = "${config.home.homeDirectory}/dotfiles";
in {
  home.file = utils.linkHomeFiles {
    # set outOfStoreSymlink = true and recursive = true to recursively link all files within source
    ".config/conky/conky.lua" = {
      source = "${dotfiles}/nix/home/conky/conky.lua";
      outOfStoreSymlink = true;
      recursive = false;
    };
  };

  services.conky = {
    enable = false;
    extraConfig = builtins.readFile ./conky/conky.lua;
  };
}
