{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    pkgs.unstable.code-cursor
  ];

  programs.zed-editor = {
    enable = true;
    extensions = ["nix" "xy-zed"];
    package = pkgs.unstable.zed-editor;
    userSettings = {
      vim_mode = true;
    };
  };
}
