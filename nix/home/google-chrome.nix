{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    pkgs.google-chrome
  ];
}
