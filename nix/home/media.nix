{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    pkgs.vlc
    pkgs.mplayer
  ];
}
