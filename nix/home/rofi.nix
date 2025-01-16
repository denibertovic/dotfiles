{ config, pkgs, lib, ... }:
{
    programs.rofi = {
    enable = true;
    font = "Hack 10";
    extraConfig = {
      modi = "window,run";
      show-match = false;
      separator-style = "solid";
      hide-scrollbar = true;
    };
    theme = "/home/deni/dotfiles/rofi/nord.rasi";
  };
}
