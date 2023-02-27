{ config, pkgs, lib, ... }:
{
    programs.rofi = {
    enable = true;
    font = "JetBrains Mono 12";
    extraConfig = {
      modi = "window,run";
      show-match = false;
      separator-style = "solid";
      hide-scrollbar = true;
    };
    theme = "/home/deni/dotfiles/rofi/nord.rasi";
  };
}
