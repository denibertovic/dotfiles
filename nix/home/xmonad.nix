{ config, pkgs, lib, ... }:
let unstable = import <nixos-unstable> { config = { allowUnfree = true;  };  };
in
{
  home.packages = [
    pkgs.feh
    pkgs.gnome.gnome-tweaks
    pkgs.networkmanagerapplet
    pkgs.xorg.xmodmap
    pkgs.xorg.xmessage
    pkgs.xorg.xf86inputsynaptics
    pkgs.trayer
    pkgs.picom
    pkgs.gnome.zenity
    pkgs.dmenu
  ];

  services.blueman-applet = {
    enable = true;
  };

  xresources.extraConfig = builtins.readFile "/home/deni/dotfiles/Xresources";

  home.file.".xmonad" = {
    source = /home/deni/dotfiles/xmonad;
    recursive = true;
  };

  programs.xmobar= {
    enable = true;
    package = unstable.xmobar;
    extraConfig = builtins.readFile "/home/deni/dotfiles/xmobarrc";
  };
}
