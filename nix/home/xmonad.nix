{ config, pkgs, lib, ... }:
let unstable = import <nixos-unstable> { config = { allowUnfree = true;  };  };
in
{
  home.packages = [
    pkgs.feh
    pkgs.gnome.gnome-tweaks
    pkgs.gnome.gnome-screenshot
    pkgs.gnome.eog
    pkgs.networkmanagerapplet
    pkgs.xorg.xmodmap
    pkgs.xorg.xmessage
    pkgs.xorg.xf86inputsynaptics
    pkgs.trayer
    pkgs.picom
    pkgs.gnome.zenity
    pkgs.dmenu
  ];

  services.trayer = {
    enable = true;
    settings = {
      edge = "top";
      align = "right";
      SetDockType = true;
      SetPartialStrut = true;
      expand = true;
      width = 20;
      transparent = true;
      alpha = 0;
      tint = "0x222222";
      height = 25;
    };
  };

  home.pointerCursor = {
    x11.enable = true;
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
    size = 24;
    gtk.enable = true;
  };

  # workaround for blueman-applet. see here: https://github.com/nix-community/home-manager/issues/2064#issuecomment-887300055
  systemd.user.targets.tray = {
    Unit = {
      Description = "Home Manager System Tray";
      Requires = [ "graphical-session-pre.target" ];
    };
  };

  services.blueman-applet = {
    enable = true;
  };

  services.network-manager-applet.enable = true;

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

  services.cbatticon = {
    enable = true;
    batteryId = "BAT0";
    commandCriticalLevel = "notify-send -u CRITICAL 'Battery Critical' 'Battery is critical - please connect charger now!'";
    criticalLevelPercent = 10;
    iconType = "symbolic";
    lowLevelPercent = 20;
    updateIntervalSeconds = 5;
  };
}
