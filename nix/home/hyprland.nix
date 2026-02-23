{
  config,
  pkgs,
  lib,
  ...
}: let
  utils = import ./utils.nix {inherit config pkgs lib;};
  dotfiles = "${config.home.homeDirectory}/dotfiles";
in {
  home.packages = with pkgs; [
    # Core utilities
    jq
    libnotify  # notify-send

    # Status bar
    waybar

    # Notifications
    mako

    # Lock screen & idle
    hyprlock
    hypridle

    # Night light & color picker
    hyprsunset
    hyprpicker

    # Wallpaper
    swaybg

    # Screenshot & clipboard
    grim
    slurp
    wl-clipboard

    # Audio/brightness control
    brightnessctl
    pamixer
    pavucontrol
    playerctl

    # System applets
    blueman
    networkmanagerapplet
    polkit_gnome

    # Hyprland plugins
    hyprlandPlugins.hyprbars
  ];

  # Link Hyprland configs (out-of-store symlinks for live editing)
  home.file = utils.linkHomeFiles {
    # Hyprland
    ".config/hypr/hyprland.conf" = {
      source = "${dotfiles}/hyprland/hyprland.conf";
      outOfStoreSymlink = true;
      recursive = false;
    };
    ".config/hypr/bindings.conf" = {
      source = "${dotfiles}/hyprland/bindings.conf";
      outOfStoreSymlink = true;
      recursive = false;
    };
    ".config/hypr/scratchpads.conf" = {
      source = "${dotfiles}/hyprland/scratchpads.conf";
      outOfStoreSymlink = true;
      recursive = false;
    };
    ".config/hypr/hyprlock.conf" = {
      source = "${dotfiles}/hyprland/hyprlock.conf";
      outOfStoreSymlink = true;
      recursive = false;
    };
    ".config/hypr/hypridle.conf" = {
      source = "${dotfiles}/hyprland/hypridle.conf";
      outOfStoreSymlink = true;
      recursive = false;
    };

    # Waybar
    ".config/waybar/config.jsonc" = {
      source = "${dotfiles}/hyprland/waybar/config.jsonc";
      outOfStoreSymlink = true;
      recursive = false;
    };
    ".config/waybar/style.css" = {
      source = "${dotfiles}/hyprland/waybar/style.css";
      outOfStoreSymlink = true;
      recursive = false;
    };

    # Mako
    ".config/mako/config" = {
      source = "${dotfiles}/hyprland/mako/config";
      outOfStoreSymlink = true;
      recursive = false;
    };

    # Scripts
    ".local/bin/hypr" = {
      source = "${dotfiles}/hyprland/scripts";
      outOfStoreSymlink = true;
      recursive = true;
    };
  };

  # Screenshot annotation
  programs.satty.enable = true;

  # Volume/brightness OSD
  services.swayosd.enable = true;

  # Launcher with clipboard manager
  services.walker = {
    enable = true;
    systemd.enable = true;  # Required for clipboard history
  };
}
