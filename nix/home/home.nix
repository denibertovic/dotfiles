{ config, pkgs, lib, ... }:
{
  imports = [
    ./common.nix
    ./vim.nix
    ./zsh.nix
    ./xscreensaver.nix
    ./rumno.nix
    ./greenclip.nix
    ./gpg.nix
    ./git.nix
    ./firefox.nix
    ./alacritty.nix
    ./dunst.nix
    ./xmonad.nix
    ./rofi.nix
    ./dropbox.nix
    ./chromium.nix
    # ghc-syb-utils doesn't compile (whatever that is)
#    ./haskell.nix
  ];

  home.packages = [
    pkgs.acpi
    pkgs.jetbrains-mono
    pkgs.rxvt-unicode
    pkgs.libnotify
    pkgs.mc
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
