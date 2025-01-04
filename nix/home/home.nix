{ inputs, outputs, config, pkgs, lib, ... }:
{
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    outputs.homeManagerModules.mydropbox
    inputs.nur.nixosModules.nur

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    ./common.nix
    ./zsh.nix
    ./xscreensaver.nix
    ./rumno.nix
    ./greenclip.nix
    ./gpg.nix
    ./git.nix
    ./firefox.nix
    ./thunderbird.nix
    ./google-chrome.nix
    ./alacritty.nix
    ./dunst.nix
    ./xmonad.nix
    ./rofi.nix
    ./dropbox.nix
    ./zoom.nix
    ./media.nix
    ./neovim.nix
    ./keybase.nix
    ./conky.nix
    # ghc-syb-utils doesn't compile (whatever that is)
#    ./haskell.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
    };
  };
}
