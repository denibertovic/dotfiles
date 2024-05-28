{ config, pkgs, lib, ... }:
let unstable = import <nixos-unstable> { config = { allowUnfree = true;   };   };
in
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
    ./thunderbird.nix
    ./google-chrome.nix
    ./alacritty.nix
    ./dunst.nix
    ./xmonad.nix
    ./rofi.nix
    ../modules/mydropbox.nix
    ./dropbox.nix
    ./zoom.nix
    # ghc-syb-utils doesn't compile (whatever that is)
#    ./haskell.nix
  ];

  # because https://github.com/NixOS/nixpkgs/pull/277422
  # tldr buildFHSEnv needs dieWithParent = false;
  # The dropbox-cli command `dropbox start` starts the dropbox daemon in a
  # separate session, and wants the daemon to outlive the launcher.  Enabling
  # `--die-with-parent` defeats this and causes the daemon to exit when
  # dropbox-cli exits.
  # TODO: remove this with 24.05
  nixpkgs.overlays = [
    (self: super: {
        dropbox = unstable.dropbox;
        devenv = unstable.devenv;
        slack = unstable.slack;
    })
  ];
}
