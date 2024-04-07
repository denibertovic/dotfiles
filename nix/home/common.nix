{ config, pkgs, lib, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "deni";
  home.homeDirectory = "/home/deni";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = [
    pkgs.acpi
    pkgs.jetbrains-mono
    pkgs.rxvt-unicode
    pkgs.libnotify
    pkgs.mc

    pkgs.htop
    pkgs.ack
    pkgs.wget
    pkgs.curl
    pkgs.rsync
    pkgs.tmate
    pkgs.ovh-ttyrec
    pkgs.unrtf
    pkgs.units
    pkgs.vagrant
    pkgs.virtualboxWithExtpack
    # NOTE: needs configuring
    pkgs.weechat
    # pkgs.weechat-unwrapped.perl
    # pkgs.weechat-unwrapped.python
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
    pkgs.yubikey-personalization-gui
    pkgs.wireshark
    pkgs._1password-gui
    # 310 was needed because otherwise vim wouldn't find neovim/pynvim
    (pkgs.python310.withPackages (ps: with ps; [
      pip
      virtualenv
      pynvim
      virtualenvwrapper
      ipython
      subliminal
    ]))

    pkgs.borgbackup
    pkgs.hddtemp
    pkgs.dos2unix
    pkgs.hplip
    pkgs.iftop
    pkgs.inotify-tools
    pkgs.keepassxc
    pkgs.gnumake
    pkgs.pandoc
    pkgs.pass
    pkgs.xorg.xev
    pkgs.lm_sensors
    pkgs.pwgen
    pkgs.tarsnap
    pkgs.dex
    # use services.redshift
    # pkgs.redshift

    pkgs.jq
    pkgs.terraform
    pkgs.awscli2
    pkgs.kubernetes-helm
    pkgs.kubectl
    pkgs.sops
    pkgs.kops
    pkgs.kail
    pkgs.packer
    pkgs.vault
    pkgs.vaultenv

    pkgs.cabal-install
    pkgs.stack

    pkgs.hledger
    pkgs.hledger-ui
    pkgs.hledger-web

    pkgs.signal-desktop
    pkgs.chromium
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
}
