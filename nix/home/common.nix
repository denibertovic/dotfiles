{ config, pkgs, lib, ... }:
let denv = pkgs.callPackage ../pkgs/denv.nix {};
    browsers = pkgs.callPackage ../pkgs/browsers.nix {};
    mc-onedark-src = pkgs.fetchFromGitHub {
        owner = "DeadNews";
        repo = "mc-onedark";
        rev = "07e8704d38fe792c285c8c23c837b6e1872a7015";
        sha256 = "sha256-evkNFgShxbVaoihzWkSYmHHOKdA9GF643uPNR9y6uWY=";
      };
in
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

  home.sessionPath = [
    "/home/deni/.local/bin"
  ];

  home.packages = [
    denv
    browsers
    pkgs.llm
    pkgs.devenv
    pkgs.qutebrowser
    pkgs.acpi
    pkgs.jetbrains-mono
    pkgs.rxvt-unicode
    pkgs.libnotify
    pkgs.mc
    pkgs.direnv

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
    pkgs.yubioath-flutter
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
    pkgs.xclip
    pkgs.dig
    # use services.redshift
    # pkgs.redshift

    pkgs.jq
    pkgs.terraform
    pkgs.awscli2
    pkgs.kubernetes-helm
    pkgs.kubernetes-helmPlugins.helm-diff
    pkgs.helmfile
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
    pkgs.slack
  ];

  home.sessionVariables = {
    DEFAULT_BROWSER = "${browsers}/bin/browsers";
  };

  xdg.mimeApps.enable = true;

  xdg.desktopEntries = {
   browsers = {
      name = "Browsers";
      genericName = "Web Browser Chooser";
      exec = "${browsers}/bin/browsers %u";
      terminal = false;
      categories = [ "Application" "Network" "WebBrowser" ];
      mimeType = [ "text/html" "text/xml" "application/xhtml+xml" "application/xml" "application/vnd.mozilla.xul+xml" "application/rss+xml" "application/rdf+xml" "x-scheme-handler/http" "x-scheme-handler/https" ];
    };
  };

  xdg.mimeApps.defaultApplications = {
    "text/html" = "browsers.desktop";
    "x-scheme-handler/http" = "browsers.desktop";
    "x-scheme-handler/https" = "browsers.desktop";
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".config/mc" = {
    source = "${mc-onedark-src}/config";
    recursive = true;
  };

  home.file.".local/share/mc/skins" = {
    source = "${mc-onedark-src}/skins";
    recursive = true;
  };
}
