# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  privateZeroTierInterfaces = [
    "ztyqbtg66f"
  ];
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # NIX / NIXOS
  nix.settings.auto-optimise-store = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.config.packageOverrides = pkgs: {
    # vaapiIntel = pkgs.vaapiIntel.override { enabledHybridCodec = true; };
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/1f80e16537599cff4c125eb306b0af827818e97c.tar.gz";
      sha256 = "1l28ds47xzn5aw8k6hg7j8arfq8pv22vpg6vy830ddwxa42jwwfv";
     }) {
      inherit pkgs;
    };
  };

  system.activationScripts.ldso = lib.stringAfter [ "usrbinenv"  ] ''
    mkdir -m 0755 -p /lib64
    ln -sfn ${pkgs.glibc.out}/lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2.tmp
    mv -f /lib64/ld-linux-x86-64.so.2.tmp /lib64/ld-linux-x86-64.so.2 # atomically replace
    '';

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.pulseaudio = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  # BOOT
  boot.kernelParams = [ "consoleblank=90" "mem_sleep_default=deep" ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # ZFS
  boot.initrd.supportedFilesystems = [ "zfs" ];
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.enableUnstable = true;
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true;
  services.zfs.autoSnapshot.frequent = 8;
  services.zfs.autoSnapshot.monthly = 1;
  services.zfs.trim.enable = true;

  # HARDWARE
  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
  hardware.pulseaudio.enable = true;
  # hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.zeroconf.discovery.enable = true;
  hardware.pulseaudio.zeroconf.publish.enable = true;
  hardware.sane.enable = true;
  # deprecated
  # hardware.video.hidpi.enable = true;
  sound.enable = true;

  # NETWORKING
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.hostId = "96b8f8ce"; # cut -c-8 </proc/sys/kernel/random/uuid
  networking.hostName = "kanta";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.firewall.trustedInterfaces = privateZeroTierInterfaces;
  networking.nameservers = [
    "1.1.1.1"
    "1.0.0.1"
  ];
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = true;

  # LOCALE
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Zagreb";

   # INPUT
  console.useXkbConfig = true;
  services.xserver.layout = "us";
  services.xserver.libinput.touchpad.disableWhileTyping = true;
  services.xserver.libinput.enable = true;
  services.xserver.synaptics.enable = false;
  services.xserver.config =  ''
      Section "InputClass"
        Identifier     "Enable libinput for TrackPoint"
        MatchIsPointer "on"
        Driver         "libinput"
      EndSection
    '';
  # services.xserver.libinput.touchpad.naturalScrolling = true;
  # services.xserver.synaptics.twoFingerScroll = true;
  services.xserver.xkbOptions = "ctrl:nocaps";
  # services.xserver.xkbVariant = "dvorak";

  # GPG
  programs.gnupg.agent.enable = true;

  # SSH
  services.openssh.enable = true;
  services.openssh.openFirewall = true;
  services.openssh.settings.PasswordAuthentication = false;
  services.openssh.settings.PermitRootLogin = "no";

  # KEYBASE
  services.kbfs.enable = true;

  # ZEROTIER
  services.zerotierone.enable = true;
  services.zerotierone.joinNetworks = [
    "8bd5124fd6213413" # home
  ];

  services.pcscd.enable = true;

  # VM
  virtualisation.docker.autoPrune.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";
  virtualisation.libvirtd.enable = true;

  # USERS
  users.extraUsers.deni.description = "Deni Bertovic";
  users.extraUsers.deni.isNormalUser = true;
  users.extraUsers.deni.extraGroups = [
    "adbusers"
    "audio"
    "dialout"
    "docker"
    "kvm"
    "libvirtd"
    "lp"
    "networkmanager"
    "power"
    "scanner"
    "video"
    "wheel"
    "smartmontools"
  ];
  programs.zsh.enable = true;
  users.extraUsers.deni.shell = pkgs.zsh;
  nix.settings.trusted-users = [ "root" "deni" ];

  security.sudo.extraRules= [
    {  users = [ "deni" ];
      commands = [
         { command = "ALL" ;
           options= [ "NOPASSWD" ];
        }
      ];
    }
  ];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      #font-awesome_4
      corefonts # Microsoft free fonts
      dejavu_fonts
      fira
      fira-mono
      line-awesome
      google-fonts
      inconsolata # monospaced
      libertine
      mononoki
      nerdfonts
      open-dyslexic
      overpass
      oxygenfonts
      powerline-fonts
      source-code-pro
      source-sans-pro
      source-serif-pro
      ttf_bitstream_vera
      ubuntu_font_family # Ubuntu fonts
      unifont # some international languages
      jetbrains-mono
      symbola
    ];
    fontconfig = {
      antialias = true;
      cache32Bit = true;
      hinting.enable = true;
      hinting.style = "slight";
      hinting.autohint = true;
      defaultFonts = {
        monospace = [ "Source Code Pro" ];
        sansSerif = [ "Source Sans Pro" ];
        serif = [ "Source Serif Pro" ];
      };
    };
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;


  # XMONAD
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: with haskellPackages; [
      unstable.haskellPackages.xmobar
    ];
  };

  services.xserver.displayManager.sessionCommands = ''
    xsetroot -cursor_name left_ptr &
    picom &
    ${pkgs.xorg.xinput}/bin/xinput disable 'Synaptics TM3471-010'
    eval $(gnome-keyring-daemon --start) &
    feh --bg-scale /home/deni/walls/hack5.png &
    #synclient TouchpadOff=1 &
    xmodmap /home/deni/.Xmodmap &
  '';

  services.blueman.enable = true;

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplipWithPlugin ];
  hardware.printers = {
    ensurePrinters = [
      {
        name = "HomeOffice";
        location = "Home";
        deviceUri = "socket://192.168.1.1:9101";
        model = "drv:///hp/hpcups.drv/hp-laserjet_professional_p_1102w.ppd";
        ppdOptions = {
          PageSize = "A4";
        };
      }
    ];
    ensureDefaultPrinter = "HomeOffice";
  };

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # dropbox-cli
    pmutils
    nfs-utils
    manix

    # these are required by the dropbox one way or another
    # it's mostly warnings but the thing barely works so better
    # safe than sorry
    python3Packages.gpgme
    python3Packages.pygobject3
    python3Packages.gst-python

  ];

  # For this to work I had to log into dropbox.com in the browser first.
  # Then do "dropbox start -i" from the terminal. This installed the daemon and
  # prompted me in the browser to connect my device to my account.
  # Once that's done the daemon starts and appears in the tray and starts syncing ...
  # BUT will likely crash after a few seconds (under a minute).
  # That's when we install the user service below.
  # For now it seems to work. This is horrible.
  # systemd.user.services.dropbox = {
  #   description = "Dropbox";
  #   wantedBy = [ "graphical-session.target" ];
  #   environment = {
  #     QT_PLUGIN_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtPluginPrefix;
  #     QML2_IMPORT_PATH = "/run/current-system/sw/" + pkgs.qt5.qtbase.qtQmlPrefix;
  #   };
  #   serviceConfig = {
  #     ExecStart = "${lib.getBin pkgs.dropbox}/bin/dropbox";
  #     ExecReload = "${lib.getBin pkgs.coreutils}/bin/kill -HUP $MAINPID";
  #     KillMode = "control-group"; # upstream recommends process
  #     Restart = "on-failure";
  #     PrivateTmp = true;
  #     ProtectSystem = "full";
  #     Nice = 10;
  #   };
  # };

  services.power-profiles-daemon.enable = true;
  services.tlp.enable = false;

  services.tailscale.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 17500 ];
  networking.firewall.allowedUDPPorts = [ 17500 ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # allow video group to change brightness without sudo
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
}
