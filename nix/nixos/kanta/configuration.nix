# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  outputs,
  config,
  pkgs,
  lib,
  ...
}: let
  privateZeroTierInterfaces = [
    "ztyqbtg66f"
  ];
  symbolsFile = pkgs.writeText "my-custom-symbols.xkb" ''
    xkb_symbols "hypers" {
        include "us(basic)"
        include "level3(ralt_switch)"

        key  <TAB> { [ Hyper_L, Hyper_L ] };
        key <BKSL> { [ Hyper_R, Hyper_R ] };
        modifier_map Mod4 { Super_L, Super_R, Hyper_L, Hyper_R };
    };
  '';
in {
  imports = [
    # my modules that the flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # NIX / NIXOS
  nix.settings.auto-optimise-store = true;
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
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

  # GC
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };

  nix.buildMachines = [
    {
      hostName = "deni@melisandre";
      system = "x86_64-linux"; # add cross building support

      # this is not required so we're leaving it null
      # since /root/.ssh/config will take care to do the right thing
      # sshKey = "/home/deni/.ssh/nixbuilder";

      protocol = "ssh-ng";
      maxJobs = 16;
      speedFactor = 100;
      supportedFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
    }
    {
      hostName = "deni@daenerys";
      system = "x86_64-linux"; # add cross building support

      # this is not required so we're leaving it null
      # since /root/.ssh/config will take care to do the right thing
      # sshKey = "/home/deni/.ssh/nixbuilder";

      protocol = "ssh-ng";
      maxJobs = 12;
      speedFactor = 90;
      supportedFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
    }
  ];

  nix.distributedBuilds = true;
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  system.activationScripts.ldso = lib.stringAfter ["usrbinenv"] ''
    mkdir -m 0755 -p /lib64
    ln -sfn ${pkgs.glibc.out}/lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2.tmp
    mv -f /lib64/ld-linux-x86-64.so.2.tmp /lib64/ld-linux-x86-64.so.2 # atomically replace
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  # BOOT
  boot.kernelParams = ["consoleblank=90" "mem_sleep_default=deep"];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # ZFS
  boot.initrd.supportedFilesystems = ["zfs"];
  boot.supportedFilesystems = ["zfs"];
  services.zfs.autoScrub.enable = true;
  services.zfs.trim.enable = true;

  # HARDWARE
  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      libva-vdpau-driver
      libvdpau-va-gl
      libva
    ];
  };
  hardware.sane.enable = true;
  # deprecated
  # hardware.video.hidpi.enable = true;

  # set trackpoint speed and sensitivity
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    speed = 255;
    sensitivity = 255;
    device = "TPPS/2 Elan TrackPoint";
  };

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
  services.libinput.touchpad.disableWhileTyping = true;
  services.libinput.enable = true;
  services.xserver.synaptics.enable = false;
  services.xserver.config = ''
    Section "InputClass"
      Identifier     "Enable libinput for TrackPoint"
      MatchIsPointer "on"
      Driver         "libinput"
    EndSection
  '';
  # services.xserver.libinput.touchpad.naturalScrolling = true;
  # services.xserver.synaptics.twoFingerScroll = true;

  # keymap
  services.xserver.xkb = {
    layout = "us,hr";
    options = "ctrl:nocaps,compose:ralt";
    # hybrid modifiers
    extraLayouts = {
      us-custom = {
        description = "My custom hybrid modifier layout";
        symbolsFile = symbolsFile;
        # ISO 639-2 codes
        # just "en" is ISO 639-1 apparently
        languages = ["eng"];
      };
    };
  };

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

  # tailscale
  services.tailscale = {
    enable = true;
  };

  services.pcscd.enable = true;

  # VM
  virtualisation.docker.autoPrune.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";
  virtualisation.libvirtd.enable = true;

  services.systembus-notify.enable = true;

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
  programs.zsh = {
    enable = true;
    # home-manager handles this so it's disabled here
    enableGlobalCompInit = false;
  };
  users.extraUsers.deni.shell = pkgs.zsh;
  nix.settings.trusted-users = ["root" "deni"];

  security.sudo.extraRules = [
    {
      users = ["deni"];
      commands = [
        {
          command = "ALL";
          options = ["NOPASSWD"];
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
      font-awesome_5
      corefonts # Microsoft free fonts
      dejavu_fonts
      fira
      fira-mono
      line-awesome
      google-fonts
      inconsolata # monospaced
      libertine
      mononoki
      nerd-fonts.ubuntu
      nerd-fonts.ubuntu-sans
      nerd-fonts.ubuntu-mono
      nerd-fonts.terminess-ttf
      nerd-fonts.symbols-only
      nerd-fonts.roboto-mono
      nerd-fonts.monoid
      nerd-fonts.monaspace
      nerd-fonts.liberation
      nerd-fonts.iosevka-term
      nerd-fonts.iosevka
      nerd-fonts.jetbrains-mono
      nerd-fonts.inconsolata
      nerd-fonts.hack
      nerd-fonts.fira-mono
      nerd-fonts.droid-sans-mono
      nerd-fonts.dejavu-sans-mono
      open-dyslexic
      overpass
      oxygenfonts
      powerline-fonts
      source-code-pro
      source-sans-pro
      source-serif-pro
      ttf_bitstream_vera
      ubuntu-classic # ubuntu-classic
      unifont # some international languages
      jetbrains-mono
      hack-font
      iosevka
      iosevka-comfy.comfy
    ];
    fontconfig = {
      antialias = true;
      cache32Bit = true;
      hinting.enable = true;
      hinting.style = "slight";
      hinting.autohint = true;
      defaultFonts = {
        monospace = ["Source Code Pro"];
        sansSerif = ["Source Sans Pro"];
        serif = ["Source Serif Pro"];
      };
    };
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # XMONAD
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    # extraPackages = haskellPackages: [
    #   pkgs.unstable.haskellPackages.xmobar
    # ];
  };

  services.xserver.displayManager.sessionCommands = ''
    xsetroot -cursor_name left_ptr &
    ${pkgs.xorg.xinput}/bin/xinput disable 'Synaptics TM3471-010'
    eval $(gnome-keyring-daemon --start) &
    ${pkgs.feh}/bin/feh --bg-scale /home/deni/walls/hack5.png &
    #synclient TouchpadOff=1 &
  '';

  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    fade = false;
    settings = {
      unredir-if-possible = false; # let full-screen apps bypass the compositor
      use-damage = true;
    };
  };

  programs.hyprland = {
    enable = true;
  };

  services.blueman.enable = true;

  services.pipewire.extraConfig.pipewire = {
    "99-disable-bell" = {
      "context.properties" = {
        "module.x11.bell" = false;
      };
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [pkgs.hplipWithPlugin];
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

  security.polkit = {
    enable = true;
  };

  security.pam.services.xscreensaver.enable = true;

  programs._1password = {
    enable = true;
  };

  programs._1password-gui = {
    enable = true;
    polkitPolicyOwners = ["deni"];
  };

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
    kitty
    virt-manager

    # these are required by the dropbox one way or another
    # it's mostly warnings but the thing barely works so better
    # safe than sorry
    python3Packages.gpgme
    python3Packages.pygobject3
    python3Packages.gst-python
  ];

  environment.localBinInPath = true;

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    # Add any missing dynamic libraries for unpackaged programs
    # here, NOT in environment.systemPackages
    gmp
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

  services.power-profiles-daemon.enable = false;
  services.thermald.enable = true;
  services.tlp = {
    enable = true;
    settings = {
      # Platform
      PLATFORM_PROFILE_ON_BAT = "low-power";
      PLATFORM_PROFILE_ON_AC = "perfomance";

      # Processor
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 60;
      CPU_BOOST_ON_BAT = 0;
      CPU_BOOST_ON_AC = 1;
      CPU_HWP_DYN_BOOST_ON_BAT = 0;
      CPU_HWP_DYN_BOOST_ON_AC = 1;

      START_CHARGE_THRESH_BAT0 = 40;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };

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
  networking.firewall.allowedTCPPorts = [17500];
  networking.firewall.allowedUDPPorts = [17500];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # allow video group to change brightness without sudo
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  services.zrepl = {
    enable = true;
    settings = {
      jobs = [
        {
          # This job pushes to the remote sink defined in job `remote_sink` on the homelab server.
          name = "kanta_home_backup";
          type = "push";
          connect = {
            # NOTE: we're sending encrypted datasets over the local network so plain 'tcp' transport
            # should be fine
            # TODO: consider switching to the 'tls' transport and manager the CA and certs with terraform
            type = "tcp";
            address = "192.168.1.54:8888";
          };
          filesystems = {
            "laptop/user/home" = true;
          };
          send = {
            encrypted = true;
          };
          # create snapshots with prefix `zrepl_` every 15 minutes
          snapshotting = {
            type = "periodic";
            interval = "15m";
            prefix = "zrepl_";
          };
          pruning = {
            keep_sender = [
              # fade-out scheme for snapshots starting with `zrepl_`
              # - keep all created in the last hour
              # - then destroy snapshots such that we keep 24 each 1 hour apart
              # - then destroy snapshots such that we keep 7 each 1 day apart
              # - then destroy all older snapshots
              {
                type = "grid";
                grid = "1x1h(keep=all) | 24x1h | 7x1d";
                regex = "^zrepl_.*";
              }
              {
                # IMPORTANT: keep all snapshots that don't have the `zrepl_` prefix
                type = "regex";
                negate = true;
                regex = "^zrepl_.*;";
              }
            ];
            # longer retention on the backup drive, we have more space there
            keep_receiver = [
              {
                type = "grid";
                grid = "1x1h(keep=all) | 24x1h | 360x1d";
                regex = "^zrepl_.*";
              }
              {
                # IMPORTANT: retain all non-zrepl snapshots on the backup drive
                type = "regex";
                negate = true;
                regex = "^zrepl_.*";
              }
            ];
          };
        }
      ];
    };
  };
}
