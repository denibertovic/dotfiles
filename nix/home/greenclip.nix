{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    pkgs.haskellPackages.greenclip
  ];
  home.file.".config/greenclip.toml" = {
    source = "/home/deni/dotfiles/greenclip.toml";
    target = ".config/greenclip.toml";
  };

  systemd.user.services = {
    greenclip-daemon = {
      Unit = {
        Description = "Clipboard manager to use with rofi - Image support and blacklist";
        Documentation = ["https://github.com/erebe/greenclip"];
        After = ["display-manager.service"];
      };

      Service = {
        Type = "simple";
        #Environment = [ "DISPLAY=:0" "XAUTHORITY=%h/.Xauthority" ];
        ExecPreStart = "${pkgs.coreutils}/bin/sleep 5";
        ExecStart = ''${pkgs.haskellPackages.greenclip}/bin/greenclip daemon'';
        Restart = "always";
      };

      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };
}
