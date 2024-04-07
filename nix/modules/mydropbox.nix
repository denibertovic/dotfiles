{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.mydropbox;
  baseDir = ".dropbox-hm";
  dropboxCmd = "${pkgs.dropbox-cli}/bin/dropbox";

in {
  meta.maintainers = [ maintainers.eyjhb ];

  options = {
    services.mydropbox = {
      enable = mkEnableOption "Dropbox daemon";

      path = mkOption {
        type = types.path;
        default = "${config.home.homeDirectory}/Dropbox";
        defaultText =
          literalExpression ''"''${config.home.homeDirectory}/Dropbox"'';
        apply = toString; # Prevent copies to Nix store.
        description = "Where to put the Dropbox directory.";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "services.mydropbox" pkgs
        lib.platforms.linux)
    ];

    home.packages = [ pkgs.dropbox-cli ];

    systemd.user.services.dropbox = {
      Unit = { Description = "dropbox"; };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        Environment = [
        ];

        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Nice = 10;

        ExecStart = "${lib.getBin pkgs.dropbox}/bin/dropbox" ;
        ExecReload = "${lib.getBin pkgs.coreutils}/bin/kill -HUP $MAINPID";
        ExecStop = "${dropboxCmd} stop";
        KillMode = "control-group";

      };
    };
  };
}
