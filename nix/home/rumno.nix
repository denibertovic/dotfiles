{ config, pkgs, lib, ... }:
let myRumnopkg = pkgs.callPackage ../pkgs/rumno/default.nix {};
in
{
  home.packages = [
    myRumnopkg
  ];

  systemd.user.services = {
    rumno-daemon = {
      Unit = {
        Description = "Rumno daemon";
        Documentation = [ "https://gitlab.com/natjo/rumno" ];
        After = [ "display-manager.service" ];
      };

      Service = {
        Type = "dbus";
        BusName="de.rumno.v1";
        # NOTE: The project hardcodes /tmp/rumno/rumno.pid so that's why
        # we're not using /run
        TimeoutStartSec = 15;
        ExecPreStart="${pkgs.coreutils}/bin/sleep 5";
        PIDFile="/tmp/rumno/rumno.pid";
        #Environment = [ "DISPLAY=:0" "XAUTHORITY=%h/.Xauthority" ];
        ExecStart = ''${myRumnopkg}/bin/rumno-background'';
        Restart = "always";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
