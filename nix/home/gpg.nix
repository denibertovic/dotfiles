{ config, pkgs, lib, ... }:
let mykey = "9C4B20E3BAF44208F2FA0A3F9E5A03FE728A9E5F";
in
{
  programs.gpg = {
    enable = true;
    mutableKeys = true;
    mutableTrust = true;
    publicKeys = [
      { source = "/home/deni/dotfiles/publickeys/${mykey}.asc"; trust = 5; }
    ];
    scdaemonSettings = {
      debug-ccid-driver = "";
      debug = "2048";
      log-file = "/home/deni/.gnupg/scdaemon.log";
    };
    settings = {
      default-key = mykey;
      keyserver = [ "hkp://keys.gnupg.net" ];
      keyid-format = "0xlong";
    };
  };
}
