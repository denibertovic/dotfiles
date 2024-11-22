{ config, pkgs, lib, ... }:
{
  services.keybase.enable = true;
  services.kbfs.enable = true;
  # workaround: https://github.com/nix-community/home-manager/pull/5655
  systemd.user.services.kbfs.Service.PrivateTmp = lib.mkForce false;
}
