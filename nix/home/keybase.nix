{ config, pkgs, lib, ... }:
{
  services.keybase.enable = true;
  services.kbfs.enable = true;
}
