{ config, pkgs, lib, ... }:
{
  services.dropbox = {
    enable = true;
  };
}
