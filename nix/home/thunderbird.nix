{ config, pkgs, lib, ... }:
{
  programs.thunderbird = {
    enable = true;
    profiles.default = {
      isDefault = true;
    };

  };
}
