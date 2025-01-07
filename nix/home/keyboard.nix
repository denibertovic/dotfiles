{ config, pkgs, lib, ... }:
{
  services.xcape = {
    enable = false;
    timeout = 200;
    mapExpression = {
      Hyper_L = "Tab";
      Hyper_R = "backslash";
    };
  };
}
