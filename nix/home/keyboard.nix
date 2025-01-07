{ config, pkgs, lib, ... }:
{
  services.xcape = {
    enable = true;
    timeout = 200;
    mapExpression = {
      Hyper_L = "Tab";
      Hyper_R = "backslash";
    };
  };
}
