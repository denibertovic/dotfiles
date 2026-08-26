{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  services.tailscale-systray.enable = true;
}
