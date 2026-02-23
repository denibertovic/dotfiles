{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [
    pkgs.most-unstable.claude-code
    pkgs.unstable.terraform-mcp-server
    pkgs.unstable.playwright-mcp
    pkgs.context7-mcp
    inputs.opencode.packages.${pkgs.stdenv.hostPlatform.system}.opencode
  ];

  programs.zed-editor = {
    enable = true;
    extensions = ["nix" "xy-zed"];
    package = pkgs.unstable.zed-editor;
    userSettings = {
      vim_mode = true;
    };
  };
}
