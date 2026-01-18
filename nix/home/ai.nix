{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    pkgs.unstable.claude-code
    pkgs.unstable.terraform-mcp-server
    pkgs.unstable.playwright-mcp
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
