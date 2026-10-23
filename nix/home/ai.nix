{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  home.packages = [
    pkgs.claude-code
    pkgs.dictate
    pkgs.unstable.codex
    pkgs.unstable.pi-coding-agent
    pkgs.unstable.terraform-mcp-server
    pkgs.unstable.playwright-mcp
    pkgs.context7-mcp
    pkgs.google-workspace-mcp
    pkgs.unstable.mcp-grafana
    inputs.opencode.packages.${pkgs.stdenv.hostPlatform.system}.opencode
    inputs.hunk.packages.${pkgs.stdenv.hostPlatform.system}.hunk
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
