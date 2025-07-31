{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.ghostty = {
    enable = true;
    # this is uses the flake (see overlays)
    package = pkgs.ghostty;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
      # Monokai Soda
      # nord-wave
      # NvimDark
      # Raycast_Dark
      # tokyonight_night
      # catppuccin-mocha
      # Tomorrow Night
      theme = "Tomorrow Night";
      window-step-resize = true;
      resize-overlay = "never";
      background-blur = "false";
      # cursor-style-blink = true;
      font-size = 8;
      font-family = "Hack";
      # no need to set the specific ones
      # as they are automatically selected
      # font-family-bold = "Hack Bold";
      # font-family-italic = "Hack Italic";
      # font-family-bold-italic = "Hack Bold Italic";
      # handled by xmonad
      # background-opacity = 1;
      window-decoration = "none";
      keybind = [
        "ctrl+l=clear_screen"
      ];
    };
  };
}
