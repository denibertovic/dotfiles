{ config, pkgs, lib, ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      # dpi = {
      #   x = 157;
      #   y = 157;
      # };
      # window = {
      #   dynamic_padding = true;
      # };
      env = {
        WINIT_X11_SCALE_FACTOR = "1.5";
      };
      font = {
        builtin_box_drawing = false;
        size = 8.0;
        # offset = {
        #   x = 1;
        #   y = 1;
        # };
        normal = {
          family = "Hack";
          style = "Regular";
        };
        bold = {
          family = "Hack";
          style = "Bold";
        };
        italic = {
          family = "Hack";
          style = "Italic";
        };
        bold_italic = {
          family = "Hack";
          style = "Bold Italic";
        };
      };
      cursor = {
        style = {
          shape = "Block";
          blinking = "Always";
        };
        blink_interval = 500;
        blink_timeout = 0;
        unfocused_hollow = true;
        thickness = 0.10;
      };
      # mouse = {
      # };
      keyboard = {
        bindings = [
          { key = "Space"; mods = "Control"; mode = "Vi"; action = "ScrollToBottom"; }
          { key = "Space"; mods = "Control";              action = "ToggleViMode"; }
        ];
      };
    };
  };
}
