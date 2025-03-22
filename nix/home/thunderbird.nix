{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.thunderbird = {
    enable = true;
    profiles.default = {
      isDefault = true;
      settings = {
        "layout.css.devPixelsPerPx" = "0.7";
      };
    };
  };
}
