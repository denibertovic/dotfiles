{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.firefox = {
    enable = true;
    # keep the pre-26.05 profile location (~/.mozilla/firefox); the new default
    # moves it under $XDG_CONFIG_HOME which would require migrating the dir.
    configPath = ".mozilla/firefox";
    # this also needs services.gnome.gnome-browser-connector.enable = true
    enableGnomeExtensions = false;
    profiles.default = {
      # this is the bare minimum to get me going
      # the rest can be synced from firefox sync
      extensions = {
        packages = with pkgs.nur.repos.rycee.firefox-addons; [
          vimium
          tree-style-tab
        ];
      };
      isDefault = true;
      settings = {
        "browser.startup.homepage" = "about:blank";
        "browser.bookmars.showMobileBookmarks" = true;
        "layout.css.devPixelsPerPx" = "0.7";
        "browser.download.useDownloadDir" = false;
        # !!!This is needed for userChrome.css to work!!!
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        # VA-API video decode (iHD driver); lowers CPU load on video calls
        "media.ffmpeg.vaapi.enabled" = true;
        "media.hardware-video-decoding.force-enabled" = true;
      };
      userChrome = builtins.readFile "/home/deni/dotfiles/userChrome.css";
    };
  };
}
