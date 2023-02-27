{ config, pkgs, lib, ... }:
{
  programs.firefox = {
    enable = true;
    # this also needs services.gnome.gnome-browser-connector.enable = true
    enableGnomeExtensions = false;
    profiles.default = {
      # this is the bare minimum to get me going
      # the rest can be synced from firefox sync
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        vimium
        tree-style-tab
      ];
      isDefault = true;
      settings = {
        "browser.startup.homepage" = "about:blank";
        "browser.bookmars.showMobileBookmarks" = true;
        "layout.css.devPixelsPerPx" = "0.8";
        "browser.download.useDownloadDir" = false;
        # !!!This is needed for userChrome.css to work!!!
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      };
      userChrome = builtins.readFile "/home/deni/dotfiles/userChrome.css";
    };

  };
}
