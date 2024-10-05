{ config, pkgs, lib, ... }:
{
  # NOTES: First log into the default browser
  # Then start the service and let it download .dropbox-dist
  # Connect via browser once the tab opens
  # if the service fails just remove the .dropbox-dist folder (not the .dropbox folder)
  # and restart
  services.mydropbox = {
    enable = true;
  };
}
