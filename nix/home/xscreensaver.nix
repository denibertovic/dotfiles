{ config, pkgs, lib, ...  }:
{
  services.xscreensaver = {
    enable = true;
    # NOTE: We just link in the hand crafted Xresources
    # settings = {
    #   fadeTicks = 20;
    #   fade = false;
    #   unfade = false;
    #   lock = true;
    #   # settings "blank" will always fadein/fadeout
    #   mode = "off";
    # };
  };
}
