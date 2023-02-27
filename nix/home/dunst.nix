{ config, pkgs, lib, ... }:
{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "JetBrains Mono 10";
        allow_markup = "yes";
        format = "<b>%s</b>\n%b";
        sort = "yes";
        indicate_hidden = "yes";
        alignment = "left";
        bounce_freq = 0;
        show_age_threshold = 120;
        word_wrap = "yes";
        ignore_newline = "no";
        geometry = "1000x6-30+50";
        shrink = "no";
        transparency = 0;
        idle_threshold = 120;
        monitor = 0;
        follow = "mouse";
        sticky_history = "yes";
        history_length = 100;
        show_indicators = "yes";
        line_height = 0;
        separator_height = 2;
        padding = 32;
        horizontal_padding = 32;
        separator_color = "frame";
        startup_notification = false;
        browser = "firefox -new-tab";
        icon_position = "off";
        # icon_folders =
      };
      frame = {
        width = 3;
        # color = "";
        color = "#4C566A";
      };

      urgency_low = {
        background = "#3b4252";
        foreground = "#eceff4";
        frame_color = "#4c566a";
        timeout = 10;
      };

      urgency_normal = {
        background = "#3b4252";
        foreground = "#eceff4";
        timeout = 8;
      };

      urgency_critical = {
        background = "#3b4252";
        foreground = "#eceff4";
        frame_color = "#bf616a";
        timeout = 0;
      };

      DND = {
        summary = "*Do Not Disturb*";
        set_stack_tag = "dnd";
      };
    };
  };
}
