{pkgs, ...}: let
  # autorandr --fingerprint with the dock attached
  edid = {
    laptop = "00ffffffffffff000e6f32140000000000210104a51e13780371ae955d5b912920515600000001010101010101010101010101010101ea3d80c870b02e40302036002dbc1000001a000000fd00283c4b4b10010a202020202020000000fe0043534f542054390a2020202020000000fe004d4e453030375153332d330a200003";
    dell23 = "00ffffffffffff0010ac73404c3231462b150104a5331d783b6ea5a3544f9f26115054a54b00714f8180d1c001010101010101010101023a801871382d40582c4500fe1f1100001e000000ff003539444a503141514631324c0a000000fc0044454c4c205532333132484d0a000000fd00384c1e5311000a20202020202000c4";
    dell27 = "00ffffffffffff0010ac66d053363730121a0104a53c22783e4455a9554d9d260f5054a54b00b300d100714fa9408180778001010101565e00a0a0a029503020350055502100001a000000ff004748383544363533303736530a000000fc0044454c4c205532373135480a20000000fd0038561e711e010a20202020202001a702031cf14f1005040302071601141f12132021222309070783010000023a801871382d40582c450055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e9600555021000018483f00ca808030401a50130055502100001e000000000000000000fe";
  };
in {
  home.packages = [pkgs.displayctl];

  programs.autorandr = {
    enable = true;

    hooks.postswitch = {
      # trayer sits on the primary monitor and does not follow it when
      # the primary changes
      "restart-trayer" = "systemctl --user restart trayer.service";
      "wallpaper" = "${pkgs.feh}/bin/feh --bg-scale /home/deni/walls/hack5.png";
    };

    # Output names are placeholders: displayctl runs autorandr with
    # --match-edid, which renames them to whatever the dock exposes.
    profiles = {
      laptop = {
        fingerprint.eDP-1 = edid.laptop;
        config.eDP-1 = {
          primary = true;
          position = "0x0";
          mode = "1920x1200";
        };
      };

      work = {
        fingerprint = {
          eDP-1 = edid.laptop;
          DP-1-2 = edid.dell23;
          DP-1-3 = edid.dell27;
        };
        config = {
          DP-1-2 = {
            position = "0x0";
            mode = "1920x1080";
            rotate = "left";
          };
          DP-1-3 = {
            primary = true;
            position = "1080x0";
            mode = "2560x1440";
          };
          # bottom aligned with the Dell 27, the panel sits lower on the desk
          eDP-1 = {
            position = "3640x240";
            mode = "1920x1200";
          };
        };
      };
    };
  };
}
