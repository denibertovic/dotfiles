{ config, pkgs, lib, ... }:
{
  programs.git = {
    enable = true;
    userEmail = "deni@denibertovic.com";
    userName = "Deni Bertovic";

    aliases = {
      ff = "pull --ff-only";
      a = "add";
      ap = "add -p";
      au = "add -u";
      st = "status";
      ci = "commit -S";
      co = "checkout";
      cp = "cherry-pick";
      br = "branch";
      df = "diff";
      lg = "log -p --show-signature";
      diffc = "diff --cached";
      hist = ''log --pretty=format:"%C(auto) %h %ad | %s%d [%an]" --graph --date=short'';
      unstage = "restore --staged --";
    };

    ignores = [
      "*.swp"
      ".ghcmodcache/"
    ];

    extraConfig = {
      core = {
        editor = "vim";
      };

      color = {
        ui = true;
      };

      github = {
        user = "denibertovic";
      };

      init = {
        defaultBranch = "main";
      };

      merge = {
        keepBackup = false;
        tool = "p4merge";
      };

      "mergetool \"p4merge\"" = {
         cmd = ''extmerge "$BASE" "$LOCAL" "$REMOTE" "$MERGED"'';
         keepTemporaries = false;
         trustExitCode = false;
         keepBackup = false;
      };
    };
    signing = {
      key = "0x9E5A03FE728A9E5F";
      gpgPath = "${pkgs.gnupg}/bin/gpg2";
      signByDefault = true;
    };
  };
}
