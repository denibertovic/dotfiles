# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'
pkgs: {
  audioctl = pkgs.callPackage ./audioctl/default.nix {};
  browsers = pkgs.callPackage ./browsers/default.nix {};
  claude-code = pkgs.callPackage ./claude-code/default.nix {};
  context7-mcp = pkgs.callPackage ./context7/default.nix {};
  denv = pkgs.callPackage ./denv.nix {};
  dictate = pkgs.callPackage ./dictate/default.nix {};
  displayctl = pkgs.callPackage ./displayctl/default.nix {};
  # Needs fastmcp >= 3.4.7, which stable nixpkgs does not have yet.
  # fastmcp 3.x does not work with mcp 2.x, which nixpkgs marks as broken, so
  # use the mcp 1.x that nixpkgs keeps for this purpose.
  google-workspace-mcp = pkgs.unstable.callPackage ./google-workspace-mcp/default.nix {
    python3Packages = pkgs.unstable.python3Packages.overrideScope (
      final: _prev: {mcp = final.mcp_1;}
    );
  };
  rumno = pkgs.callPackage ./rumno/default.nix {};
  dropbox = pkgs.callPackage ./dropbox/default.nix {};
}
