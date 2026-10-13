# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'
pkgs: {
  browsers = pkgs.callPackage ./browsers/default.nix {};
  claude-code = pkgs.callPackage ./claude-code/default.nix {};
  context7-mcp = pkgs.callPackage ./context7/default.nix {};
  denv = pkgs.callPackage ./denv.nix {};
  displayctl = pkgs.callPackage ./displayctl/default.nix {};
  # Needs fastmcp >= 3.4.7, which stable nixpkgs does not have yet.
  google-workspace-mcp = pkgs.unstable.callPackage ./google-workspace-mcp/default.nix {};
  rumno = pkgs.callPackage ./rumno/default.nix {};
  dropbox = pkgs.callPackage ./dropbox/default.nix {};
}
