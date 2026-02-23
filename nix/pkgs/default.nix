# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'
pkgs: {
  browsers = pkgs.callPackage ./browsers.nix {};
  context7-mcp = pkgs.callPackage ./context7/default.nix {};
  denv = pkgs.callPackage ./denv.nix {};
  rumno = pkgs.callPackage ./rumno/default.nix {};
  dropbox = pkgs.callPackage ./dropbox/default.nix {};
}
