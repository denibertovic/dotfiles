{ config, pkgs, lib, ... }:
{
  home.packages = [
#    pkgs.haskellPackages.stack2nix
    pkgs.haskellPackages.hoogle
    pkgs.haskellPackages.hscolour
    pkgs.haskellPackages.ghc-mod
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.hdevtools
    pkgs.haskellPackages.hfmt
  ];
}
