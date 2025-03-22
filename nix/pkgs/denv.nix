{
  pkgs,
  lib,
  ...
}: let
  version = "0.7.0.1";

  sources = let
    base = "https://github.com/denibertovic/denv/releases/download/${version}/denv-${version}";
  in {
    x86_64-linux = pkgs.fetchurl {
      url = "${base}-linux-x86_64-static.tar.gz";
      sha256 = "1fk5gjr1pirq4wjc5qygnwp0giq07p40vp6kz5miwacp46ag7zjn";
    };
    x86_64-darwin = pkgs.fetchurl {
      url = "${base}-osx-x86_64.tar.gz";
      sha256 = "3ba38bd2bc19c68b7a385349def4c69bd3affa94147eee43306f790075b794c3";
    };
  };
in
  pkgs.stdenv.mkDerivation rec {
    inherit version;
    name = "denv-${version}";
    buildInputs = [pkgs.unzip];

    src = sources.${pkgs.stdenv.hostPlatform.system} or (throw "denv doesn't support system: ${pkgs.stdenv.hostPlatform.system}");

    unpackPhase = ''
      tar zxvf $src
    '';

    installPhase = ''
      mkdir -p $out/bin
      install -D -m555 -T denv $out/bin/denv
    '';
  }
