{
  lib,
  stdenv,
  fetchurl,
  patchelf,
  makeWrapper,
  bubblewrap,
  procps,
  socat,
}: let
  platformMap = {
    "x86_64-linux" = "linux-x64";
    "aarch64-linux" = "linux-arm64";
  };
  platform = platformMap.${stdenv.hostPlatform.system} or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
  stdenv.mkDerivation rec {
    pname = "claude-code";
    version = "2.1.142";

    src = fetchurl {
      url = "https://registry.npmjs.org/@anthropic-ai/claude-code-${platform}/-/claude-code-${platform}-${version}.tgz";
      hash = "sha256-TQn2N35LxRRXmotcVps2xxPzkEyJQEkX1YJbrXOVWZ4=";
    };

    sourceRoot = "package";

    nativeBuildInputs = [ patchelf makeWrapper ];

    dontPatchELF = true;
    dontStrip = true;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      cp claude $out/bin/claude
      patchelf --set-interpreter "${stdenv.cc.bintools.dynamicLinker}" $out/bin/claude
      wrapProgram $out/bin/claude \
        --set DISABLE_AUTOUPDATER 1 \
        --set DISABLE_INSTALLATION_CHECKS 1 \
        --unset DEV \
        --prefix PATH : ${
        lib.makeBinPath (
          [
            procps
          ]
          ++ lib.optionals stdenv.hostPlatform.isLinux [
            bubblewrap
            socat
          ]
        )
      }
      runHook postInstall
    '';

    meta = with lib; {
      description = "Agentic coding tool that lives in your terminal";
      homepage = "https://github.com/anthropics/claude-code";
      license = licenses.unfree;
      mainProgram = "claude";
    };
  }
