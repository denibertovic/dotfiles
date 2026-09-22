{
  pkgs,
  lib,
  fetchFromGitHub,
  rustPlatform,
}:
rustPlatform.buildRustPackage rec {
  pname = "browsers";
  version = "0.7.5";

  src = fetchFromGitHub {
    owner = "Browsers-software";
    repo = "browsers";
    rev = version;
    hash = "sha256-mbYtoWfnDgE7UkXh9KpAtFx6YvNryMv11ntkrXclHaA=";
  };

  nativeBuildInputs = [pkgs.pkg-config];
  buildInputs = [pkgs.glib.dev pkgs.glibc.dev pkgs.cairo.dev pkgs.pango.dev pkgs.atkmm.dev pkgs.gtk3.dev];

  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    # The git dependencies point at forks whose branches get force pushed.
    # Fixed output hashes fetch by commit and keep working after that;
    # allowBuiltinFetchGit needs the commit to be reachable from a branch.
    outputHashes = {
      "druid-0.8.3" = "sha256-MF1aVgmXLcvagEw2aeYCZY80ZSAgjVH2BPlYYBuS1q8=";
      "rolling-file-0.2.0" = "sha256-3xeOSXFVVgeKRE39gtzTURt0OkKScQ4uwtvLl4CE3R4=";
    };
  };

  # cargoHash = "";

  meta = {
    description = "A really simple command line utility. Takes a .zip file and turns it into a parquet file with two columns.";
    homepage = "https://github.com/Browsers-software/browsers";
    license = lib.licenses.unlicense;
    maintainers = [];
  };
}
