{
  pkgs,
  lib,
  fetchFromGitHub,
  rustPlatform,
}:
rustPlatform.buildRustPackage rec {
  pname = "browsers";
  # 0.5.5 (latest) requires rustc >=1.74 and NixOS 23.11 has 1.73.
  # I couldn't figure out quickly how to override this
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "Browsers-software";
    repo = "browsers";
    rev = "0.6.0";
    hash = "sha256-qLqyv5XXG7cpW+/eNCWguqemT3G2BhnolntHi2zZJ0o=";
  };

  nativeBuildInputs = [pkgs.pkg-config];
  buildInputs = [pkgs.glib.dev pkgs.glibc.dev pkgs.cairo.dev pkgs.pango.dev pkgs.atkmm.dev pkgs.gtk3.dev];

  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    allowBuiltinFetchGit = true;
  };

  # cargoHash = "";

  meta = {
    description = "A really simple command line utility. Takes a .zip file and turns it into a parquet file with two columns.";
    homepage = "https://github.com/Browsers-software/browsers";
    license = lib.licenses.unlicense;
    maintainers = [];
  };
}
