{
  pkgs,
  lib,
  fetchFromGitLab,
  rustPlatform,
}:
rustPlatform.buildRustPackage rec {
  pname = "rumno";
  version = "2020-08-17";

  src = fetchFromGitLab {
    owner = "natjo";
    repo = "rumno";
    rev = "8a0ef43c06cbd353d34f33f938c6b30763eda91a";
    sha256 = "sha256-mSswFbwWsNDV8KnHlG4CyhOI0bpHCYMKAVGXwCtkD5U=";
  };

  nativeBuildInputs = [pkgs.pkg-config];
  buildInputs = [pkgs.dbus.dev pkgs.gtk3.dev pkgs.glib.dev];

  cargoHash = "sha256-0V7Lgf+QCYY9P3awiQFa0foGfhRGt9/ncAqLAW9GfPs=";

  cargoPatches = [
    # a patch file to add/update Cargo.lock in the source code
    ./add-Cargo.lock.patch
  ];

  meta = with lib; {
    description = "Rust media notification manager for volume, brightness and music control. Based on the idea of volnoti.";
    homepage = "https://gitlab.com/natjo/rumno";
  };
}
