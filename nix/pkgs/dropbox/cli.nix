{ lib, stdenv
, substituteAll
, autoreconfHook
, pkg-config
, fetchurl
, python3
, dropbox
, gtk3
, gnome
, gdk-pixbuf
, gobject-introspection
}:

let
  version = "2022.12.05";
  dropboxd = "${dropbox}/bin/dropbox";
in
stdenv.mkDerivation {
  pname = "dropbox-cli";
  inherit version;

  outputs = [ "out" "nautilusExtension" ];

  src = fetchurl {
    url = "https://linux.dropbox.com/packages/nautilus-dropbox-${version}.tar.bz2";
    sha256 = "4c1f13176d66a5ad9ebf964e035be15960ca1d47f4cc9f2976963f39e5de592b";
  };

  strictDeps = true;

  patches = [
    # Fix extension for Nautilus 43
    # https://github.com/dropbox/nautilus-dropbox/pull/105
    # ./nautilus-43.patch

    (substituteAll {
      src = ./fix-cli-paths.patch;
      inherit dropboxd;
    })
  ];

  nativeBuildInputs = [
    autoreconfHook
    pkg-config
    gobject-introspection
    gdk-pixbuf
    # only for build, the install command also wants to use GTK through introspection
    # but we are using Nix for installation so we will not need that.
    (python3.withPackages (ps: with ps; [
      docutils
      pygobject3
    ]))
  ];

  buildInputs = [
    python3
    gtk3
    gnome.nautilus
  ];

  configureFlags = [
    "--with-nautilus-extension-dir=${placeholder "nautilusExtension"}/lib/nautilus/extensions-3.0"
  ];

  makeFlags = [
    "EMBLEM_DIR=${placeholder "nautilusExtension"}/share/nautilus-dropbox/emblems"
  ];

  meta = {
    homepage = "https://www.dropbox.com";
    description = "Command line client for the dropbox daemon";
    license = lib.licenses.gpl3Plus;
    # NOTE: Dropbox itself only works on linux, so this is ok.
    platforms = lib.platforms.linux;
  };
}
