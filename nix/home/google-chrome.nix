{
  config,
  pkgs,
  lib,
  ...
}: {
  home.packages = [
    # Chrome on Linux leaves VA-API video encode off by default, so video
    # calls encode the camera stream on the CPU.
    (pkgs.google-chrome.override {
      commandLineArgs = ["--enable-features=AcceleratedVideoEncoder"];
    })
  ];
}
