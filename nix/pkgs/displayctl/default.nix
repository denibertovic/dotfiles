{
  writeShellApplication,
  autorandr,
  rofi,
  libnotify,
  xorg,
}:
writeShellApplication {
  name = "displayctl";
  runtimeInputs = [autorandr rofi libnotify xorg.xrandr];
  text = builtins.readFile ./displayctl.sh;
}
