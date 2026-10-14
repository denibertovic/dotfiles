{
  writeShellApplication,
  pipewire,
  jq,
  rofi,
  libnotify,
}:
writeShellApplication {
  name = "audioctl";
  runtimeInputs = [pipewire jq rofi libnotify];
  text = builtins.readFile ./audioctl.sh;
}
