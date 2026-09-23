# kanta: ThinkPad T14 gen1
{lib, ...}: {
  imports = [./hardware-configuration.nix];

  networking.hostName = "kanta";
  networking.hostId = "96b8f8ce"; # cut -c-8 </proc/sys/kernel/random/uuid
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # psmouse.synaptics_intertouch=0 forces the touchpad OFF the RMI4/SMBus
  # ("intertouch") path and back onto legacy PS/2 Synaptics. The Elan
  # TrackPoint is a PS/2 pass-through routed through the RMI4 device, so when
  # RMI4/SMBus drops interrupts ("rmi_driver_clear_irq_bits: Failed to change
  # enabled interrupts!") the trackpoint stalls with it. Disabling intertouch
  # removes that whole failure path. See ~/scripts/fix_trackpoint_stalling.sh
  # for the old runtime workaround this replaces.
  boot.kernelParams = ["psmouse.synaptics_intertouch=0"];

  # set trackpoint speed and sensitivity
  hardware.trackpoint = {
    enable = true;
    emulateWheel = true;
    # NOTE: `speed` is intentionally omitted. This Elan TrackPoint (fw 0x11)
    # does not expose a `speed` sysfs attribute, so setting it here is a no-op
    # (kernel logs "Could not chase sysfs attribute .../speed, ignoring").
    # Pointer speed is instead controlled via services.libinput.mouse.accelSpeed.
    sensitivity = 255;
    device = "TPPS/2 Elan TrackPoint";
  };

  # The home dataset moved to kanta2, which is now the machine that gets
  # backed up. Keep this host from snapshotting and pushing a stale copy.
  services.zrepl.enable = lib.mkForce false;
}
