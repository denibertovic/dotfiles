# kanta2: Intel Core Ultra 7 155U laptop
{lib, ...}: {
  imports = [./hardware-configuration.nix];

  networking.hostName = "kanta2";
  networking.hostId = "1272609d"; # cut -c-8 </proc/sys/kernel/random/uuid
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # HIBERNATION
  # Swap is a LUKS partition, unlocked in the initrd before the resume attempt.
  # The pool must never be force imported: a forced import while a hibernated
  # kernel still holds the pool open corrupts it. unsafeAllowHibernation drops
  # the "nohibernate" kernel param that the zfs module adds by default and
  # asserts forceImportRoot = false. Never import this pool from a live USB
  # while the machine is hibernated.
  boot.zfs.forceImportRoot = false;
  boot.zfs.unsafeAllowHibernation = true;
  boot.initrd.luks.devices.cryptswap = {
    device = "/dev/disk/by-uuid/4bc9082f-8efe-4695-bbc1-a590149acf1e";
    allowDiscards = true;
  };
  swapDevices = [{device = "/dev/mapper/cryptswap";}];
  boot.resumeDevice = "/dev/mapper/cryptswap";
  # nixpkgs orders the initrd pool import only after module loading, so on a
  # resume boot it can import the pool (and ask for the key) before
  # systemd-hibernate-resume runs. The hibernated kernel then resumes with a
  # pool that another kernel just wrote to. Force the import after the
  # resume attempt; on a successful resume the import never runs.
  boot.initrd.systemd.services.zfs-import-laptop.after = ["systemd-hibernate-resume.service"];

  # Only safe here because of the hibernation setup above. Applies on battery
  # and on AC; logind ignores the lid when an external monitor is attached.
  services.logind.settings.Login.HandleLidSwitch = "suspend-then-hibernate";
}
