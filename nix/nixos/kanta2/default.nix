# kanta2: Intel Core Ultra 7 155U laptop
{...}: {
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
    device = "/dev/disk/by-uuid/@LUKS_UUID@";
    allowDiscards = true;
  };
  swapDevices = [{device = "/dev/mapper/cryptswap";}];
  boot.resumeDevice = "/dev/mapper/cryptswap";
}
