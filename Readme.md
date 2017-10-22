# init_instroot.sh

General purpose root filesystem initializer using LUKS encryption for root, and choice between LVM of swapfile based swap configuration. Optionally allows for using a ZFS pool for root directories and swap volume.

## Examples

    init_instroot.sh -s 4G /dev/sda

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for an LUKS encrypted device used as LVM logical volumes for ext4 formatted root and 4G swap devices.

    init_instroot.sh -s 4G -S 2 /dev/sda

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for an LUKS encrypted device formatted ext4 for root. Swap space of 4G is split into two 2G swap files under `/var/swap` directory.

The initialized devices will be mounted by default under `/mnt/instroot`, with `fstab` and `crypttab` entries generated and placed under `/etc`. For all LUKS encrypted devices header backups are saved under `/root/crypt/headers`.

See usage help `-h` for more details about options.
