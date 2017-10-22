# init_instroot.sh

General purpose root filesystem initializer using LUKS encryption for root, and choice between LVM of swapfile based swap configuration. Optionally allows for using a ZFS pool for root directories and swap volume.

## Examples

    init_instroot.sh -s 4G /dev/sda

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for an LUKS encrypted device used as LVM logical volumes for ext4 formatted root and 4G swap devices.

    init_instroot.sh -s 4G -S 2 /dev/sda

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for an LUKS encrypted device formatted ext4 for root. Swap space of 4G is split into two 2G swap files under `/var/swap` directory.

    init_instroot.sh -z tank -r system -d home,var,gnu -s 4G /dev/sda

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for an LUKS encrypted device formatted ext4 for root. Additionally will look for a ZFS pool named `tank`on which it creates dataset named `system`under wich it creates subdatasets home, var, gnu, and a ZFS volume named `swap` with size of 4G. The swap volume is formatted and used as swap device, while the other subdatasets are mounted as directories under the target root of the LUKS encrypted partition. 

    init_instroot.sh -k mykey -d /dev/sdb:crypt_tank1,/dev/sdc:crypt_tank2
    -z tank -r system -d home,var,gnu -s 4G /dev/sda

Similar to before, except beforehand it additionally uses key-file `mykey`to unlock LUKS devices `/dev/sdb` and `/dev/sdc` with the given labels. These encrypted devices potentially hold the ZFS pool, which will then be accessed as before. The keyfile used to unclock the devices will be placed under `/root/crypt`of the target root directory, and entries for the devices will be generated in `/etc/crypttab`.

In all examples the initialized devices will be mounted by default under `/mnt/instroot`, with `fstab` and `crypttab` entries generated and placed under `/etc`. For all LUKS encrypted devices header backups are saved under `/root/crypt/headers`.

See usage help with option `-h` for more details about all options.
