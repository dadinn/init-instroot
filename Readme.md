# init_instroot.sh

Fully POSIX compliant (no *isms) root filesystem initializer using LUKS encryption for root partition, and choice between LVM or swapfile based swap configuration. Optionally allows for using a ZFS pool for custom root directories and swap volume.

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

# destroy_instroot.sh

After each execution of `init_instroot.sh`a config file `.lastrun` is generated containing the parameters which were used. This allows easy un-initialization of the complete root setup with a single call to `destroy_instroot.sh`. This destroys any content which has since been added to the "initialized" root directory:
  * It unmounts all folders,
  * detaches swap devices,
  * removes LVM volumes and volume-groups,
  * unmounts and exports ZFS pools,
  * closes all LUKS encrypted devices,
  * destroys all partition tables created.
