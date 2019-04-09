# init-instroot.scm

Root filesystem initializer using LUKS encryption for root partition, and choice between LVM or swapfile based swap configuration. Optionally allows for using a ZFS pool for custom root directories and swap volume.

## Examples

    init_instroot.scm -r /dev/sda -s 4G

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for a [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) encrypted device, used with two [LVM](https://en.wikipedia.org/wiki/Logical_volume_management) logical volumes, one as a 4G swap, and the rest as an ext4 formatted root.

    init_instroot.scm  -r /dev/sda -s 4G -S 2

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for a [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) encrypted device formatted ext4 for root. Additionally, a swap space of 4G is split into two 2G swap files under `/var/swap` directory.

    init_instroot.scm -r /dev/sda -z tank -r system -d home,var,gnu -s 4G

Configures device `/dev/sda` with two partitions: 500M formatted ext2 for /boot, and the rest for a [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) encrypted device formatted ext4 for root. Additionally will look for a ZFS pool named `tank`on which it creates dataset named `system`under which it creates subdatasets home, var, gnu, and a ZFS volume named `swap` with size of 4G. The swap volume is formatted and used as swap device, while the other subdatasets are mounted as directories under the target root of the LUKS encrypted partition. 

    init_instroot.scm -k mykey -v /dev/sdb:crypt_tank1,/dev/sdc:crypt_tank2 \
    -z tank -r system -d home,var,gnu -s 4G /dev/sda

Similar to before, except beforehand it additionally uses key-file `mykey`to unlock LUKS devices `/dev/sdb` and `/dev/sdc` with the given labels. These encrypted devices potentially hold the ZFS pool, which will then be accessed as before. The keyfile used to unclock the devices will be placed under `/root/crypt`of the target root directory, and entries for the devices will be generated in `/etc/crypttab`.

In all examples the initialized devices will be mounted by default under `/mnt/instroot`, with `fstab` and `crypttab` entries generated and placed under `/etc`. For all LUKS encrypted devices header backups are saved under `/root/crypt/headers`.

See usage help with option `-h` for more details about all options.

# destroy-instroot.scm

After each execution of `init_instroot.scm`a config file `.lastrun.scm` is generated containing the parameters which were used. This allows easy un-initialization of the complete root setup with a single call to `destroy-instroot.scm`. This destroys any content which has since been added to the "initialized" root directory:
  * It unmounts all folders,
  * detaches swap devices,
  * removes LVM volumes and volume-groups,
  * unmounts and exports ZFS pools,
  * closes all LUKS encrypted devices,
  * destroys all partition tables created.
