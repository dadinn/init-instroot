# init-instroot.scm

Initialises and mounts a root filesystem.

* Uses a [LUKS](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) encrypted device formatted as [EXT4](https://wiki.debian.org/Ext4), or [ZFS](https://github.com/openzfs/zfs) pool for root filesystem.
* Uses either plain files, [LVM](https://en.wikipedia.org/wiki/Logical_volume_management), or a [ZFS](https://github.com/openzfs/zfs) volume for swap space.
* Supports either [BIOS](https://en.wikipedia.org/wiki/BIOS) or [UEFI](https://en.wikipedia.org/wiki/Unified_Extensible_Firmware_Interface) boot partitioning.
* Using a separate boot device is an option (but mandatory with ZFS root).

The ZFS pool must be created and configured manually beforehand.

## Examples

In all examples the initialised devices will be mounted by default under `/mnt/instroot`, with `/etc/fstab` and `/etc/crypttab` entries generated. For all LUKS encrypted devices header backups are saved under `/root/crypt/headers`.

See usage help with option `-h` for more details about all options.

#### LUKS encrypted root with LVM for swap space

    init-instroot.scm -r /dev/sda -s 4G

Configures device `/dev/sda` with two partitions: 500M as EXT4 formatted `/boot`, and the rest as a LUKS encrypted device, used as LVM volume group, with two logical volumes: one as a 4G swap, and the rest as EXT4 formatted root.

#### LUKS encrypted root with plain files for swap space

    init-instroot.scm -r /dev/sda -s 4G -S 2

Configures device `/dev/sda` with two partitions: 500M as EXT4 formatted `boot`, and the rest as a LUKS encrypted device as EXT4 formatted root. Additionally, a swap space of 4G is split into two 2G contiguous files under `/var/swap` directory.

#### LUKS encrypted root with unencrypted ZFS pool for swap space and additional root directories

    init-instroot.scm -r /dev/sda -s 4G -z tank -r system -d home,var,gnu

Configures device `/dev/sda` with two partitions: 500M as EXT4 formatted `/boot`, and the rest for a LUKS encrypted device as EXT4 formatted root. Additionally, uses ZFS pool named `tank`. It creates a dataset named `system`, and subdatasets `system/home`, `system/var`, `system/gnu`, and a ZFS volume named `system/swap` with size of 4G. The swap volume is formatted and used as swap device, while the other subdatasets are mounted as subdirectories under the LUKS encrypted root filesystem. 

The `/etc/fstab` is generated with legacy mountpoints for all ZFS datasets.

#### LUKS encrypted root with LUKS encrypted ZFS pool for swap space and additional root directories

    init-instroot.scm -k mykey -v /dev/sdb:crypt_tank1,/dev/sdc:crypt_tank2 \
    -z tank -r system -d home,var,gnu -s 4G /dev/sda

Similar to before, except beforehand it additionally uses key-file `mykey`to unlock LUKS devices `/dev/sdb` and `/dev/sdc` with the given labels. These encrypted devices potentially hold the ZFS pool, which will then be accessed as before. The keyfile used to unclock the devices will be placed under `/root/crypt`of the target root directory, and entries for the devices will be generated in `/etc/crypttab`.

### ZFS root using native encryption

This requires using Debian version 10 (Buster) or later.

    init-instroot.scm -z tank -b /dev/sdb -f system -s 4G

Configures ZFS pool `tank` using dataset `system` as root filesystem, and 4G ZFS volume as swap device. Also, uses `/dev/sdb` as separate boot device.

## Setting up a new ZFS pool

### Installing ZFS kernel modules

To create a ZFS pool, first make sure that the ZFS kernel modules are installed on the system. This can be done easily on [Debian](https://www.debian.org/) with the following command:

	init-instroot.scm -Z

This will install all the package dependencies, and builds and loads the kernel modules.

### Create and configure a new ZFS pool

Always use the long `/dev/disk/by-id/*` or `/dev/disk/by-path/*` device paths when creating a ZFS pool. Using the `/dev/sd*` device nodes directly can cause sporadic import failures, especially on systems that have more than one storage pool. 

Create a ZFS pool with suggested options for passphrase encrypted root dataset, UTF-8 filenames, and 4Kb sector size with the following command:

	init-instroot -Z $ZPOOL_NAME $VDEV_SPECS

This creates a new ZFS pool with encrypted root dataset, asking for passphrase, and is equivalent to creating the pool manually with the following command:

	zpool create -f -o ashift=12 \
	-O encryption=aes-128-gcm -O pbkdf2iters=1000000 \
	-O keyformat=passphrase -O keylocation=prompt \
	-O normalization=formD -O atime=off -O devices=off \
	-O acltype=posixacl -O xattr=sa -O \
	$ZPOOL_NAME $VDEV_SPECS

For more details about the `zpool` command, the available options, and the VDEV specification format refer to the [manual](https://zfsonlinux.org/manpages/0.8.1/man8/zpool.8.html#lbAE).

# destroy-instroot.scm

After each execution of `init-instroot.scm`a config file `.defaults.scm` is generated containing the parameters which were used. This allows easy un-initialisation of the complete root setup with a single call to `destroy-instroot.scm`. This destroys any content which has since been added to the "initialised" root directory. It does the following steps:
  * unmounts all target folders, and ZFS datasets
  * exports ZFS pool used
  * removes LVM volumes and volume-groups
  * closes LUKS encrypted root device
  * closes additional LUKS devices
  * destroys all partition tables created

