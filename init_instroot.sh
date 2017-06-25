#!/bin/sh

partuuid () {
    if [ ! "$#" -eq 2 -o ! -b "$1" -o ! "$2" -lt 3 ]
    then
	echo "ERROR: called partuuid with args: $@" >&2
	exit 1
    fi

    sgdisk -i $2 $1|grep "Partition unique GUID:"|sed -e "s;^.*: \([[:alnum:]-]*\)$;\L\1;";
}

fsuuid () {
    if [ ! "$#" -eq 1 -o ! -b "$1" ]
    then
	echo "ERROR: called fsuuid with args: $@" >&2
	exit 1
    fi

    blkid -s UUID -o value $1
}

install_deps_base () {
    if [ ! -e .deps_base ]
    then
	echo "Installing necessary packages..."
	apt update
	apt install -y gdisk cryptsetup pv lvm2
	touch .deps_base
    fi
}

install_deps_zfs () {
    if [ ! -e .deps_zfs ]
    then
	RELEASE=$(cat /etc/debian_version | sed -e 's;^\([0-9][0-9]*\)\..*$;\1;')
	case $RELEASE in
	    "8")
		echo /etc/apt/sources.list | grep -E '^deb .* jessie main$' | sed -e 's/jessie main/jessie-backports main contrib/' > /etc/apt/sourced.list.d/backports.list
		apt update
		apt install -y -t jessie-backports zfs-dkms
		;;
	    "9")
		sed -i -re 's/^deb \(.+\) stretch main$/deb \1 stretch main contrib/' /etc/apt/sources.list.d/base.list
		apt update
		apt install -y zfs-dkms
		;;
	    *)
		echo "ERROR: Debian version $RELEASE is not supported!"
		exit 1
		;;
	esac
	touch .deps_zfs
    fi
}

init_parts () {
    if [ ! "$#" -eq 1 -o ! -b "$1" ]
    then
	echo "ERROR: called init_parts with args: $@" >&2
	exit 1
    fi

    ROOT_DRIVE=$1
    echo "Setting up partitions..."
    sgdisk $ROOT_DRIVE -o -n 1:0:+500M -N 2 -t 1:ef02
    echo "Finished setting up partitions."
}

init_cryptroot () {
    if [ ! "$#" -eq 2 -o ! -b "/dev/disk/by-partuuid/$1" -o -z "$(echo $2|grep -E '^[[:alnum:]_]+$')" ]
    then
	echo "ERROR: calling init_cryptroot with args: $@" >&2
	exit 1
    fi

    LUKS_PARTUUID=$1
    LUKS_LABEL=$2

    echo "Formatting partition to be used as LUKS device..."
    cryptsetup luksFormat /dev/disk/by-partuuid/$LUKS_PARTUUID
    echo "Opening LUKS device..."
    cryptsetup luksOpen /dev/disk/by-partuuid/$LUKS_PARTUUID $LUKS_LABEL && exit 1

    cat <<EOF

It is recommended to overwrite the LUKS device with random data.
WARNING: This can take quite a long time!
EOF

    read -p "Would you like to overwrite LUKS device with random data? [Y/n]" shred
    case $shred in
	[nN])
	    echo "Skipping LUKS device shredding."
	    ;;
	*)
	    LUKS_DEV=/dev/mapper/$LUKS_LABEL
	    echo "Shredding LUKS device..."
	    pv --size $(blockdev --getsize64 $LUKS_DEV) < /dev/zero > $LUKS_DEV
	    echo "Finished shredding LUKS device..."
	    ;;
    esac

    echo "Finished setting up LUKS device: $LUKS_LABEL"
}

init_cryptdevs () {
    if [ ! "$#" -eq 2 -o ! -e "$1"]
    then
	echo "ERROR: calling init_cryptdevs with args: $@" >&2
	exit 1
    fi

    KEYFILE=$1
    DEVLIST=$2

    for i in $(echo $DEVLIST| tr "," "\n")
    do
	device=$(echo $i|cut -d : -f1)
	label=$(echo $i|cut -d : -f2)
	if [ ! -b "/dev/mapper/$label" ]
	then
	    cryptsetup luksOpen $device $label --key-file $KEYFILE || exit 1
	fi
    done
    unset device
    unset label
    unset i
}

init_zfsroot () {
    if [ ! "$#" -eq 4 -o -z "$(zpool list $1)" -o -z "$(echo 2 | grep -E '^[[:alnum:]]+$')" -o -z "$(echo $3 | grep -E '^[0-9]+[KMGT]?$')" ]
    then
	echo "ERROR: calling init_zfsroot with args: $@" >&2
	exit 1
    fi

    ZPOOL=$1
    FSNAME=$2
    SWAPSIZE=$3
    DIRLIST=$4

    SYSTEMFS=$ZPOOL/$FSNAME

    if [ ! -z "$(zfs list $SYSTEMFS)" ]
    then
	echo "ERROR: $SYSTEMFS dataset already exist!" >&2
	exit 1
    fi

    # system dataset should be used for mountpoint inheritance only, and never automount
    zfs create -o compression=lz4 -o canmount=off $SYSTEMFS
    for i in $(echo $DIRLIST | tr "," "\n")
    do zfs create $SYSTEMFS/$i; done
    zfs create -V $SWAPSIZE $SYSTEMFS/swap
    mkswap /dev/zvol/$SYSTEMFS/swap
}

init_lvmroot () {
    if [ ! "$#" -eq 2 -o ! -b "/dev/mapper/$1" -o -z "$(echo $SWAPSIZE | grep -E '^[0-9]+[KMGT]?$')" ]
    then
	echo "ERROR: calling init_lvmroot with args: $@" >&2
	exit 1
    fi
    LUKS_LABEL=$1
    VG_NAME=${LUKS_LABEL}_vg
    SWAP_SIZE=$2
    pvcreate  /dev/mapper/$LUKS_LABEL
    vgcreate $VG_NAME /dev/mapper/$LUKS_LABEL
    lvcreate -L $SWAP_SIZE $VG_NAME -n swap
    lvcreate -l 100%FREE $VG_NAME -n root
}

init_instroot_lvm () {
    if [ ! "$#" -eq 3 -o -e "$1" -o ! -b "/dev/mapper/$2" -o ! -b "/dev/disk/by-partuuid/$3" ]
    then
	echo "ERROR: calling init_instroot_lvm with args: $@" >&2
	exit 1
    fi

    INSTROOT=$1
    ROOT_DEV=/dev/mapper/$2
    BOOT_DEV=/dev/disk/by-partuuid/$3

    mkdir -p $INSTROOT
    mkfs.ext4 $ROOT_DEV
    mkfs.ext4 -m 0 -j $BOOT_DEV
    mount $ROOT_DEV $INSTROOT
    mkdir $INSTROOT/boot
    mount $BOOT_DEV /$INSTROOT/boot
}

init_instroot_zfs () {
    if [ ! "$#" -eq 9 -o -d "$1" -o ! -b "/dev/mapper/$2" -o ! -b "/dev/disk/by-partuuid/$3" -o ! -b "/dev/disk/by-partuuid/$4" -o -z "$(zpool list $5)" -o -z "$(zfs list $5/$6)" ]
    then
	echo "ERROR: calling init_instroot_zfs with args: $@" >&2
	exit 1
    fi

    INSTROOT=$1
    LUKS_LABEL=$2
    LUKS_PARTUUID=$3
    BOOT_PARTUUID=$4
    ZPOOL=$5
    ROOTFS=$6
    KEYFILE=$7
    DEVLIST=$8
    DIRLIST=$9

    mkdir -p $INSTROOT
    mkfs.ext4 /dev/mapper/$LUKS_LABEL
    mkfs.ext4 -m 0 -j /dev/disk/by-partuuid/$BOOT_PARTUUID
    mount /dev/mapper/$LUKS_LABEL $INSTROOT
    mkdir $INSTROOT/boot
    mkdir $INSTROOT/root
    mkdir $INSTROOT/etc
    mount /dev/disk/by-partuuid/$BOOT_PARTUUID /$INSTROOT/boot
    zfs set mountpoint=$INSTROOT $ZPOOL/$ROOTFS

    LUKS_UUID=$(fsuuid /dev/disk/by-partuuid/$LUKS_PARTUUID)
    ROOT_UUID=$(fsuuid /dev/mapper/$LUKS_LABEL)
    SWAP_UUID=$(fsuuid /dev/zvol/$ZPOOL/$ROOTFS/swap)

    cat <<EOF > $INSTROOT/etc/fstab
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors-remount-ro 0 1
UUID=$SWAP_UUID none swap sw,x-systemd.after=zfs.target 0 0

# systemd specific legacy mounts of ZFS datasets
EOF

    for i in $(echo $DIRLIST | tr "," "\n")
    do
	# systemd specific legacy ZFS fstab mountpoint entries (commented out by default)
	echo "#$ZPOOL/$ROOTFS/$i /$i zfs defaults,x-systemd.after=zfs.target 0 0" >> $INSTROOT/etc/fstab
    done
    unset i

    cat <<EOF > $INSTROOT/etc/crypttab
# LUKS device containing root filesystem
$LUKS_LABEL UUID=$LUKS_UUID none luks

# LUKS encrypted devices of ZFS member vdevs
EOF

    ROOTCRYPT_DIR=$INSTROOT/root/crypt
    mkdir -p $ROOTCRYPT_DIR/headers
    cryptsetup luksHeaderBackup /dev/disk/by-partuuid/$LUKS_PARTUUID \
	       --header-backup-file $ROOTCRYPT_DIR/headers/$LUKS_LABEL

    if [ ! -z "$KEYFILE" -a -e "$KEYFILE"]
    then
	cp $KEYFILE $ROOTCRYPT_DIR
	ROOT_KEYFILE=$ROOTCRYPT_DIR/$(basename $KEYFILE)
	
	for i in $(echo "$DEVLIST" | tr "," "\n")
	do
	    device=$(echo $i|cut -d : -f1)
	    label=$(echo $i|cut -d : -f2)
	    if [ -b "$device" -a ! -z "$label" ]
	    then
	       uuid=$(fsuuid $device)
	       
	       # creating crypttab entries for LUKS encrypted devices of ZFS member vdevs
	       echo "$label UUID=$uuid $ROOT_KEFILE luks" >> $INSTROOT/etc/crypttab
	       
	       # backing up LUKS headers of ZFS member vdevs
	       cryptsetup luksHeaderBackup $device \
			  --header-backup-file $ROOTCRYPT_DIR/headers/$label
	    fi
	done
	unset device
	unset label
	unset uuid
	unset i
    fi
}

LUKS_LABEL=crypt_root
ROOTFS=system
DIRLIST="home,var,gnu"
INSTROOT=/mnt/instroot

usage () {
    cat <<EOF

USAGE:

$0 [OPTIONS] DEVICE

General purpose encrypted root filesystem initializer, using LVM or optionally a ZFS pool, for home, var, and swap space

Valid options are:

-m PATH
Install root mountpoint (default $INSTROOT)

-l LABEL
LUKS encrypted device name (default $LUKS_LABEL)

-Z
Install and configure package dependencies only

-z ZPOOL
ZFS pool name for system directories and swap device

-k KEYFILE
Keyfile used to decrypt other encrypted devices (i.e. ZFS pool members)

-c DEVLIST
Coma separeted list of colon separated pairs of other encrypted devices (i.e. members of ZFS pool), and their repsective LUKS labels.
E.g. /dev/sdb:foo,/dev/sdc:bar,/dev/sdd:baz

These mappings are used to:
 a) unlock these devices before importing ZFS pools
 b) create crypttab entries for automatic unlocking during boot

Specifying a keyfile is necessary for this feature!

-d DIRLIST
Coma separated list of root directories to mount as ZFS datasets (default $DIRLIST)

-r NAME
Name of the system root dataset in the ZFS pool (default $ROOTFS)

-s SWAPSIZE
Size of swap device partition (KMGT suffixes allowed)

-h
This usage help...

EOF
}

if [ $(id -u) -ne 0 ]
then
    echo "This script must be run as root!" >&2
    exit 1
fi

while getopts 'l:m:Zz:k:c:d:r:s:h' opt
do
    case $opt in
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	m)
	    INSTROOT=$OPTARG
	    ;;
	Z)
	    install_deps_base
	    install_deps_zfs
	    "Finished installing all package dependencies!"
	    exit 0
	    ;;
	z)
	    ZPOOL=$OPTARG
	    ;;
	k)
	    KEYFILE=$OPTARG
	    ;;
	c)
	    DEVLIST=$OPTARG
	    ;;
	d)
	    DIRLIST=$OPTARG
	    ;;
	r)
	    ROOTFS=$OPTARG
	    ;;
	s)
	    SWAPSIZE=$OPTARG
	    ;;
	h)
            usage
            exit 0
	    ;;
	:)
	    echo "ERROR: Missing argument for potion: -$OPTARG" >&2
	    exit 1
	    ;;
	\?)
	    echo "ERROR: Illegal option -$OPTARG" >&2
	    exit 1
	    ;;
	*)
	    usage
	    exit 1
	    ;;
    esac
done

shift $(($OPTIND - 1))

if [ "$#" -eq 1 -a -b "$1" ]
then
    ROOT_DRIVE=$1
else
    echo "ERROR: Block device must be specified for root filesystem!" >&2
    exit 1
fi

if [ -z "$SWAPSIZE" -o -z "$(echo $SWAPSIZE | grep -E '^[0-9]+[KMGT]?$')" ]
then
    echo "ERROR: Swap size has to be specified (KMGT suffixes allowed)" >&2
    exit 1
fi

if [ ! -z "$KEYFILE" -a ! -e "$KEYFILE" ]
then
    echo "ERROR: keyfile $KEYFILE is not found!" >&2
    exit 1
fi

if [ ! -z "$DEVLIST" -a -z "$KEYFILE" ]
then
    echo "ERROR: Encrypted device mappings cannot be specified without keyfile!" >&2
    exit 1
fi

install_deps_base
init_parts $ROOT_DRIVE
BOOT_PARTUUID=$(partuuid $ROOT_DRIVE 1)
LUKS_PARTUUID=$(partuuid $ROOT_DRIVE 2)
init_cryptroot $LUKS_PARTUUID $LUKS_LABEL

if [ ! -z "$ZPOOL" ]
then
    [ -z "$KEYFILE" ] || init_cryptdevs $KEYFILE "$DEVLIST"
    install_deps_zfs
    init_zfsroot $ZPOOL $ROOTFS  $SWAPSIZE "$DIRLIST"
    init_instroot_zfs $INSTROOT $LUKS_LABEL $LUKS_PARTUUID $BOOT_PARTUUID $ZPOOL $ROOTFS "$KEYFILE" "$DEVLIST" "$DIRLIST"
else
    init_lvmroot $LUKS_LABEL $SWAPSIZE
    ROOT_LVNAME=${LUKS_LABEL}_vg-root
    init_instroot_lvm $INSTROOT $ROOT_LVNAME $BOOT_PARTUUID
fi
