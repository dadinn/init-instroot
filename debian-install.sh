#!/bin/sh

partuuid () {
    if [ ! $# -eq 2 -o ! -b $1 -o ! $2 -lt 3 ]
    then
	echo "ERROR: called partuuid with args: $@" >&2
	exit 1
    fi

    sgdisk -i $2 $1|grep "Partition unique GUID:"|sed -e "s;^.*: \([[:alnum:]-]*\)$;\L\1;";
}

fsuuid () {
    if [ ! $# -eq 1 -o ! -b $1 ]
    then
	echo "ERROR: called fsuuid with args: $@" >&2
	exit 1
    fi

    blkid -s UUID -o value $1
}

install_deps_base () {
    echo "Installing necessary packages..."
    apt update && apt install -y gdisk cryptsetup pv lvm2 debootstrap
}

install_deps_zfs () {
    if [ ! $# -eq 1 -o -z $1 ]
    then
	echo "ERROR: called install_deps_zfs with args: $@" >&2
	exit 1
    fi

    RELEASE=$1

    case $RELEASE in
	"jessie")
	    echo "deb http://ftp.debian.org/debian jessie-backports main contrib" > /etc/apt/sources.list.d/backports.list
	    apt update
	    apt install -y -t jessie-backports zfs-dkms
	    ;;
	"stretch")
	    sed -i -e 's/^deb \(.*\) stretch main$/deb \1 stretch main contrib/' /etc/apt/sources.list.d/base.list
	    apt update
	    apt install -y zfs-dkms
	    ;;
    esac
}

init_parts () {
    if [ ! $# -eq 1 -o ! -b $1 ]
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
    if [ ! $# -eq 2 -o ! -b /dev/disk/by-partuuid/$1 -o -z "$(echo $2|grep -E '^[[:alnum:]_]+$')" ]
    then
	echo "ERROR: calling init_cryptroot with args: $@" >&2
	exit 1
    fi

    LUKS_PARTUUID=$1
    LUKS_LABEL=$2

    echo "Formatting partition to be used as LUKS device..."
    cryptsetup luksFormat /dev/disk/by-partuuid/$LUKS_PARTUUID
    echo "Opening LUKS device..."
    cryptsetup luksOpen /dev/disk/by-partuuid/$LUKS_PARTUUID $LUKS_LABEL

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

init_zfsroot () {
    if [ ! $# -eq 4 -o -z "$(zpool list $1)" -o -z $(echo 2 | grep -E '^[:alnum:]+$') -o -z $3 -o -z "$(echo $4 | grep -E '^[0-9]+[TGMK]*$')" ]
    then
	echo "ERROR: calling init_zfsroot with args: $@" >&2
	exit 1
    fi

    ZPOOL=$1
    FSNAME=$2
    DIRLIST=$3
    SWAPSIZE=$4
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
    if [ ! $# -eq 2 -o ! -b /dev/mapper/$1 -o -z "$(echo $SWAPSIZE | grep -E '^[0-9]+[TGMK]$')" ]
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
    if [ ! $# -eq 3 -o -e $1 -o ! -b /dev/mapper/$2 -o ! -b /dev/disk/by-partuuid/$3 ]
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
    if [ ! $# -eq 4 -o -e $1 -o ! -b /dev/mapper/$2 -o ! -b /dev/disk/by-partuuid/$3 -o -z "$(zpool list $4)" -o -z "$(zfs list $4/system)" ]
    then
	echo "ERROR: calling init_instroot_zfs with args: $@" >&2
	exit 1
    fi

    INSTROOT=$1
    LUKS_LABEL=$2
    BOOT_PARTUUID=$3
    ZPOOL=$4

    mkdir -p $INSTROOT
    mkfs.ext4 /dev/mapper/$LUKS_LABEL
    mkfs.ext4 -m 0 -j /dev/disk/by-partuuid/$BOOT_PARTUUID
    mount /dev/mapper/$LUKS_LABEL $INSTROOT && mkdir $INSTROOT/boot
    mount /dev/disk/by-partuuid/$BOOT_PARTUUID /$INSTROOT/boot
    zfs set mountpoint=$INSTROOT $ZPOOL/system
}

RELEASE=jessie
MIRROR=http://ftp.uk.debian.org/debian
LUKS_LABEL=crypt_root
DIRLIST="home,var,gnu"
INSTROOT=/mnt/inst_root

usage () {
    cat <<EOF

USAGE:

$0 [OPTIONS] DEVICE

Installs Debian on DEVICE with encrypted root filesystem, optionally using a ZFS pool for home, var, and swap filesystems.

Valid options are:

-r RELEASE
Debian release to use (default $RELEASE)

-m URL
Debian mirror URL (default $MIRROR)

-n HOSTNAME
Hostname for new system

-l LABEL
LUKS encrypted device name (default $LUKS_LABEL)

-z ZPOOL
ZFS pool name for system directories and swap device

-d DIRLIST
Coma separated list of root directories to mount as ZFS datasets (default $DIRLIST)

-s SWAPSIZE
Size of swap device partition (TGMK suffixes allowed)

-i PATH
Install root mountpoint (default $INSTROOT)

-h
This usage help...
EOF
}

while getopts 'r:m:n:l:z:d:s:i:h' opt
do
    case $opt in
	r)
	    RELEASE=$OPTARG
	    ;;
	m)
	    MIRROR=$OPTARG
	    ;;
	n)
	    HOSTNAME=$OPTARG
	    ;;
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	z)
	    ZPOOL=$OPTARG
	    ;;
	d)
	    DIRLIST=$OPTARG
	    ;;
	s)
	    SWAPSIZE=$OPTARG
	    ;;
	i)
	    INSTROOT=$OPTARG
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

if [ $# -eq 1 -a -b $1 ]
then
    ROOT_DRIVE=$1
else
    echo "ERROR: Block device must be specified for root filesystem!" >&2
    exit 1
fi

if [ $(id -u) -ne 0 ]
then
    echo "This script must be run as root!" >&2
    exit 1
fi

if [ -z "$SWAPSIZE" -o -z "$(echo $SWAPSIZE | grep -E '^[0-9]+[TGMK]$')" ]
then
    echo "ERROR: Swap size has to be specified (TGMK suffixes allowed)" >&2
    exit 1
fi

if [ -z "$HOSTNAME" -o -z "$(echo $HOSTNAME | grep -E '^[[:alpha:]][[:alnum:]-]+$')" ]
then
    echo "ERROR: Hostname has to be specified for the new system" >&2
    exit 1
fi

install_deps_base
init_parts $ROOT_DRIVE
BOOT_PARTUUID=$(partuuid $ROOT_DRIVE 1)
LUKS_PARTUUID=$(partuuid $ROOT_DRIVE 2)
init_cryptroot $LUKS_PARTUUID $LUKS_LABEL

if [ -z "$ZPOOL" ]
then
    init_lvmroot $LUKS_LABEL $SWAPSIZE
    ROOT_LVNAME=${LUKS_LABEL}_vg-root
    init_instroot_lvm $INSTROOT $ROOT_LVNAME $BOOT_PARTUUID
else
    install_deps_zfs $RELEASE
    init_zfsroot $ZPOOL "system" $DIRLIST $SWAPSIZE
    init_instroot_zfs $INSTROOT $LUKS_LABEL $BOOT_PARTUUID $ZPOOL
fi

debootstrap $RELEASE $INSTROOT $MIRROR
