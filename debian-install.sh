#!/bin/sh

partuuid () {
    if [ $# -eq 2 -a -b $1 -a $2 -lt 3 ]
    then
	sgdisk -i $2 $1|grep "Partition unique GUID:"|sed -e "s;^.*: \([[:alnum:]-]*\)$;\L\1;";
    else
	echo "ERROR: called partuuid with args: $@" >&2
	return 1
    fi
}

fsuuid () {
    if [ $# -eq 1 -a -b $1 ]
    then
	blkid -s UUID -o value $1
    else
	echo "ERROR: called fsuuid with args: $@" >&2
	return 1
    fi
}

install_deps () {
    if [ $# -eq 1 -a ! -z "$1" ]
    then
	RELEASE=$1
	echo "deb http://ftp.debian.org/debian $RELEASE-backports main contrib" > /etc/apt/sources.list.d/backports.list
	apt update
	apt upgrade -y
	apt install -y gdisk cryptsetup lvm2 debootstrap linux-headers-$(uname -r) pv
	apt install -y -t $RELEASE-backports zfs-dkms
    else
	echo "ERROR: called install-deps with args: $@" >&2
	return 1
    fi
}

init_parts () {
    if [ $# -eq 1 -a -b $1 ]
    then
	ROOT_DRIVE=$1
	echo "Setting up partitions..."
	sgdisk $ROOT_DRIVE -o -n 1:0:+500M -N 2 -t 1:ef02
    else
	echo "ERROR: called init-parts with args: $@" >&2
	return 1
    fi
}

init_cryptroot () {
    if [ $# -eq 2 -a -b /dev/disk/by-partuuid/$1 -a ! -z $(echo $2|grep -E "^[[:alnum:]_]+$") ]
    then
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
		echo "Shredding LUKS device..."
		pv < /dev/zero > /dev/mapper/$LUKS_LABEL
		echo "Finished shredding LUKS device..."
		;;
	esac

	echo "Finished setting up LUKS device: $LUKS_LABEL"
    else
	echo "ERROR: calling init-cryptroot with args: $@" >&2
	return 1
    fi
}

init_zfsroot () {
    if [ $# -eq 2 -a ! -z $(echo $1|grep -E "^[[:alnum:]]+$") -a ! -z $(echo $2 | grep -E "^[0-9]+[TGMK]$") ]
    then
	ZPOOL=$1
	SWAPSIZE=$2
	SYSTEMFS=$ZPOOL/system
	if [ ! -z "$(zfs list -H)" -a -z $(zfs list -o name|grep -E "^$SYSTEMFS$")]
	then
	    # These steps assume that a ZFS pool has been already imported
	    zfs create -o compression=lz4 -o canmount=off $SYSTEMFS
	    for i in home var gnu
	    do zfs create $SYSTEMFS/$i; done
	    zfs create -V $SWAPSIZE $SYSTEMFS/swap
	    mkswap /dev/zvol/$SYSTEMFS/swap
	else
	    echo "ERROR: no ZFS datasets available, or the system dataset already exist!" >&2
	    return 1
	fi
    else
	echo "ERROR: calling init-zfsroot with args: $@" >&2
	return 1
    fi
}

init_instroot () {
    if [ $# -eq 4 -a ! -e $1 -a -b /dev/mapper/$2 -a -b /dev/disk/by-partuuid/$3 -a ! -z $(zpool list -H -o name|grep -E "^$4$")]
    then
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
    else
	echo "ERROR: calling init-instroot with args: $@" >&2
	return 1
    fi
}

RELEASE=jessie
MIRROR=http://ftp.uk.debian.org/debian
LUKS_LABEL=crypt_root
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

-s SWAPSIZE
Size of swap device partition (TGMK suffixes allowed)

-i PATH
Install root mountpoint (default $INSTROOT)

-h
This usage help...
EOF
}

while getopts 'r:m:n:l:z:s:i:h' opt
do
    case $opt in
	r)
	    RELEASE=$OPTARG
	    ;;
	m)
	    MIRROR=$OPTARG
	    ;;
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	z)
	    ZPOOL=$OPTARG
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
	    echo "MISSING ARGUMENT FOR OPTION: $OPTARG" >&2
	    exit 1
	    ;;
	?)
	    echo "INVALID OPTION: $OPTARG" >&2
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

if [ -z "$ZPOOL" -o -z $(zpool list -H -o name $ZPOOL | grep -E "^$ZPOOL$") ]
then
    echo "ZFS pool is required for swap space, home, var, and gnu directories" >&2
    exit 1
fi

if [ -z "$SWAPSIZE" -o -z $(echo $SWAPSIZE | grep -E "^[0-9]+[TGMK]$") ]
then
    echo "Swap size has to be specified" >&2
    exit 1
fi

install_deps $RELEASE
init_parts $ROOT_DRIVE
BOOT_PARTUUID=$(partuuid $ROOT_DRIVE 1)
LUKS_PARTUUID=$(partuuid $ROOT_DRIVE 2)
init_cryptroot $LUKS_PARTUUID $LUKS_LABEL
init_zfsroot $ZPOOL $SWAPSIZE
init_instroot $INSTROOT $LUKS_LABEL $BOOT_PARTUUID $ZPOOL

debootstrap $RELEASE $INSTROOT $MIRROR
