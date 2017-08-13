#!/bin/sh

partuuid () {
    if [ ! "$#" -eq 2 -o ! -b "$1" -o ! "$2" -lt 3 ]
    then
	echo "ERROR: called partuuid with args: $@" >&2
	exit 1
    fi

    sgdisk -i $2 $1 | grep -E '^Partition unique GUID:' | sed -e 's;^[^:]*: \([[:alnum:]-]*\)$;\1;' | tr '[:upper:]' '[:lower:]'
}

fsuuid () {
    [ $# -eq 1 ] || (echo "ERROR: called fsuuid with $# args: $@" && exit 1) >&2

    blkid -s UUID -o value $1
}

install_deps_base () {
    if ! type sgdisk partprobe cryptsetup pv pvcreate vgcreate lvcreate &> /dev/null
    then
	if [ ! -e /etc/debian_version ]
	then
	    echo "ERROR: necessary binaries are missing!" >&2
	    echo "ERROR: Please make sure following binaries are available: sgdisk partprobe cryptsetup pv pvcreate vgcreate lvcreate" >&2
	    exit 1
	fi

	echo "Installing necessary packages..."
	apt update
	apt install -y gdisk parted cryptsetup pv lvm2
    fi
}

install_deps_zfs () {
    if [ ! -e .deps_zfs ]
    then
	if [ ! -e /etc/debian_version ]
	then
	    echo "ERROR: necessary binaries are missing!" >&2
	    echo "ERROR: please make sure ZFS kernel modules are loaded and CLI tools *zpool* and *zfs* are avialable." >&2
	    exit 1
	fi

	RELEASE=$(cat /etc/debian_version | sed -e 's;^\([0-9][0-9]*\)\..*$;\1;')
	case $RELEASE in
	    "8")
		cat /etc/apt/sources.list | grep -E '^deb .* jessie main$' | sed -e 's/jessie main/jessie-backports main contrib/' > /etc/apt/sources.list.d/backports.list
		apt update
		apt install -y -t jessie-backports zfs-dkms
		modprobe zfs
		;;
	    "9")
		sed -i -re 's/^deb (.+) stretch main$/deb \1 stretch main contrib/' /etc/apt/sources.list.d/base.list
		apt update
		apt install -y zfs-dkms
		modprobe zfs
		;;
	    *)
		echo "ERROR: Debian version $RELEASE is not supported!" >&2
		exit 1
		;;
	esac
	touch .deps_zfs
    fi
}

init_parts () {
    if [ $# -eq 1 ]
    then
	ROOT_DRIVE=$1
    else
	echo "ERROR: called init_parts with $# args: $@" >&2
	exit 1
    fi

    echo "Setting up partitions..."
    sgdisk $ROOT_DRIVE -Z -n 1:0:+500M -N 2 -t 1:ef02 > /dev/null
    partprobe $ROOT_DRIVE
    echo "Finished setting up partitions on: $ROOT_DRIVE"
}

init_cryptroot () {
    if [ $# -eq 2 ]
    then
	LUKS_PARTDEV=$1
	LUKS_LABEL=$2
    else
	echo "ERROR: called init_cryptroot with $# args: $@" >&2
	exit 1
    fi

    echo
    echo "Formatting $LUKS_PARTDEV to be used as LUKS device..."
    cryptsetup luksFormat $LUKS_PARTDEV
    echo "Finished formatting device $LUKS_PARTDEV for LUKS encryption!"
    echo
    echo "Opening LUKS device..."
    if ! cryptsetup luksOpen $LUKS_PARTDEV $LUKS_LABEL
    then
	echo "ERROR: failed to open LUKS device: $LUKS_LABEL " >&2
	exit 1
    fi

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
	    pv -Ss $(blockdev --getsize64 $LUKS_DEV) < /dev/zero > $LUKS_DEV
	    echo "Finished shredding LUKS device: $LUKS_DEV"
	    ;;
    esac

    echo "Finished setting up LUKS device: $LUKS_LABEL"
}

init_cryptdevs () {
    if [ $# -eq 2 ]
    then
	KEYFILE=$1
	DEVLIST=$2
    else
	echo "ERROR: called init_cryptdevs with $# args: $@" >&2
	exit 1
    fi

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
    if [ $# -eq 4 ]
    then
	ZPOOL=$1
	FSNAME=$2
	SWAPSIZE=$3
	DIRLIST=$4
    else
	echo "ERROR: called init_zfsroot with $# args: $@" >&2
	exit 1
    fi

    SYSTEMFS=$ZPOOL/$FSNAME

    if ! zpool list $ZPOOL > /dev/null 2>&1
    then
	if ! zpool import $ZPOOL > /dev/null
	then
	    echo "ERROR: could not find or import ZFS pool: $ZPOOL" >&2
	    exit 1
	fi
    fi

    if zfs list $SYSTEMFS > /dev/null 2>&1
    then
	echo "ERROR: $SYSTEMFS dataset already exist!" >&2
	exit 1
    fi

    # system dataset should be used for mountpoint inheritance only, and never automount
    zfs create -o compression=lz4 -o canmount=off $SYSTEMFS
    for i in $(echo $DIRLIST | tr "," "\n")
    do zfs create $SYSTEMFS/$i; done

    echo "Creating ZFS volume for swap device..."
    zfs create -V $SWAPSIZE $SYSTEMFS/swap
    mkswap /dev/zvol/$SYSTEMFS/swap
    echo "Finished setting up ZFS pool: $ZPOOL"
}

init_lvmroot () {
    if [ $# -eq 2 ]
    then
	LUKS_LABEL=$1
	SWAP_SIZE=$2
    else
	echo "ERROR: called init_lvmroot with $# args: $@" >&2
	exit 1
    fi

    VG_NAME=${LUKS_LABEL}_vg

    pvcreate  /dev/mapper/$LUKS_LABEL
    vgcreate $VG_NAME /dev/mapper/$LUKS_LABEL
    lvcreate -L $SWAP_SIZE $VG_NAME -n swap
    lvcreate -l 100%FREE $VG_NAME -n root
}

init_instroot_lvm () {
    if [ $# -eq 4 ]
    then
	INSTROOT=$1
	BOOT_PARTDEV=$2
	LUKS_PARTDEV=$3
	LUKS_LABEL=$4

	[ ! -e $INSTROOT ] || (echo "ERROR: $INSTROOT already exists" && exit 1) >&2
	[ -b $BOOT_PARTDEV ] || (echo "ERROR: $BOOT_PARTDEV has to be a block device" && exit 1) >&2
	[ -b $LUKS_PARTDEV ] || (echo "ERROR: $LUKS_PARTDEV has to be a block device" && exit 1) >&2
	[ ! -z $(echo $LUKS_LABEL | grep -E '^[[:alnum:]_]+$') ] || (echo "ERROR: invalid LUKS label: $LUKS_LABEL" && exit 1) >&2
    else
	echo "ERROR: called init_instroot_lvm with $# args: $@" >&2
	exit 1
    fi

    VG_NAME=${LUKS_LABEL}_vg
    LV_ROOT_DEV=/dev/mapper/${VG_NAME}-root
    LV_SWAP_DEV=/dev/mapper/${VG_NAME}-swap

    mkdir -p $INSTROOT
    mkfs.ext4 -q -m 0 -j $BOOT_PARTDEV
    mkfs.ext4 -q $LV_ROOT_DEV
    mkswap $LV_SWAP_DEV
    swapon $LV_SWAP_DEV
    mount $LV_ROOT_DEV $INSTROOT
    mkdir $INSTROOT/boot
    mkdir $INSTROOT/root
    mkdir $INSTROOT/etc
    mount $BOOT_PARTDEV /$INSTROOT/boot

    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    ROOT_UUID=$(fsuuid $LV_ROOT_DEV)
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)
    SWAP_UUID=$(fsuuid $LV_SWAP_DEV)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat <<EOF > $INSTROOT/etc/fstab
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2
UUID=$SWAP_UUID none swap sw 0 0

EOF

    echo "Generating entries for ${INSTROOT}/etc/crypttab..."
    cat <<EOF > $INSTROOT/etc/crypttab
# LUKS device containing root filesystem
$LUKS_LABEL UUID=$LUKS_UUID none luks

EOF

    ROOTCRYPT_DIR=$INSTROOT/root/crypt
    mkdir -p $ROOTCRYPT_DIR/headers
    chmod -R 700 $INSTROOT/root

    echo "Backing up LUKS headers in ${ROOTCRYPT_DIR}/headers..."
    cryptsetup luksHeaderBackup $LUKS_PARTDEV \
	       --header-backup-file $ROOTCRYPT_DIR/headers/$LUKS_LABEL

    chmod 400 $ROOTCRYPT_DIR/headers/*
}

init_instroot_zfs () {
    if [ $# -eq 9 ]
    then
	INSTROOT=$1
	BOOT_PARTDEV=$2
	LUKS_PARTDEV=$3
	LUKS_LABEL=$4
	ZPOOL=$5
	ROOTFS=$6
	KEYFILE=$7
	DEVLIST=$8
	DIRLIST=$9

	[ ! -e $INSTROOT ] || (echo "ERROR: target $INSTROOT already exists" && exit 1) >&2
	[ -b $BOOT_PARTDEV ] || (echo "ERROR: cannot find boot partition device $BOOT_PARTDEV" && exit 1) >&2
	[ -b $LUKS_PARTDEV ] || (echo "ERROR: cannot find root partition device $LUKS_PARTDEV" && exit 1) >&2
	[ -b /dev/mapper/$LUKS_LABEL ] || (echo "ERROR: cannot find LUKS device $LUKS_LABEL" && exit 1) >&2
	zpool list $ZPOOL || (echo "ERROR: zpool $ZPOOL not available" && exit 1) >&2
	zfs list $ZPOOL/$ROOTFS || (echo "ERROR: ZFS dataset $ZPOOL/ROOTFS does not exist" && exit 1) >&2
    else
	echo "ERROR: called init_instroot_zfs with $# args: $@" >&2
	exit 1
    fi

    mkdir -p $INSTROOT
    mkfs.ext4 /dev/mapper/$LUKS_LABEL
    mkfs.ext4 -m 0 -j $BOOT_PARTDEV
    mount /dev/mapper/$LUKS_LABEL $INSTROOT
    mkdir $INSTROOT/boot
    mkdir $INSTROOT/root
    mkdir $INSTROOT/etc
    mount $BOOT_PARTDEV /$INSTROOT/boot
    zfs set mountpoint=$INSTROOT $ZPOOL/$ROOTFS

    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    ROOT_UUID=$(fsuuid /dev/mapper/$LUKS_LABEL)
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)
    SWAP_UUID=$(fsuuid /dev/zvol/$ZPOOL/$ROOTFS/swap)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat <<EOF > $INSTROOT/etc/fstab
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2
UUID=$SWAP_UUID none swap sw,x-systemd.after=zfs.target 0 0

# systemd specific legacy mounts of ZFS datasets
EOF

    # systemd specific legacy ZFS fstab mountpoint entries (commented out by default)
    for i in $(echo $DIRLIST | tr "," "\n")
    do
	echo "#$ZPOOL/$ROOTFS/$i /$i zfs defaults,x-systemd.after=zfs.target 0 0" >> $INSTROOT/etc/fstab
    done
    unset i
    #echo "Finished generating entries in ${INSTROOT}/etc/fstab"

    echo "Generating entries for ${INSTROOT}/etc/crypttab..."
    cat <<EOF > $INSTROOT/etc/crypttab
# LUKS device containing root filesystem
$LUKS_LABEL UUID=$LUKS_UUID none luks

# LUKS encrypted devices of ZFS member vdevs
EOF

    ROOTCRYPT_DIR=$INSTROOT/root/crypt
    mkdir -p $ROOTCRYPT_DIR/headers
    chmod -R 700 $INSTROOT/root

    echo "Backing up LUKS headers in ${ROOTCRYPT_DIR}/headers..."
    cryptsetup luksHeaderBackup $LUKS_PARTDEV \
	       --header-backup-file $ROOTCRYPT_DIR/headers/$LUKS_LABEL

    if [ ! -z "$KEYFILE" -a -e "$KEYFILE" ]
    then
	chmod 400 $KEYFILE
	cp $KEYFILE $ROOTCRYPT_DIR
	KEYFILENAME=$(basename $KEYFILE)

	for i in $(echo "$DEVLIST" | tr "," "\n")
	do
	    device=$(echo $i|cut -d : -f1)
	    label=$(echo $i|cut -d : -f2)
	    if [ -b "$device" -a ! -z "$label" ]
	    then
	       uuid=$(fsuuid $device)
	       
	       # creating crypttab entries for LUKS encrypted devices of ZFS member vdevs
	       echo "$label UUID=${uuid} /root/crypt/${KEYFILENAME} luks" >> $INSTROOT/etc/crypttab

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

    #echo "Finished generating entries for ${INSTROOT}/etc/crypttab"
    chmod 400 $ROOTCRYPT_DIR/headers/*
    #echo "Finished backing up LUKS headers in ${ROOTCRYPT_DIR}/headers"
}

LUKS_LABEL=crypt_root
ROOTFS=system
DIRLIST="home,var,gnu"
INSTROOT=/mnt/instroot
[ -e ./.lastrun ] && . ./.lastrun
unset ROOT_DRIVE

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
Install and configure necessary ZFS dependencies only

-z ZPOOL
ZFS pool name for system directories and swap device

-K FILENAME
Generate new keyfile

-k KEYFILE
Keyfile used to decrypt other encrypted devices (i.e. ZFS pool members)

-c DEVLIST
Coma separeted list of colon separated pairs of other encrypted devices (i.e. members of ZFS pool), and their repsective LUKS labels.
E.g. /dev/sdb:foo,/dev/sdc:bar,/dev/sdd:baz

These mappings are used to:
 a) unlock these devices before importing ZFS pools
 b) create crypttab entries for automatic unlocking during boot

Specifying a keyfile is necessary for this feature!

-r NAME
Name of the system root dataset in the ZFS pool (default $ROOTFS)

-d DIRLIST
Coma separated list of root directories to mount as ZFS datasets (default $DIRLIST)

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

while getopts 'l:m:Zz:K:k:c:d:r:s:h' opt
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
	    echo "Finished installing all package dependencies!"
	    exit 0
	    ;;
	z)
	    ZPOOL=$OPTARG
	    ;;
	K)
	    NEW_KEYFILE=$OPTARG
	    echo "Generating new key-file..."
	    dd if=/dev/random of=$NEW_KEYFILE bs=1024 count=4
	    chmod 0400 $NEW_KEYFILE
	    echo "Finished generating new key-file."
	    exit 0
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

cat <<EOF > .lastrun
ROOT_DRIVE=$ROOT_DRIVE
LUKS_LABEL=$LUKS_LABEL
KEYFILE=$KEYFILE
DEVLIST=$DEVLIST
ZPOOL=$ZPOOL
ROOTFS=$ROOTFS
DIRLIST=$DIRLIST
SWAPSIZE=$SWAPSIZE
INSTROOT=$INSTROOT
EOF

install_deps_base
init_parts $ROOT_DRIVE
BOOT_PARTDEV="${ROOT_DRIVE}1"
LUKS_PARTDEV="${ROOT_DRIVE}2"
init_cryptroot $LUKS_PARTDEV $LUKS_LABEL

if [ ! -z "$ZPOOL" ]
then
    if [ ! -z "$KEYFILE" ]
    then
	init_cryptdevs "$KEYFILE" "$DEVLIST"
    fi
    install_deps_zfs
    init_zfsroot $ZPOOL $ROOTFS $SWAPSIZE "$DIRLIST"
    init_instroot_zfs $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL $ZPOOL $ROOTFS "$KEYFILE" "$DEVLIST" "$DIRLIST"
else
    init_lvmroot $LUKS_LABEL $SWAPSIZE
    init_instroot_lvm $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL
fi

echo "Finished setting up installation root $INSTROOT"
