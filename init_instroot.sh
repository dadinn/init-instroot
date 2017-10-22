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
    if ! type sgdisk partprobe cryptsetup pv pvcreate vgcreate lvcreate 2>&1 > /dev/null
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
	local ROOT_DRIVE=$1
    else
	echo "ERROR: called init_parts with $# args: $@" >&2
	exit 1
    fi

    echo "Setting up partitions..."
    sgdisk -Z $ROOT_DRIVE 2>&1 > /dev/null
    sgdisk $ROOT_DRIVE -n 1:0:+500M -N 2 -t 1:ef02 2>&1 > /dev/null
    partprobe $ROOT_DRIVE 2>&1 > /dev/null
    echo "Finished setting up partitions on: $ROOT_DRIVE"
}

init_cryptroot () {
    if [ $# -eq 2 ]
    then
	local LUKS_PARTDEV=$1
	local LUKS_LABEL=$2
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

    cat <<EOF
Finished setting up LUKS device: $LUKS_LABEL

EOF
}

init_cryptdevs () {
    if [ $# -eq 2 ]
    then
	local KEYFILE=$1
	local DEVLIST=$2
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
    unset device label i
}

init_zfsroot () {
    if [ $# -eq 4 ]
    then
	local ZPOOL=$1
	local FSNAME=$2
	local SWAPSIZE=$3
	local DIRLIST=$4

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
    mkswap /dev/zvol/$SYSTEMFS/swap 2>&1 > /dev/null
    swapon /dev/zvol/$SYSTEMFS/swap 2>&1 > /dev/null
    echo "Finished setting up ZFS pool: $ZPOOL"
}

init_instroot_lvm () {
    if [ $# -eq 5 ]
    then
	local INSTROOT=$1
	local BOOT_PARTDEV=$2
	local LUKS_PARTDEV=$3
	local LUKS_LABEL=$4
	local SWAP_SIZE=$5

	[ ! -e $INSTROOT ] || (echo "ERROR: $INSTROOT already exists" && exit 1) >&2
	[ -b $BOOT_PARTDEV ] || (echo "ERROR: $BOOT_PARTDEV has to be a block device" && exit 1) >&2
	[ -b $LUKS_PARTDEV ] || (echo "ERROR: $LUKS_PARTDEV has to be a block device" && exit 1) >&2
	[ ! -z $(echo $LUKS_LABEL | grep -E '^[[:alnum:]_]+$') ] || (echo "ERROR: invalid LUKS label: $LUKS_LABEL" && exit 1) >&2
    else
	echo "ERROR: called init_instroot_lvm with $# args: $@" >&2
	exit 1
    fi

    VG_NAME=${LUKS_LABEL}_vg

    echo "Setting up LVM with volumes for root and swap filesystems..."
    pvcreate  /dev/mapper/$LUKS_LABEL
    vgcreate $VG_NAME /dev/mapper/$LUKS_LABEL
    lvcreate -L $SWAP_SIZE $VG_NAME -n swap
    lvcreate -l 100%FREE $VG_NAME -n root

    LV_ROOT=/dev/mapper/${VG_NAME}-root
    LV_SWAP=/dev/mapper/${VG_NAME}-swap

    mkdir -p $INSTROOT
    echo "Formatting LVM logical volume $LV_ROOT with ext4 to be used as root filesystem..."
    mkfs.ext4 -q $LV_ROOT 2>&1 > /dev/null
    if ! mount $LV_ROOT $INSTROOT
    then
	echo "ERROR: $LV_ROOT failed to mount as $INSTROOT!" >&2
	exit 1
    fi

    mkdir $INSTROOT/boot
    mkdir $INSTROOT/etc
    mkdir $INSTROOT/root
    chmod 700 $INSTROOT/root

    echo "Formatting partition $BOOT_PARTDEV with ext4 to be used as /boot..."
    mkfs.ext4 -qF -m 0 -j $BOOT_PARTDEV 2>&1 > /dev/null
    if ! mount $BOOT_PARTDEV /$INSTROOT/boot
    then
	echo "ERROR: $BOOT_PARTDEV failed to mount as $INSTROOT/boot!" >&2
	exit 1
    fi

    echo "Formatting $LV_SWAP to be used as swap space..."
    mkswap $LV_SWAP 2>&1 > /dev/null
    if ! swapon $LV_SWAP
    then
	echo "ERROR: $LV_SWAP failed to swap on!" >&2
	exit 1
    fi

    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)
    ROOT_UUID=$(fsuuid $LV_ROOT)
    SWAP_UUID=$(fsuuid $LV_SWAP)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat > $INSTROOT/etc/fstab <<EOF
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2
UUID=$SWAP_UUID none swap sw 0 0

EOF

    echo "Generating entries for ${INSTROOT}/etc/crypttab..."
    cat > $INSTROOT/etc/crypttab <<EOF
# LUKS device containing root filesystem
$LUKS_LABEL UUID=$LUKS_UUID none luks

EOF

    ROOTCRYPT_DIR=$INSTROOT/root/crypt
    mkdir -p $ROOTCRYPT_DIR/headers
    chmod 700 $ROOTCRYPT_DIR/headers

    echo "Backing up LUKS headers in ${ROOTCRYPT_DIR}/headers..."
    cryptsetup luksHeaderBackup $LUKS_PARTDEV \
	       --header-backup-file $ROOTCRYPT_DIR/headers/$LUKS_LABEL

    chmod 400 $ROOTCRYPT_DIR/headers/*
}

init_instroot_zfs () {
    if [ $# -eq 9 ]
    then
	local INSTROOT=$1
	local BOOT_PARTDEV=$2
	local LUKS_PARTDEV=$3
	local LUKS_LABEL=$4
	local KEYFILE=$5
	local DEVLIST=$6
	local ZPOOL=$7
	local ROOTFS=$8
	local DIRLIST=$9

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
    echo "Formatting LUKS device $LUKS_LABEL with ext4 to be used as root filesystem..."
    if ! mkfs.ext4 /dev/mapper/$LUKS_LABEL && mount /dev/mapper/$LUKS_LABEL $INSTROOT 2>&1 > /dev/null
    then
	echo "ERROR: Failed to format and mount LUKS device $LUKS_LABEL as $INSTROOT!" >&2
	exit 1
    fi

    mkdir $INSTROOT/boot
    echo "Formatting partition $BOOT_PARTDEV with ext4 to be used as /boot..."
    if ! mkfs.ext4 -qF -m 0 -j $BOOT_PARTDEV && mount $BOOT_PARTDEV /$INSTROOT/boot 2>&1 > /dev/null
    then
	echo "ERROR: $BOOT_PARTDEV failed to mount as $INSTROOT/boot!" >&2
	exit 1
    fi

    mkdir $INSTROOT/etc
    mkdir $INSTROOT/root
    chmod 700 $INSTROOT/root

    echo "Mounting all ZFS root directories..."
    zfs set mountpoint=$INSTROOT $ZPOOL/$ROOTFS

    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    ROOT_UUID=$(fsuuid /dev/mapper/$LUKS_LABEL)
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)
    SWAP_UUID=$(fsuuid /dev/zvol/$ZPOOL/$ROOTFS/swap)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat > $INSTROOT/etc/fstab <<EOF
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2
UUID=$SWAP_UUID none swap sw,x-systemd.after=zfs.target 0 0

# systemd specific legacy mounts of ZFS datasets
EOF

    # systemd specific legacy ZFS fstab mountpoint entries (commented out by default)
    for i in $(echo $DIRLIST | tr "," "\n")
    do
	cat >> $INSTROOT/etc/fstab <<EOF
# $ZPOOL/$ROOTFS/$i /$i zfs defaults,x-systemd.after=zfs.target 0 0
EOF
    done
    unset i
    #echo "Finished generating entries in ${INSTROOT}/etc/fstab"

    echo "Generating entries for ${INSTROOT}/etc/crypttab..."
    cat > $INSTROOT/etc/crypttab <<EOF
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
	       cat >> $INSTROOT/etc/crypttab <<EOF
$label UUID=${uuid} /root/crypt/${KEYFILENAME} luks

EOF

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

init_instroot_swapfile() {
    if [ $# -eq 6 ]
    then
	local INSTROOT=$1
	local BOOT_PARTDEV=$2
	local LUKS_PARTDEV=$3
	local LUKS_LABEL=$4
	local SWAP_SIZE=$5
	local SWAPFILES=$6
    else
	echo "ERROR: called init_instroot_swapfile with $# args: $@" >&2
	exit 1
    fi

    echo "Setting up installation root with swapfile for swap space..."

    mkdir -p $INSTROOT
    echo "Formatting LUKS device $LUKS_LABEL with ext4 to be used as root filesystem..."
    if ! mkfs.ext4 /dev/mapper/$LUKS_LABEL && mount /dev/mapper/$LUKS_LABEL $INSTROOT 2>&1 > /dev/null
    then
	echo "ERROR: Failed to format and mount LUKS device $LUKS_LABEL as $INSTROOT!"
	exit 1
    fi

    mkdir $INSTROOT/boot
    echo "Formatting partition $BOOT_PARTDEV with ext4 to be used as /boot..."
    if ! mkfs.ext4 -qF -m 0 -j $BOOT_PARTDEV && mount $BOOT_PARTDEV /$INSTROOT/boot 2>&1 > /dev/null
    then
	echo "ERROR: Failed to format and mount $BOOT_PARTDEV as $INSTROOT/boot!" >&2
	exit 1
    fi

    mkdir $INSTROOT/etc
    mkdir $INSTROOT/root
    chmod 700 $INSTROOT/root
    mkdir -p $INSTROOT/var/swap
    chmod 700 $INSTROOT/var/swap
    
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)
    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    ROOT_UUID=$(fsuuid /dev/mapper/$LUKS_LABEL)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat <<EOF > $INSTROOT/etc/fstab
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2

# Swapfiles
EOF

    SWAPSIZE_NUM=$(echo $SWAP_SIZE|sed -E 's;([0-9]+)[KMGT]?;\1;')
    SWAPSIZE_KMGT=$(echo $SWAP_SIZE|sed -E 's;[0-9]+([KMGT]?);\1;')
    SWAPFILE_SIZE="$(($SWAPSIZE_NUM / $SWAPFILES))$SWAPSIZE_KMGT"

    for count in $(seq 1 $SWAPFILES)
    do
	SWAPFILE_PATH="/var/swap/file$(printf %04d $count)_${SWAPFILE_SIZE}"
	SWAPFILE=${INSTROOT}${SWAPFILE_PATH}
	echo "Allocating $SWAPFILE_SIZE of swap space in $SWAPFILE..."
	pv -Ss $SWAPSIZE < /dev/zero > $SWAPFILE
	chmod 600 $SWAPFILE
	if mkswap $SWAPFILE && swapon $SWAPFILE 2>&1 > /dev/null
	then
	    echo "$SWAPFILE_PATH none swap sw 0 0" >> $INSTROOT/etc/fstab
	else
	    echo "WARNING: Failed to allocate swap space for $SWAPFILE!" >&2
	fi
    done

    echo "Generating entries for ${INSTROOT}/etc/crypttab..."
    cat <<EOF > $INSTROOT/etc/crypttab
# LUKS device containing root filesystem
$LUKS_LABEL UUID=$LUKS_UUID none luks

EOF

    ROOTCRYPT_DIR=$INSTROOT/root/crypt
    mkdir -p $ROOTCRYPT_DIR/headers

    echo "Backing up LUKS headers in ${ROOTCRYPT_DIR}/headers..."
    cryptsetup luksHeaderBackup $LUKS_PARTDEV \
	       --header-backup-file $ROOTCRYPT_DIR/headers/$LUKS_LABEL

    # Making header backups non-writeable and readable only to root
    chmod 400 $ROOTCRYPT_DIR/headers/*
}

# DEFAULTS

LUKS_LABEL=crypt_root
ROOTFS=system
DIRLIST="home,var,gnu"
INSTROOT=/mnt/instroot
SWAPFILES=0
PREINIT_DEPENDENCIES=0

 # make sure root device can only be passed as CLI argument
unset ROOT_DRIVE
unset NEW_KEYFILE

usage () {
    cat | more <<EOF

USAGE:

$0 [OPTIONS] DEVICE

Valid options are:

-m PATH
Install root mountpoint (default $INSTROOT)

-l LABEL
LUKS encrypted device name (default $LUKS_LABEL)

-k KEYFILE
Keyfile used to decrypt other encrypted devices (i.e. ZFS pool members)

-K FILENAME
Generate new keyfile

-z ZPOOL
ZFS pool name for system directories and swap device

-Z
Install and configure necessary ZFS dependencies only, then exit

-r NAME
Name of the system root dataset in the ZFS pool (default $ROOTFS)

-d DIRLIST
Coma separated list of root directories to mount as ZFS datasets (default $DIRLIST)

-c DEVLIST
Coma separeted list of colon separated pairs of other encrypted devices
(e.g. members of ZFS pool), and their repsective LUKS labels.
(e.g. /dev/sdb:foo,/dev/sdc:bar,/dev/sdd:baz)
These device mappings are used to:
 a) unlock these devices before importing ZFS pools
 b) create crypttab entries for automatic unlocking during boot
Specifying a keyfile is necessary for this feature!

-s SWAPSIZE
Size of the total swap space to use (KMGT suffixes allowed)

-S COUNT
Number of swapfiles to use to break total swap-space up into. Swapfiles are created
in equally sized chunks. COUNT zero means to use LVM volumes instead of swapfiles.
(default $SWAPFILES)

-h
This usage help...

EOF
}

while getopts 'l:m:Zz:K:k:c:d:r:S:s:h' opt
do
    case $opt in
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	m)
	    INSTROOT=$OPTARG
	    ;;
	Z)
	    PREINIT_DEPENDENCIES=1
	    ;;
	z)
	    ZPOOL=$OPTARG
	    ;;
	K)
	    NEW_KEYFILE=$OPTARG
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
	S)
	    SWAPFILES=$OPTARG
	    ;;
	s)
	    SWAPSIZE=$OPTARG
	    ;;
	h)
            usage
            exit 0
	    ;;
	:)
	    exit 1
	    ;;
	\?)
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

if [ ! -z "$DEVLIST" -a -z "$KEYFILE" ]
then
    echo "ERROR: Encrypted device mappings cannot be specified without keyfile!" >&2
    exit 1
fi

if [ ! -z "$KEYFILE" -a ! -e "$KEYFILE" ]
then
    echo "ERROR: keyfile $KEYFILE is not found!" >&2
    exit 1
fi

if [ $(id -u) -ne 0 ]
then
    echo "ERROR: This script must be run as root!" >&2
    exit 1
fi

if [ $PREINIT_DEPENDENCIES -eq 1 ]
then
    install_deps_base
    install_deps_zfs
    echo "Finished installing all package dependencies!"
    exit 0
fi

if [ ! -z "$NEW_KEYFILE"]
then
    if [ -e "$NEW_KEYFILE" ]
    then
	echo "ERROR: $NEW_KEYFILE already exists. Cannot generate keyfile!"
	exit 1
    fi

    echo "Generating new key-file..."
    dd if=/dev/random of=$NEW_KEYFILE bs=1024 count=4
    chmod 0400 $NEW_KEYFILE
    echo "Finished generating new key-file."
    exit 0
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
SWAPFILES=$SWAPFILES
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
    init_instroot_zfs $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL "$KEYFILE" "$DEVLIST" $ZPOOL $ROOTFS "$DIRLIST"
else
    if [ "$SWAPFILES" -gt 0 ]
    then
	init_instroot_swapfile $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL $SWAPSIZE $SWAPFILES
    else
	init_instroot_lvm $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL $SWAPSIZE
    fi
fi

echo "Finished setting up installation root $INSTROOT"
