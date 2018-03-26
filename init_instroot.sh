#!/bin/sh

ERROR_EXIT() {
    local MESSAGE="$1"
    local CODE=${2:-1}
    echo "ERROR: $MESSAGE" >&2
    exit $CODE
}

partuuid () {
    if [ ! "$#" -eq 2 -o ! -b "$1" -o ! "$2" -lt 3 ]
    then
	ERROR_EXIT "called partuuid with args: $@"
    fi

    sgdisk -i $2 $1 | grep -E '^Partition unique GUID:' | sed -e 's;^[^:]*: \([[:alnum:]-]*\)$;\1;' | tr '[:upper:]' '[:lower:]'
}

fsuuid () {
    [ $# -eq 1 ] || ERROR_EXIT "called fsuuid with $# args: $@"

    blkid -s UUID -o value $1
}

install_deps_base () {
    if ! type sgdisk partprobe cryptsetup pv pvcreate vgcreate lvcreate 2>&1 > /dev/null
    then
	if [ ! -e /etc/debian_version ]
	then
	    ERROR_EXIT "Necessary binaries are missing: sgdisk partprobe cryptsetup pv pvcreate vgcreate lvcreate!"
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
	    ERROR_EXIT "Necessary binaries are missing! Please make sure ZFS kernel modules are loaded and CLI commands *zpool* and *zfs* are available."
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
		ERROR_EXIT "Debian version $RELEASE is not supported!"
		;;
	esac
	touch .deps_zfs
    fi
}

init_parts_bios () {
    if [ $# -eq 1 ]
    then
	local ROOT_DEV=$1
    else
	ERROR_EXIT "called init_parts_bios with $# args: $@"
    fi

    echo "Setting up partitions..."
    sgdisk $ROOT_DEV -Z \
	   -n 1:0:+2M -n 2:0:+500M -N 3 \
	   -t 1:ef02 -t 2:8300 -t 3:8300 -s 2>&1 > /dev/null
    partprobe $ROOT_DEV 2>&1 > /dev/null
    echo "Finished setting up partitions on: $ROOT_DEV"
}

init_parts_efi () {
    if [ $# -eq 1 ]
    then
	local ROOT_DEV=$1
    else
	ERROR_EXIT "called init_parts_efi with $# args: $@"
    fi

    echo "Setting up partitions..."
    sgdisk $ROOT_DEV -Z \
	   -n 1:0:+500M -N 2 \
	   -t 1:ef00 -t 2:8300 -s 2>&1 > /dev/null
    partprobe $ROOT_DEV 2>&1 > /dev/null
    echo "Finished setting up partitions on: $ROOT_DEV"
}

init_cryptroot () {
    if [ $# -eq 2 ]
    then
	local LUKS_PARTDEV=$1
	local LUKS_LABEL=$2
    else
	ERROR_EXIT "called init_cryptroot with $# args: $@"
    fi

    echo
    echo "Formatting $LUKS_PARTDEV to be used as LUKS device..."
    cryptsetup luksFormat $LUKS_PARTDEV
    echo "Finished formatting device $LUKS_PARTDEV for LUKS encryption!"
    echo
    echo "Opening LUKS device..."
    if ! cryptsetup luksOpen $LUKS_PARTDEV $LUKS_LABEL
    then
	ERROR_EXIT "failed to open LUKS device: $LUKS_LABEL"
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
	ERROR_EXIT "called init_cryptdevs with $# args: $@"
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
    if [ $# -eq 5 ]
    then
	local ZPOOL=$1
	local FSNAME=$2
	local SWAPSIZE=$3
	local SWAPFILES=$4
	local DIRLIST=$5

    else
	ERROR_EXIT "called init_zfsroot with $# args: $@"
    fi

    SYSTEMFS=$ZPOOL/$FSNAME

    if ! zpool list $ZPOOL > /dev/null 2>&1
    then
	if ! zpool import $ZPOOL > /dev/null
	then
	    ERROR_EXIT "could not find or import ZFS pool: $ZPOOL"
	fi
    fi

    if zfs list $SYSTEMFS > /dev/null 2>&1
    then
	echo "WARNING: $SYSTEMFS dataset already exist!" >&2
	read -p "Would you like to destroy and recreate dataset? [y/N]" recreate
	case $recreate in
	    [yY])
		zfs destroy -r $SYSTEMFS
		;;
	    *)
		echo "Skipped recreating ZFS root dataset. Exiting..."
		exit 1
		;;
	esac
    fi

    # system dataset should be used for mountpoint inheritance only, and never automount
    zfs create -o compression=lz4 -o canmount=off $SYSTEMFS
    for i in $(echo $DIRLIST | tr "," "\n")
    do zfs create $SYSTEMFS/$i; done

    if [ "$SWAPFILES" -eq 0 ]
    then
	echo "Creating ZFS volume for swap device..."
	zfs create -V $SWAPSIZE \
	    -o sync=always \
	    -o primarycache=metadata \
	    -o logbias=throughput \
	    $SYSTEMFS/swap
	mkswap /dev/zvol/$SYSTEMFS/swap 2>&1 > /dev/null
	swapon /dev/zvol/$SYSTEMFS/swap 2>&1 > /dev/null
	echo "Finished setting up ZFS pool: $ZPOOL"
    fi
}

init_instroot_lvm () {
    if [ $# -eq 5 ]
    then
	local INSTROOT=$1
	local BOOT_PARTDEV=$2
	local LUKS_PARTDEV=$3
	local LUKS_LABEL=$4
	local SWAP_SIZE=$5

	[ ! -e $INSTROOT ] || ERROR_EXIT "$INSTROOT already exists"
	[ -b $BOOT_PARTDEV ] || ERROR_EXIT "$BOOT_PARTDEV has to be a block device"
	[ -b $LUKS_PARTDEV ] || ERROR_EXIT "$LUKS_PARTDEV has to be a block device"
	[ ! -z $(echo $LUKS_LABEL | grep -E '^[[:alnum:]_]+$') ] || ERROR_EXIT "invalid LUKS label: $LUKS_LABEL"
    else
	ERROR_EXIT "called init_instroot_lvm with $# args: $@"
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
	ERROR_EXIT "$LV_ROOT failed to mount as $INSTROOT!"
    fi

    mkdir $INSTROOT/boot
    mkdir $INSTROOT/etc
    mkdir $INSTROOT/root
    chmod 700 $INSTROOT/root

    echo "Formatting partition $BOOT_PARTDEV with ext4 to be used as /boot..."
    if ! mount $BOOT_PARTDEV /$INSTROOT/boot
    then
	ERROR_EXIT "$BOOT_PARTDEV failed to mount as $INSTROOT/boot!"
    fi

    echo "Formatting $LV_SWAP to be used as swap space..."
    mkswap $LV_SWAP 2>&1 > /dev/null
    if ! swapon $LV_SWAP
    then
	ERROR_EXIT "$LV_SWAP failed to swap on!"
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
    if [ $# -eq 11 ]
    then
	local INSTROOT=$1
	local BOOT_PARTDEV=$2
	local LUKS_PARTDEV=$3
	local LUKS_LABEL=$4
	local KEYFILE=$5
	local DEVLIST=$6
	local ZPOOL=$7
	local ROOTFS=$8
	local SWAP_SIZE=$9
	local SWAPFILES=$10
	local DIRLIST=$11

	[ ! -e $INSTROOT ] || ERROR_EXIT "target $INSTROOT already exists!"
	[ -b $BOOT_PARTDEV ] || ERROR_EXIT "cannot find boot partition device $BOOT_PARTDEV"
	[ -b $LUKS_PARTDEV ] || ERROR_EXIT "cannot find root partition device $LUKS_PARTDEV"
	[ -b /dev/mapper/$LUKS_LABEL ] || ERROR_EXIT "cannot find LUKS device $LUKS_LABEL"

	if ! zpool list $ZPOOL 2>&1 > /dev/null
	then
	    ERROR_EXIT "zpool $ZPOOL not available"
	fi

	if ! zfs list $ZPOOL/$ROOTFS 2>&1 > /dev/null
	then
	    ERROR_EXIT "ZFS dataset $ZPOOL/ROOTFS does not exist"
	fi
    else
	ERROR_EXIT "called init_instroot_zfs with $# args: $@"
    fi

    mkdir -p $INSTROOT
    echo "Formatting LUKS device $LUKS_LABEL with ext4 to be used as root filesystem..."
    mkfs.ext4 -q /dev/mapper/$LUKS_LABEL 2>&1 > /dev/null
    if ! mount /dev/mapper/$LUKS_LABEL $INSTROOT
    then
	 ERROR_EXIT "Failed to format and mount LUKS device $LUKS_LABEL as $INSTROOT!"
    fi

    mkdir $INSTROOT/boot
    mkdir $INSTROOT/etc
    mkdir $INSTROOT/root
    chmod 700 $INSTROOT/root

    echo "Formatting partition $BOOT_PARTDEV with ext4 to be used as /boot..."
    mkfs.ext4 -q -m 0 -j $BOOT_PARTDEV 2>&1 > /dev/null
    if ! mount $BOOT_PARTDEV /$INSTROOT/boot
    then
	ERROR_EXIT "$BOOT_PARTDEV failed to mount as $INSTROOT/boot!"
    fi

    echo "Mounting all ZFS root directories..."
    zfs set mountpoint=$INSTROOT $ZPOOL/$ROOTFS

    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    ROOT_UUID=$(fsuuid /dev/mapper/$LUKS_LABEL)
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat > $INSTROOT/etc/fstab <<EOF
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2
EOF
    if [ "$SWAPFILES" -gt 0 ]
    then
	mkdir $INSTROOT/root/swap
	chmod 700 $INSTROOT/root/swap
	cat >> $INSTROOT/etc/fstab <<EOF

# swapfiles
EOF

	SWAPSIZE_NUM=$(echo $SWAP_SIZE|sed -E 's;([0-9]+)[KMGT]?;\1;')
	SWAPSIZE_KMGT=$(echo $SWAP_SIZE|sed -E 's;[0-9]+([KMGT]?);\1;')
	SWAPFILE_SIZE="$(($SWAPSIZE_NUM / $SWAPFILES))$SWAPSIZE_KMGT"

	for count in $(seq 1 $SWAPFILES)
	do
	    SWAPFILE_PATH="/root/swap/file$(printf %04d $count)_${SWAPFILE_SIZE}"
	    SWAPFILE=${INSTROOT}${SWAPFILE_PATH}
	    echo "Allocating $SWAPFILE_SIZE of swap space in $SWAPFILE..."
	    pv -Ss $SWAPFILE_SIZE < /dev/zero > $SWAPFILE
	    chmod 600 $SWAPFILE
	    mkswap $SWAPFILE 2>&1 > /dev/null
	    if swapon $SWAPFILE
	    then
		echo "$SWAPFILE_PATH none swap sw 0 0" >> $INSTROOT/etc/fstab
	    else
		echo "WARNING: $SWAPFILE failed to swap on!" >&2
	    fi
	done
    else
	SWAP_UUID=$(fsuuid /dev/zvol/$ZPOOL/$ROOTFS/swap)
	cat >> $INSTROOT/etc/fstab <<EOF
UUID=$SWAP_UUID none swap sw,x-systemd.after=zfs.target 0 0
EOF
    fi

    cat >> $INSTROOT/etc/fstab <<EOF

# systemd specific legacy mounts of ZFS datasets
EOF

    for i in $(echo $DIRLIST | tr "," "\n")
    do
	cat >> $INSTROOT/etc/fstab <<EOF
# $ZPOOL/$ROOTFS/$i /$i zfs defaults,x-systemd.after=zfs.target 0 0
EOF
    done
    unset i

    echo "Generating entries for ${INSTROOT}/etc/crypttab..."
    cat > $INSTROOT/etc/crypttab <<EOF
# LUKS device containing root filesystem
$LUKS_LABEL UUID=$LUKS_UUID none luks

# LUKS encrypted devices of ZFS member vdevs
EOF

    ROOTCRYPT_DIR=$INSTROOT/root/crypt
    mkdir -p $ROOTCRYPT_DIR/headers
    chmod -R 700 $ROOTCRYPT_DIR

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
	unset device label uuid i
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
	ERROR_EXIT "called init_instroot_swapfile with $# args: $@"
    fi

    echo "Setting up installation root with swapfile for swap space..."

    mkdir -p $INSTROOT
    echo "Formatting LUKS device $LUKS_LABEL with ext4 to be used as root filesystem..."
    mkfs.ext4 -q /dev/mapper/$LUKS_LABEL 2>&1 > /dev/null
    if ! mount /dev/mapper/$LUKS_LABEL $INSTROOT
    then
	ERROR_EXIT "Failed to format and mount LUKS device $LUKS_LABEL as $INSTROOT!"
    fi

    mkdir $INSTROOT/boot
    mkdir $INSTROOT/etc
    mkdir -p $INSTROOT/root/swap
    chmod -R 700 $INSTROOT/root

    echo "Formatting partition $BOOT_PARTDEV with ext4 to be used as /boot..."
    mkfs.ext4 -q -m 0 -j $BOOT_PARTDEV 2>&1 > /dev/null
    if ! mount $BOOT_PARTDEV /$INSTROOT/boot
    then
	ERROR_EXIT "Failed to format and mount $BOOT_PARTDEV as $INSTROOT/boot!"
    fi
    
    BOOT_UUID=$(fsuuid $BOOT_PARTDEV)
    LUKS_UUID=$(fsuuid $LUKS_PARTDEV)
    ROOT_UUID=$(fsuuid /dev/mapper/$LUKS_LABEL)

    echo "Generating entries for ${INSTROOT}/etc/fstab..."
    cat <<EOF > $INSTROOT/etc/fstab
# <file system> <mountpoint> <type> <options> <dump> <pass>
UUID=$ROOT_UUID / ext4 errors=remount-ro 0 1
UUID=$BOOT_UUID /boot ext4 defaults 0 2

# swapfiles
EOF

    SWAPSIZE_NUM=$(echo $SWAP_SIZE|sed -E 's;([0-9]+)[KMGT]?;\1;')
    SWAPSIZE_KMGT=$(echo $SWAP_SIZE|sed -E 's;[0-9]+([KMGT]?);\1;')
    SWAPFILE_SIZE="$(($SWAPSIZE_NUM / $SWAPFILES))$SWAPSIZE_KMGT"

    for count in $(seq 1 $SWAPFILES)
    do
	SWAPFILE_PATH="/root/swap/file$(printf %04d $count)_${SWAPFILE_SIZE}"
	SWAPFILE=${INSTROOT}${SWAPFILE_PATH}
	echo "Allocating $SWAPFILE_SIZE of swap space in $SWAPFILE..."
	pv -Ss $SWAPFILE_SIZE < /dev/zero > $SWAPFILE
	chmod 600 $SWAPFILE
	mkswap $SWAPFILE 2>&1 > /dev/null
	if swapon $SWAPFILE
	then
	    echo "$SWAPFILE_PATH none swap sw 0 0" >> $INSTROOT/etc/fstab
	else
	    echo "WARNING: $SWAPFILE failed to swap on!" >&2
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
UEFI_BOOT=0
SWAPFILES=0
PREINIT_DEPENDENCIES=0

 # make sure root device can only be passed as CLI argument
unset ROOT_DEV
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
Generate new keyfile with the given FILENAME

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

-E
Use UEFI boot partitions instead of BIOS (default)

-h
This usage help...

EOF
}

while getopts 'l:m:Zz:K:k:c:d:r:S:s:Eh' opt
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
	    NEW_KEYFILE=$(basename $OPTARG)

	    if [ -e "$NEW_KEYFILE" ]
	    then
		ERROR_EXIT "$NEW_KEYFILE already exists!"
	    fi
	    ;;
	k)
	    KEYFILE=$OPTARG

	    if [ ! -z "$KEYFILE" -a ! -e "$KEYFILE" ]
	    then
		ERROR_EXIT "keyfile $KEYFILE is not found!"
	    fi
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

	    if [ -z "$(echo $SWAPSIZE | grep -E '^[0-9]+[KMGT]?$')" ]
	    then
		ERROR_EXIT "swap size has to be specified with KGMT suffixes"
	    fi
	    ;;
	E)
	    UEFI_BOOT=1
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

if [ ! -z "$NEW_KEYFILE" ]
then
    echo "Generating new key-file $NEW_KEYFILE..."
    dd if=/dev/random of=$NEW_KEYFILE bs=1024 count=4
    chmod 0400 $NEW_KEYFILE
    echo "Finished generating new key-file $NEW_KEYFILE."
    exit 0
fi

if [ $(id -u) -ne 0 ]
then
    ERROR_EXIT "This script must be run as root!"
fi

if [ $PREINIT_DEPENDENCIES -eq 1 ]
then
    install_deps_zfs
    echo "Finished installing all package dependencies!"
    exit 0
fi

if [ "$#" -eq 1 -a -b "$1" ]
then
    ROOT_DEV=$1
else
    ERROR_EXIT "Block device must be specified for root filesystem!"
fi

if [ -z "$SWAPSIZE" ]
then
    ERROR_EXIT "Swap size must be specified!"
fi

if [ ! -z "$DEVLIST" -a -z "$KEYFILE" ]
then
    ERROR_EXIT "Keyfile must be specified for encrypted devices!"
fi

cat <<EOF > .lastrun
ROOT_DEV=$ROOT_DEV
UEFI_BOOT=$UEFI_BOOT
LUKS_LABEL=$LUKS_LABEL
KEYFILE=$KEYFILE
DEVLIST=$DEVLIST
ZPOOL=$ZPOOL
ROOTFS=$ROOTFS
DIRLIST=$DIRLIST
SWAPSIZE=$SWAPSIZE
SWAPFILES=$SWAPFILES
INSTROOT=$INSTROOT
EOF

install_deps_base

if [ "$UEFI_BOOT" -eq 0 ]
then
    init_parts_bios $ROOT_DEV
    BOOT_PARTDEV="${ROOT_DEV}2"
    LUKS_PARTDEV="${ROOT_DEV}3"
    mkfs.ext4 -q -m 0 -j $BOOT_PARTDEV 2>&1 > /dev/null
else
    init_parts_efi $ROOT_DEV
    BOOT_PARTDEV="${ROOT_DEV}1"
    LUKS_PARTDEV="${ROOT_DEV}2"
    mkfs.fat -F32 $BOOT_PARTDEV 2>&1 > /dev/null
fi

init_cryptroot $LUKS_PARTDEV $LUKS_LABEL

if [ ! -z "$ZPOOL" ]
then
    if [ ! -z "$KEYFILE" ]
    then
	init_cryptdevs "$KEYFILE" "$DEVLIST"
    fi
    install_deps_zfs
    init_zfsroot $ZPOOL $ROOTFS $SWAPSIZE $SWAPFILES "$DIRLIST"
    init_instroot_zfs $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL "$KEYFILE" "$DEVLIST" $ZPOOL $ROOTFS $SWAPSIZE $SWAPFILES "$DIRLIST"
else
    if [ "$SWAPFILES" -gt 0 ]
    then
	init_instroot_swapfile $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL $SWAPSIZE $SWAPFILES
    else
	init_instroot_lvm $INSTROOT $BOOT_PARTDEV $LUKS_PARTDEV $LUKS_LABEL $SWAPSIZE
    fi
fi

cp .lastrun $INSTROOT/CONFIG_ME
echo "Finished setting up installation root $INSTROOT"
