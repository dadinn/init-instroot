#!/bin/sh

[ -e ./.lastrun ] && . ./.lastrun

usage () {
    cat <<EOF

USAGE:

$0 [OPTIONS]

Unmounts and destroys installation root directory, set up previously by init_instroot.sh script. Unmounts boot partition, swaps off swapfiles or LVM/ZFS swap devices, destroys LUKS devices, and zapps all device partitions used. By default uses options from variables defined in .lastrun file.

Valid options are:

-t PATH
Install root mountpoint ${TARGET:+(default $TARGET)}

-l LABEL
LUKS encrypted root device name ${LUKS_LABEL:+(default $LUKS_LABEL)}

-r DEVICE
Device with LUKS encrypted root ${ROOTDEV:+(default $ROOTDEV)}

-b DEVICE
Device for boot partition ${BOOTDEV:+(default $BOOTDEV)}

-z ZPOOL
ZFS pool name for system directories and swap device ${ZPOOL:+(default $ZPOOL)}

-f NAME
Name of the system root dataset in the ZFS pool ${ROOTFS:+(default $ROOTFS)}

-c DEVLIST
Coma separeted list of other opened LUKS device labels. ${DEVLIST:+(default $DEVLIST)}

-S
Swap file has been used instead of LVM or ZFS volume ${SWAPFILES:+(default $SWAPFILES)}

-h
This usage help...

EOF
}

while getopts 'l:t:r:b:z:c:d:f:Sh' opt
do
    case $opt in
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	t)
	    TARGET=$OPTARG
	    ;;
	r)
	    ROOTDEV=$OPTARG
	    ;;
	b)
	    BOOTDEV=$OPTARG
	    ;;
	z)
	    ZPOOL=$OPTARG
	    ;;
	c)
	    DEVLIST=$OPTARG
	    ;;
	f)
	    ROOTFS=$OPTARG
	    ;;
	S)
	    SWAPFILES=$OPTARG
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

if [ -z "$ROOTDEV" ]
then
    echo "ERROR: Root device is not specified!"
    exit 1
fi

if [ -z "$LUKS_LABEL" ]
then
    echo "ERROR: LUKS label is not specified!"
    exit 1
fi

if [ -z "$TARGET" ]
then
    echo "ERROR: Mounted root directory is not specified."
    exit 1
fi

if [ $(id -u) -ne 0 ]
then
    echo "ERROR: This script must be run as root!" >&2
    exit 1
fi

umount $TARGET/boot
if [ ! -z "$BOOTDEV" ]
then
    sgdisk -Z $BOOTDEV 2>&1 > /dev/null
    partprobe $BOOTDEV 2>&1 > /dev/null
fi

if [ ! -z "$ZPOOL" ]
then
    zfs destroy -r $ZPOOL/$ROOTFS
    zpool export $ZPOOL

    umount $TARGET

    for i in $(echo "$DEVLIST" | tr "," "\n")
    do
	device=$(echo $i|cut -d : -f1)
	label=$(echo $i|cut -d : -f2)

	cryptsetup luksClose $label;
    done
elif [ "${SWAPFILES:-0}" -gt 0 ]
then
    umount $TARGET
else
    umount $TARGET
    vgremove -f ${LUKS_LABEL}_vg
fi

if [ ! -z "$ROOTDEV" ]
then
    cryptsetup luksClose $LUKS_LABEL
    sgdisk -Z $ROOTDEV 2>&1 > /dev/null
    partprobe $ROOTDEV 2>&1 >/dev/null
fi

if [ -d $TARGET -a ! $(rmdir $TARGET) ]
then
    read -p "Directory $TARGET is not empty. Would you still like to remove it? [y/N]" delinstroot
    case $delinstroot in
	[yY])
	    echo "Removing directory $TARGET with its content..."
	    rm -rf $TARGET
	    ;;
	*)
	    echo "Skipped removing $TARGET directory."
	    ;;
    esac
fi

echo "Finished destroying initialized root structure: $TARGET"
