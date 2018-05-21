#!/bin/sh

[ -e ./.lastrun ] && . ./.lastrun

usage () {
    cat <<EOF

USAGE:

$0 [OPTIONS]

Destroys installation root folder set up previously by init_instroot script.
By default uses options from variables defined in .lastrun

Valid options are:

-m PATH
Install root mountpoint ${INSTROOT:+(default $INSTROOT)}

-l LABEL
LUKS encrypted root device name ${LUKS_LABEL:+(default $LUKS_LABEL)}

-d DEVICE
Device with LUKS encrypted root ${ROOT_DEV:+(default $ROOT_DEV)}

-b DEVICE
Device for boot partition ${BOOT_DEV:+(default $BOOT_DEV)}

-z ZPOOL
ZFS pool name for system directories and swap device ${ZPOOL:+(default $ZPOOL)}

-r NAME
Name of the system root dataset in the ZFS pool ${ROOTFS:+(default $ROOTFS)}

-c DEVLIST
Coma separeted list of other opened LUKS device labels. ${DEVLIST:+(default $DEVLIST)}

-S
Swap file has been used instead of LVM or ZFS volume ${SWAPFILES:+(default $SWAPFILES)}

-h
This usage help...

EOF
}

while getopts 'l:m:d:z:c:d:b:r:Sh' opt
do
    case $opt in
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	m)
	    INSTROOT=$OPTARG
	    ;;
	d)
	    ROOT_DEV=$OPTARG
	    ;;
	b)
	    BOOT_DEV=$OPTARG
	    ;;
	z)
	    ZPOOL=$OPTARG
	    ;;
	c)
	    DEVLIST=$OPTARG
	    ;;
	r)
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

if [ -z "$ROOT_DEV" ]
then
    echo "ERROR: Root device is not specified!"
    exit 1
fi

if [ -z "$LUKS_LABEL" ]
then
    echo "ERROR: LUKS label is not specified!"
    exit 1
fi

if [ -z "$INSTROOT" ]
then
    echo "ERROR: Mounted root directory is not specified."
    exit 1
fi

if [ $(id -u) -ne 0 ]
then
    echo "ERROR: This script must be run as root!" >&2
    exit 1
fi

umount $INSTROOT/boot
if [ ! -z "$BOOT_DEV" ]
then
    sgdisk -Z $BOOT_DEV 2>&1 > /dev/null
    partprobe $BOOT_DEV 2>&1 > /dev/null
fi

if [ ! -z "$ZPOOL" ]
then
    zfs destroy -r $ZPOOL/$ROOTFS
    zpool export $ZPOOL

    umount $INSTROOT

    for i in $(echo "$DEVLIST" | tr "," "\n")
    do
	device=$(echo $i|cut -d : -f1)
	label=$(echo $i|cut -d : -f2)

	cryptsetup luksClose $label;
    done
elif [ "${SWAPFILES:-0}" -gt 0 ]
then
    umount $INSTROOT
else
    umount $INSTROOT
    vgremove -f ${LUKS_LABEL}_vg
fi

if [ ! -z "$ROOT_DEV" ]
then
    cryptsetup luksClose $LUKS_LABEL
    sgdisk -Z $ROOT_DEV 2>&1 > /dev/null
    partprobe $ROOT_DEV 2>&1 >/dev/null
fi

if [ -d $INSTROOT -a ! $(rmdir $INSTROOT) ]
then
    read -p "Directory $INSTROOT is not empty. Would you still like to remove it? [y/N]" delinstroot
    case $delinstroot in
	[yY])
	    echo "Removing directory $INSTROOT with its content..."
	    rm -rf $INSTROOT
	    ;;
	*)
	    echo "Skipped removing $INSTROOT directory."
	    ;;
    esac
fi

echo "Finished destroying initialized root structure: $INSTROOT"
