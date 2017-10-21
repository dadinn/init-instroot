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

while getopts 'l:m:z:c:d:r:Sh' opt
do
    case $opt in
	l)
	    LUKS_LABEL=$OPTARG
	    ;;
	m)
	    INSTROOT=$OPTARG
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

if [ ! -z "$ZPOOL" ]
then
    swapoff /dev/zvol/$ZPOOL/$ROOTFS/swap
    zfs destroy -r $ZPOOL/$ROOTFS
    zpool export $ZPOOL

    umount $INSTROOT/boot
    umount $INSTROOT

    for i in $(echo "$DEVLIST" | tr "," "\n")
    do
	device=$(echo $i|cut -d : -f1)
	label=$(echo $i|cut -d : -f2)

	cryptsetup luksClose $label;
    done
elif [ "${SWAPFILES:-0}" -gt 0 ]
then
    swapoff $INSTROOT/var/swap/*
    umount $INSTROOT/boot
    umount $INSTROOT
else
    VG_NAME=${LUKS_LABEL}_vg
    swapoff /dev/mapper/${VG_NAME}-swap
    umount $INSTROOT/boot
    umount $INSTROOT
    vgremove -f $VG_NAME
fi

cryptsetup luksClose $LUKS_LABEL
sgdisk -Z $ROOT_DRIVE &> /dev/null
if ! rmdir $INSTROOT
then
    read -p "Directory $INSTROOT is not empty. Would you still like to remove it? [y/N]" delinstroot
    case $delinstroot in
	[yY])
	    echo "$Removing directory $INSTROOT with its content..."
	    rm -rf $INSTROOT
	    ;;
	*)
	    echo "Skipped removing $INSTROOT directory."
	    ;;
    esac
fi

echo "Finished destroying initialized root structure: $INSTROOT"
