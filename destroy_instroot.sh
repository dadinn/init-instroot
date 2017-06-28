#!/bin/sh


while getopts 'l:m:Zz:k:c:d:r:s:h' opt
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
	k)
	    KEYFILE=$OPTARG
	    ;;
	c)
	    DEVLIST=$OPTARG
	    ;;
	r)
	    ROOTFS=$OPTARG
	    ;;
	d)
	    DIRLIST=$OPTARG
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

zfs destroy -r $ZPOOL/$SYSTEMFS
umount $INSTROOT/boot
umount $INSTROOT
cryptsetup luksClose $LUKS_LABEL
for i in $(echo "$DEVLIST" | tr "," "\n")
do cryptsetup luksClose $i; done
sgdisk -Z $ROOT_DRIVE
