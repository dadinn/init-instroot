#!/bin/sh

function empty? { [ $# -eq 1 -a -z "$(ls -A $1)" ] }

function partuuid {
    if [ $# -eq 2 -a -b $1 -a $2 -lt 3 ]
    then
	sgdisk -i $2 $1|grep "Partition unique GUID:"|sed -e "s;^.*: \([[:alnum:]-]*\)$;\L\1;";
    else
	echo "ERROR: called partuuid with args: $@" >&2
	return -1
    fi
}

function fsuuid {
    if [ $# -eq 1 -a -b $1 ]
    then
	blkid -s UUID -o value $1
    else
	echo "ERROR: called fsuuid with args: $@" >&2
	return -1
    fi
}

function install-deps {
    if [ $# -eq 1 -a ! -z "$1" ]
    then
	RELEASE=$1
	echo "deb http://ftp.debian.org/debian $RELEASE-backports main contrib" > /etc/apt/sources.list.d/backports.list
	apt update
	apt upgrade
	apt install -y gdisk cryptsetup lvm2 debootstrap linux-headers-$(uname -r)
	apt install -y -t $RELEASE-backport zfs-dkms
    else
	echo "ERROR: called install-deps with args: $@" >&2
	return -1
    fi
}

function init-parts {
    if [ $# -eq 1 -a -b $1 ]
    then
	ROOT_DRIVE=$1
	sgdisk $ROOT_DRIVE -o -n 1:0:+500M -N 2 -t 1:ef02
    else
	echo "ERROR: called init-parts with args: $@" >&2
	return -1
    fi
}

function init-cryptroot {
    if [ $# -eq 2 -a -b /dev/disk/by-partuuid/$1 -a ! -z $(echo $2|grep -E "^[[:alnum:]_]+$") ]
    then
	LUKS_PARTUUID=$1
	LUKS_LABEL=$2

	cat <<EOF
It is recommended to overwrite LUKS device with random data. See more details at: https://gitlab.com/cryptsetup/cryptsetup/wikis/FrequentlyAskedQuestions#2-setup

WARNING: This can take quite a long time!
EOF

	read -p "Would you like to overwrite LUKS device with random data? [Y/n]" shred
	case $shred in
	    [nN])
		echo "Skipping LUKS device shredding."
		;;
	    *)
		echo "Shredding LUKS device..."
		cryptsetup luksFormat /dev/disk/by-partuuid/crypt_shred
		cryptsetup luksOpen /dev/disk/by-partuuid/crypt_shred
		dd if=/dev/zero of=/dev/mapper/crypt_shred
		cryptsetup luksClose crypt_shred
		;;
	esac

	echo "Formatting partition $LUKS_PARTUUID to be used as LUKS device..."
	cryptsetup luksFormat /dev/disk/by-partuuid/$LUKS_PARTUUID
	cryptsetup luksOpen /dev/disk/by-partuuid/$LUKS_PARTUUID $LUKS_LABEL
	echo "Finished setting up LUKS device: $LUKS_LABEL"
    else
	echo "ERROR: calling init-cryptroot with args: $@" >&2
	return -1
    fi
}

function init-zfsroot {
    if [ $# -eq 2 -a ! -z $(echo $1|grep -E "^[[:alnum:]/]+$") -a ! -z $(echo $2 | grep -E "^[0-9]+[TGMK]$") ]
    then
	SYSTEMFS=$1
	SWAPSIZE=$2
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
	    return -1
	fi
    else
	echo "ERROR: calling init-zfsroot with args: $@" >&2
	return -1
    fi
}

function valid-arch {
    if [ $# -eq 1 ]
    then
	case $1 in
	    amd64|arm64|armel|armhf|i386|mips|mipsel|powerpc|ppc64el|s390x)
		true
		;;
	    *)
		false
		;;
	esac
    fi
}

function init-instroot {
    if [ $# -eq 5 -a $(echo $1|grep -E "^[[:alpha:]]+$") -a ! -e $2 -a -b /dev/mapper/$3 -a -b /dev/disk/by-partuuid/$4 -a $(zfs list -o name|grep -E "^$5$")] 
    then
	RELEASE=$1
	INSTROOT=$2
	LUKS_LABEL=$3
	BOOT_PARTUUID=$4
	SYSTEMFS=$5

	mkdir -p $INSTROOT
	mkfs.ext4 /dev/mapper/$LUKS_LABEL
	mkfs.ext4 -m 0 -j /dev/disk/by-partuuid/$BOOT_PARTUUID
	mount /dev/mapper/$LUKS_LABEL $INSTROOT &&  mkdir $INSTROOT/boot
	mount /dev/disk/by-partuuid/$BOOT_PARTUUID /$INSTROOT/boot
	zfs set mountpoint=$INSTROOT $SYSTEMFS
    else
	echo "ERROR: calling init-instroot with args: $@" >&2
	return -1
    fi
}

function configure-system {
    if [ $# -eq 1 ]
    then
	INSTROOT=$1
	ROOT_FSUUID=$3
	BOOT_FSUUID=$2
	LUKS_LABEL=$4
	SYSTEMFS=$5
	MIRROR_URL=$6
	RELEASE=$7
	
	cat > $INSTROOT/etc/apt/apt.conf.d/norecommends <<EOF
APT::Get::Install-Recommends "false";
APT::Get::Install-Suggests "false";
EOF
	cat > /etc/apt/sources.list.d/backports.list <<EOF
deb $MIRROR_URL $RELEASE-backports main contrib
EOF
	cat >> $INSTROOT/fstab <<EOF
UUID=$ROOT_FSUUID / ext4 errors=remont-ro 0 1
UUID=$BOOT_FSUUID /boot ext4 defaults 0 2
/dev/zvol/$SYSTEMFS/swap none swap sw,x-systemd.after=zfs.target 0 0
EOF
	cat >> $INSTROOT/crypttab <<EOF
# <luks label> <device> <key> <options>
$LUKS_LABEL UUID=$LUKS_FSUUID none luks
EOF
	for i in dev proc sys
	do
	    mount --rbind /$i $INSTROOT/$i
	done

	chroot $INSTROOT /bin/bash --login
    fi
}

source debinst.defaults

install-deps $RELEASE
init-parts $ROOT_DRIVE
BOOT_PARTUUID=$(partuuid $ROOT_DRIVE 1)
LUKS_PARTUUID=$(partuuid $ROOT_DRIVE 2)
init-boot $BOOT_PARTUUID
init-cryptroot $LUKS_PARTUUID $LUKS_LABEL
init-zfsroot $SYSTEMFS $SWAPSIZE
init-instroot $INSTROOT $LUKS_LABEL $BOOT_PARTUUID $SYSTEMFS

debootstrap $RELEASE $INSTROOT $MIRROR
