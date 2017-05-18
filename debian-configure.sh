#!/bin/bash

STATEFILE=.debian-state
RELEASE=jessie

function apt-init {
    cat >> /etc/apt/apt.conf <<EOF
APT::Get::Install-Recommends "false";
APT::Get::Install-Suggests "false";
EOF
    echo "APT_INIT=1" >> $STATEFILE
}

function sources-backports {
    if [ ! -f /etc/apt/sources.list.d/backports.list ]
    then
	echo "deb http://ftp.uk.debian.org/debian $RELEASE-backports main contrib" > /etc/apt/sources.list.d/backports.list
	apt update
    fi
}

function sources-docker {
    if [ ! -f /etc/apt/sources.list.d/docker.list ]
    then
	apt install -y apt-transport-https
	echo "deb https://apt.dockerproject.org/repo debian-$RELEASE main" > /etc/apt/sources.list.d/docker.list
	wget -O - "https://apt.dockerproject.org/gpg" | apt-key add -
	apt update
    fi
}

function system-upgrade {
    echo
    read -p "Upgrade the system? [y/N]" system_upgrade
    case $system_upgrade in
	[yY])
	    echo "Upgrading Debian system..."
	    apt update
	    apt upgrade -y
	    echo "Finished upgrading Debian system!"
	    echo 'SYSTEM_UPGRADE=1' >> $STATEFILE
	    ;;
	*)
	    echo "Skipping upgrading Debian system"
	    ;;
    esac
}

function system-reboot {
    echo
    read -p "Reboot now? [Y/n]" system_reboot
    case $system_reboot in
	[nN])
	    echo "Skipping reboot. Exiting!"
	    exit 0
	    ;;
	*)
	    echo "Rebooting..."
	    reboot
	    ;;
    esac
}

function system-cleanup {
    echo
    read -p "Clean up apt repository and unneeded packages? [Y/n]" cleanup
    case $cleanup in
	[nN])
	    echo "Skipped cleaning up"
	    ;;
	*)
	    apt-get autoremove -y
	    apt-get autoclean -y
	    ;;
    esac
}

function install-grsec {
    echo
    read -p "Install grsecurity kernel patches? [y/N]" grsec
    case $grsec in
	[yY])
	    echo "Installing grsecurity kernel patches..."
	    sources-backports
	    apt install -y -t $RELEASE-backports linux-image-grsec-amd64
	    echo "Finished installing grsecurity patched kernel!"
	    echo 'INSTALL_GRSEC=1' >> $STATEFILE
	    system-reboot
	    ;;
	*)
	    echo "Skipping grsecurity kernel patches"
	    echo 'INSTALL_GRSEC=2' >> $STATEFILE
	    ;;
    esac
}

function install-samba {
    echo
    read -p "Install NFS / Samba packages? [y/N]" install_samba
    case $install_samba in
	[yY])
	    echo "Installing NFS / Samba packages..."
	    apt install -y nfs-kernel-server samba
	    echo "Finished installing NFS / Samba packages"
	    echo 'INSTALL_SAMBA=1' >> $STATEFILE
	    ;;
	*)
	    echo "Skipping NFS / Samba packages"
	    echo 'INSTALL_SAMBA=2' >> $STATEFILE
	    ;;
    esac
}

function install-zfs {
    read -p "Install ZFS tools & kernel modules? [y/N]" zfs
    case $zfs in
	[yY])
	    echo "Installing ZFS tools & kernel modules..."
	    sources-backports
	    apt install -y -t $RELEASE-backports linux-headers-$(uname -r)
	    apt install -y -t $RELEASE-backports zfs-dkms zfs-initramfs
	    echo "Finished installing ZFS tools & kernel modules!"
	    echo 'INSTALL_ZFS=1' >> $STATEFILE
	    system-reboot
	    ;;
	*)
	    echo "Skipping ZFS tools & kernel modules"
	    echo 'INSTALL_ZFS=2' >> $STATEFILE
	    ;;
    esac
}

function install-kvm {
    echo
    read -p "Install KVM? [y/N]" kvm
    case $kvm in
	[yY])
	    echo "Installing KVM..."
	    apt install -y qemu-kvm libvirt-bin virtinst
	    echo "Finished installing KVM!"
	    echo 'INSTALL_KVM=1' >> $STATEFILE
	    echo "Check that virtualization support is enabled in BIOS!"
	    ;;
	*)
	    echo "Skipping KVM packages"
	    echo 'INSTALL_KVM=2' >> $STATEFILE
	    ;;
    esac
}

function install-docker {
    echo
    read -p "Install Docker? [y/N]" docker
    case $docker in
	[yY])
	    echo "Installing Docker..."
	    sources-docker
	    apt install -y docker-engine
	    echo "Finished installing Docker!"
	    echo 'INSTALL_DOCKER=1' >> $STATEFILE
	    ;;
	*)
	    echo "Skipping Docker Engine"
	    echo 'INSTALL_DOCKER=2' >> $STATEFILE
	    ;;
    esac
}

function install-extra {
    echo
    read -p "Install extra packages? [y/N]" extra
    case $extra in
	[yY])
	    echo "Installing extra packages..."
	    apt install -y wget tar xz-utils info
	    apt install -y gdisk cryptsetup
	    echo "Finished installing extra packages!"
	    echo 'INSTALL_EXTRA=1' >> $STATEFILE
	    ;;
	*)
	    echo "Skipping extra packages"
	    echo 'INSTALL_EXTRA=2' >> $STATEFILE
	    ;;
    esac
}

function usage {
    cat <<EOF
Initialize Debian system.

USAGE:

$0 [-N]

With option -N, ask to redo previously ignored steps too.
EOF
}

LEVEL=1

while getopts "N" opt
do
    case $opt in
	N)
	    LEVEL=3
	    ;;
	?)
	    echo "INVALID ARGUMENT: $OPTARG" >&2
	    exit -1
	    ;;
	*)
	    usage
	    exit -1
	    ;;
    esac
done

if [ -f $STATEFILE ]
then
    source $STATEFILE
else
    echo '# Variable flags for debian-init state' > $STATEFILE
fi

if [[ $APT_INIT -lt $LEVEL ]]
then
    apt-init
fi

if [[ $SYSTEM_UPGRADE -lt $LEVEL ]]
then
    system-upgrade
fi

if [[ $INSTALL_GRSEC -lt $LEVEL ]]
then
    install-grsec
fi

if [[ $INSTALL_ZFS -lt $LEVEL ]]
then
    install-zfs
fi

if [[ $INSTALL_SAMBA -lt $LEVEL ]]
then
    install-samba
fi

if [[ $INSTALL_KVM -lt $LEVEL ]]
then
    install-kvm
fi

if [[ $INSTALL_DOCKER -lt $LEVEL ]]
then
    install-docker
fi

if [[ $INSTALL_EXTRA -lt $LEVEL ]]
then
    install-extra
fi

system-cleanup
system-reboot
