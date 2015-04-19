#!/bin/bash
# Install script for setting up all of my programs on Linux

printHelp()
{
    echo "Ben's Software Manager"
    echo "   Options:"
    echo "   -h  Print Help"
    echo "   -u  Upgrade"
    echo "   -i  Install new programs"
    echo "   -m  Install minimum program set"
}

update()
{
    sudo apt-get -y update
}

upgrade()
{
    sudo apt-get -y upgrade
}

installAll()
{
    installMin

    # Development
    sudo apt-get -y install ack-grep
    sudo apt-get -y install exuberant-ctags
    sudo apt-get -y install python-dev
    sudo apt-get -y build-dep python3.4
    sudo apt-get -y install icedtea-netx
    sudo apt-get -y install icedtea-plugin
    sudo apt-get -y install subversion
    sudo apt-get -y install python-software-properties pkg-config
    sudo apt-get -y install octave
    sudo apt-get -y install shellcheck

    installArcanist

    # Browsers
    sudo apt-get -y install chromium-browser
    sudo apt-get -y install google-chrome
    sudo apt-get -y install flashplugin-installer

    # Networking
    sudo apt-get -y install nmap
    sudo apt-get -y install vinagre
    sudo apt-get -y install wireshark
    sudo apt-get -y install filezilla
    sudo apt-get -y install curl

    # Media
    sudo apt-get -y install libavcodec-extra
    sudo apt-get -y install vlc
    sudo apt-get -y install totem
    sudo apt-get -y install xbmc
    sudo apt-get -y install libav-tools
    sudo apt-get -y install mupdf
    sudo apt-get -y install gimp
    sudo apt-get -y install imagemagick

    # Other
    sudo apt-get -y install deluge
    sudo apt-get -y install htop
}

installMin()
{
    sudo apt-get -y install vim-gtk
    sudo apt-get -y install awesome
    sudo apt-get -y install build-essential cmake
    sudo apt-get -y install tmux
    sudo apt-get -y install keepass2

    if ! [[ -f ~/.gitconfig ]]; then
        cp ~/.gitconfig.notes ~/.gitconfig
    fi

    if ! [[ -f ~/.vim/bundle/Vundle.vim/README.md ]]; then
        git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    fi

    if ! [[ -f ~/.config/awesome/README.md ]]; then
        git clone https://github.com/brhCS/awesome_wm ~/.config/awesome
    fi
}

installArcanist()
{
    if ! [[ -f ~/arcanist/arcanist/README ]]; then
        sudo apt-get -y install php5-cli php5-curl php5-json
        mkdir ~/arcanist
        cd ~/arcanist
        git clone https://github.com/facebook/libphutil.git
        git clone https://github.com/facebook/arcanist.git
    fi
}


## ============================================================================
##                                  OptArgs
## ============================================================================
if [ -z "$1" ]; then printHelp; fi

while getopts "huim" opt; do
    case $opt in
        h)
            printHelp
            ;;
        u)
            echo "Upgrading"
            update
            upgrade
            ;;
        i)
            echo "Installing programs" >&2
            update
            installAll
            ;;
        m)
            echo "Installing minimum program set" >&2
            update
            installMin
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done
