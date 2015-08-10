#!/bin/bash
# Install script for setting up all of my programs on Linux

printHelp()
{
    echo "Ben's Software Manager"
    echo "   Options:"
    echo "   -h  Print this help message and exit"
    echo "   -i  Install new programs"
    echo "   -l  Install programming languages"
    echo "   -m  Install minimum program set"
    echo "   -u  Upgrade"
}

getUpdates()
{
    sudo apt-get -y update
}

getUpgrades()
{
    sudo apt-get -y upgrade
}

installAll()
{
    installMin

    # Development
    sudo apt-get -y install \
                            ack-grep \
                            bison \
                            clang \
                            clang-format-3.6 \
                            curl \
                            exuberant-ctags \
                            flex \
                            gdb \
                            icedtea-netx \
                            icedtea-plugin \
                            npm \
                            pkg-config \
                            python-software-properties \
                            shellcheck \
                            subversion \

    sudo npm -g install instant-markdown-d
    sudo npm -g install glob

    installArcanist

    # Browsers
    sudo apt-get -y install \
                            chromium-browser \
                            flashplugin-installer \
                            google-chrome \

    # Networking
    sudo apt-get -y install \
                            curl \
                            filezilla \
                            nmap \
                            vinagre \
                            wireshark \

    # Media
    sudo apt-get -y install \
                            calibre \
                            gimp \
                            imagemagick \
                            libav-tools \
                            libavcodec-extra \
                            mupdf \
                            totem \
                            vlc \
                            xbmc \

    # Other
    sudo apt-get -y install \
                            deluge \
                            htop \

}

installMin()
{
    sudo apt-get -y install keepass2

    if [ -z "$(which drive)" ]; then
        echo "Installing gdrive"
        wget https://github.com/prasmussen/gdrive -O drive
        sudo mv drive /usr/local/bin/drive
    fi
}

installArcanist()
{
    if ! [[ -f ~/arcanist/arcanist/README ]]; then
        sudo apt-get -y install php5-cli php5-curl php5-json
        mkdir -p ~/arcanist
        cd ~/arcanist
        git clone https://github.com/facebook/libphutil.git
        git clone https://github.com/facebook/arcanist.git
    fi
}

installRuby()
{
    curl -sSL https://rvm.io/mpapis.asc | gpg --import -
    curl -sSL https://get.rvm.io | bash -s stable --ruby
}

installGems()
{
    sudo apt-get install -y bundler
    sudo gem install bundler
    bundle install
}

# This one takes a very long time to complete
installHaskell()
{
    sudo apt-get -y install \
                            cabal-install \
                            ghc \
                            ghc-mod \

    cabal update
    cabal install parsec
    cabal install happy
    cabal install hoogle
}

installLanguages()
{
    installHaskell

    sudo apt-get -y install \
                            golang \
                            mit-scheme \
                            octave \
                            python \
                            python-dev \
                            python-pip \

    sudo pip install "ipython[all]"

    #installRuby
    checkYCM
}

# Before enabling this, upgrade the llvm_clang
# script to be idempotent (or replace with Chef)
installClang()
{
    ~/bin/llvm_clang_install.sh
    sudo ln -s /usr/local/bin/clang++ clang++
    sudo ln -s /usr/local/bin/clang clang
}

checkYCM()
{
    if [ -f ~/.vim/bundle/YouCompleteMe/third_party/ycmd/ycm_core.so ]; then
        echo "YCM already built."
    else
        ~/bin/ycm_home_install.sh
    fi
}

## ============================================================================
##                                  OptArgs
## ============================================================================
if [ -z "$1" ]; then printHelp; fi

while getopts "hilmru" opt; do
    case $opt in
        h)
            printHelp
            ;;
        i)
            echo "Installing programs" >&2
            getUpdates
            installAll
            ;;
        l)
            echo "Installing Languages" >&2
            getUpdates
            installLanguages
            ;;
        m)
            echo "Installing minimum program set" >&2
            getUpdates
            installMin
            ;;
        r)
            echo "Installing Ruby" >&2
            installRuby
            installGems
            ;;
        u)
            echo "Upgrading"
            getUpdates
            getUpgrades
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
