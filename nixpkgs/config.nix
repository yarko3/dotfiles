{
  allowUnfree = true;
  allowBroken = true;
  useSandbox = true;

  packageOverrides = pkgs: with pkgs; {
    # dev packages to install everywhere
    devEnv = hiPrio (buildEnv {
      name = "devEnv";
      paths = [
        # chromium
        # virtualbox
        bash-completion
        bashInteractive
        boost
        checkstyle
        cmake
        coreutils
        ctags
        docker
        doxygen
        file
        gcc
        git
        global
        gnumake
        gnupg
        gnused
        gnutar
        gnutls
        graphviz
        htop
        imagemagick
        jdk
        maven
        nox
        shellcheck
        tmux
        tree
        unzip
        vagrant
        vim_configurable
        watch
        wget
        xclip
        xsel
        zlib
      ];
    });

    # For "permanent" systems; these packages don't seem to play well with MacOS
    bigEnvLinux = hiPrio (buildEnv {
      name = "bigEnvLinux";
      paths = [
        calibre
        vlc
      ];
    });

    pyEnv = hiPrio (python27.withPackages (ps: with ps; [
        flake8
        futures
        isort
        numpy
        paramiko
        pep8
        pylint
        setuptools
        toolz
        yamllint
    ]));
  };
}
