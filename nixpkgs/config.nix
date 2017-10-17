{
  allowUnfree = true;
  allowBroken = true;
  useSandbox = true;

  packageOverrides = pkgs: with pkgs; {
    # Minimal set of packages to install everywhere
    minEnv = hiPrio (buildEnv {
      name = "minEnv";
      paths = [
        bash-completion
        bashInteractive
        coreutils
        ctags
        curl
        file
        git
        global
        gnused
        gnutar
        htop
        nox
        shellcheck
        tmux
        tree
        unzip
        vim_configurable
        watch
        wget
        zsh
      ];
    });

    # For "permanent" systems; compatible on both Mac and Linux
    bigEnv = hiPrio (buildEnv {
      name = "bigEnv";
      paths = [
        boost
        checkstyle
        # chromium
        cmake
        docker
        doxygen
        gnumake
        gnupg
        gnutls
        graphviz
        imagemagick
        jdk
        maven
        vagrant
        # virtualbox
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
