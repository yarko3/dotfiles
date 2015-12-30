# This recipe congtains packages that I need for all environments
include_recipe 'apt'

# Development
package 'bison'
package 'build-essential'
package 'clang'
package 'clang-format-3.6'
package 'cmake'
package 'curl'
package 'exuberant-ctags'
package 'flex'
package 'gdb'
package 'graphviz'
package 'pkg-config'
package 'texlive-latex-recommended'
package 'tmux'
package 'vim-gtk'
package 'xclip'
package 'xsel'
package 'zsh'

# Programming Languages
package 'golang'
package 'mit-scheme'
package 'octave'

# Python
package 'ipython'
package 'ipython-qtconsole'
package 'pyflakes'
package 'python'
package 'python-dev'
package 'python-pip'
package 'python-sklearn'
package 'python-software-properties'

# Haskell
package 'cabal-install'
package 'ghc'
package 'ghc-mod'
package 'haskell-platform'
package 'hdevtools'
package 'hlint'
package 'hoogle'
package 'libghc-hunit-dev'
package 'pandoc'
package 'shellcheck'
