# This recipe congtains packages that I need for all environments
include_recipe 'apt'

# Development Tools
package 'libtool'
package 'autoconf'
package 'build-essential'
package 'clang'
package 'clang-format-3.6'
package 'cmake'
package 'curl'
package 'exuberant-ctags'
package 'gdb'
package 'par'
package 'pkg-config'
package 'tmux'
package 'vim-gtk'
package 'xclip'
package 'xsel'
package 'zsh'

# Python
package 'pyflakes'
package 'python'
package 'python-dev'
package 'python-software-properties'

# Haskell
package 'cabal-install'
package 'ghc'
package 'ghc-mod'
package 'hdevtools'
package 'hlint'
package 'libghc-hunit-dev'
package 'shellcheck'
