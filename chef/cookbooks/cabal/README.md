cabal Cookbook
==============

A cookbook that provides a lwrp for interacting with
[cabal](http://www.haskell.org/haskellwiki/Cabal-Install), a
dependency managent tool for haskell.

Requirements
------------

### Chef

No specific version requirements. There are no external
dependencies. Tested on 11.4.0.

### Platform

The following platforms have been tested with this cookbook, meaning that the
recipes run on these platforms without error:

 * FreeBSD
 * Arch Linux
 * Ubuntu


Installation
------------

There are several ways you can install this cookbook.

### <a name="installation-platform"></a> From the Opscode Community Platform

To install this cookbook from the Opscode platform, use the *knife* command:

    knife cookbook site install cabal

### <a name="installation-librarian"></a> Using Librarian-Chef

[Librarian-Chef](https://github.com/applicationsonline/librarian-chef)
is a bundler for your Chef cookbooks.  Include a reference to the
cookbook in a `Cheffile` and run `librarian-chef install`.

To reference the latest published version:

    cookbook 'cabal', '>= 0.1.0'

To reference the github version:

    cookbook 'cookbook'
      :github => 'apiengine/chef-cabal'

Then run:

    librarian-chef install

Resources
---------


### cabal_install

Install a package from hackage, a path, a git repo or a github repo.

#### Attributes

 * `package_name` - This is the name of the package to install from hackage. Ignored if `:path`, `:git` or `:github` are set. This is the name attribute.
 * `path` - Specify path to a cabal project to install from file system. Ignored if `:git` or `:github` are set.
 * `git` - Specify git repository containing cabal project to clone and install. Ignored if `:github` is set.
 * `github` - Specify github repository containing cabal project to clone and install. This is shorthand for `git "https://github.com/#{github}.git`
 * `reference` - Specify git reference to checkout if using `:git` or `:github`. See git lwrp for more details.
 * `only_dependencies` - Pass `--only-dependencies` flag to cabal. Off by default.
 * `reinstall` - Pass `--reinstall` flag to cabal. Off by default.
 * `force_reinstalls` - Pass `--force-reinstalls` flag to cabal. Off by default.
 * `avoid_reinstalls` - Pass `--avoid-reinstalls` flag to cabal. Off by default.
 * `upgrade_dependencies` - Pass `--upgrade-dependencies` flag to cabal. Off by default.
 * `user_install` - Pass `--user` flag to cabal. Off by default.
 * `global_install` - Pass `--global` flag to cabal. Off by default.
 * `solver` - Pass `--solver #{solver}` flag to cabal. Off by default.
 * `user` - User to run cabal and git checkouts as.
 * `group` - Group to run git checkouts as.
 * `cabal_dev` - Use cabal-dev to the package in isolation. Only used if `:path`, `:git` or `:github` are used. Most useful in conjunction with `:install_binary` to install the isolated binary to some system location. Default is false.
 * `install_binary` - Upon completion of cabal install, copy file to target. The hash must contain :from and :to attributes. The hash can contain :user and :group for setting owner after copy. Paths are relative to the root of the cabal project.
 * `cabal_update` - Run cabal update before install.
 * `cache_for` - See cabal_update resource. This is minutes to cache current cabal index for.

</table>

#### Examples

Run cabal install the yesod package for fred.

    cabal_install 'yesod' do
      user 'fred'
    end


Run cabal-dev install the angel binary from github and install to /usr/bin.

    cabal_install 'angel' do
      github 'jamwt/Angel'
      reference '3ee0a190b354f143273dcf2fe9bfa5b730dc5fe5'
      install_binary :from => 'angel/dest/build/angel/angel', :to => '/usr/bin/angel'
      cabal_dev true
      user username
      group username
    end

### cabal_update

Run cabal update to get latest package index from hackage.

#### Attributes

 * `user` - Specify the user to run cabal update as. This is the name attribute.
 * `cache_for` - The time (in minutes) that must have elapsed since last cabal update. If the cabal index has been updated in the last `cache_for` minutes, it will not be updated again. Default is 24 hours.

#### Examples

Run cabal update for fred (if it has not been run in last 24 hours)

    cabal_update 'fred'


Run cabal update for barney if it has not been run in last 30 minutes.

    cabal_update 'barney' do
      cache_for 30
    end

Usage
-----

To get access to the cabal resource, include the default cabal recipe.

    include_recipe 'cabal'



License and Author
------------------

Author:: Mark Hibberd <mark@hibberd.id.au>

Copyright:: 2013, Mark Hibberd

All code is copyright 2013 Mark Hibberd <mark@hibberd.id.au>

All code is licensed under a 3-point BSD style license.

See LICENSE or https://github.com/apiengine/chef-cabal/blob/master/LICENSE.
