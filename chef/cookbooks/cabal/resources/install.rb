#
# Cookbook Name:: cabal
# Recipe:: cabal
#

actions :install

def initialize(*args)
  super
  @action = :install
end

attribute :package_name, :kind_of => String, :name_attribute => true
attribute :path, :kind_of => String, :default => nil
attribute :git, :kind_of => String, :default => nil
attribute :github, :kind_of => String, :default => nil
attribute :reference, :kind_of => String, :default => nil
attribute :only_dependencies, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :reinstall, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :force_reinstalls, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :avoid_reinstalls, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :upgrade_dependencies, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :user_install, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :global_install, :kind_of => [TrueClass, FalseClass], :default => nil
attribute :solver, :kind_of => String, :default => nil
attribute :user, :kind_of => String, :default => nil
attribute :group, :kind_of => String, :default => nil
attribute :cabal_dev, :kind_of => [TrueClass, FalseClass], :default => false
attribute :cabal_update, :kind_of => [TrueClass, FalseClass], :default => false
attribute :cache_for, :kind_of => Integer, :default => 60 * 24
attribute :install_binary, :kind_of => Hash
