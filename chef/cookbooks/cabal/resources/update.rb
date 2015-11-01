#
# Cookbook Name:: cabal
# Resource:: cabal_update
#

actions :update

def initialize(*args)
  super
  @action = :update
end

attribute :user, :kind_of => String, :name_attribute => true
attribute :cache_for, :kind_of => Integer, :default => 60 * 24
