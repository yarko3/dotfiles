#
# Cookbook Name:: cabal
# Provider:: cabal
#

def whyrun_supported?
  true
end

class FlagStore
  def initialize
    @flags = []
  end

  def flag(trigger, f)
    @flags << f if trigger
  end

  def to_args
    @flags.join(" ")
  end
end

def run_cabal_update
  cabal_update new_resource.user do
    cache_for new_resource.cache_for
  end
end

def install_cabal_dev
  cabal_install 'cabal-dev' do
    user new_resource.user
  end
end

def install_binary(path)
  from = new_resource.install_binary[:from]
  to = new_resource.install_binary[:to]
  to_user = new_resource.install_binary[:user]
  to_group = new_resource.install_binary[:group]
  to_chown = "#{to_user}#{to_group ? ';' + to_group : ''}"
  execute "cd #{path} && cp #{from} #{to}"
  execute "cd #{path} && chown #{to_chown} #{to}" if to_user
end

def cabal_command(use_cabal_dev = false)
  install_cabal_dev if use_cabal_dev

  store = FlagStore.new
  cmd = use_cabal_dev ? '~/.cabal/bin/cabal-dev' : 'cabal'

  store.flag(new_resource.reinstall, '--reinstall')
  store.flag(new_resource.force_reinstalls, '--force-reinstalls')
  store.flag(new_resource.avoid_reinstalls, '--avoid-reinstalls')
  store.flag(new_resource.upgrade_dependencies, '--upgrade-dependencies')
  store.flag(new_resource.user_install, '--user')
  store.flag(new_resource.global_install, '--global')
  store.flag(new_resource.solver, '--solver')

  "#{cmd} install #{store.to_args}"
end

def install_by_name
  run_cabal_update if new_resource.cabal_update
  execute "su - #{new_resource.user} -c '#{cabal_command} #{new_resource.package_name}'"
end


def install_by_path(path)
  run_cabal_update if new_resource.cabal_update
  execute "su - #{new_resource.user} -c 'cd #{path} && #{cabal_command(new_resource.cabal_dev)}'"
  install_binary(path) if new_resource.install_binary
end

def install_by_git(repository)
  git_tmp="#{Chef::Config[:file_cache_path]}/git"
  name = ::File.basename(repository, '.git') + rand(99999).to_s
  path = "#{git_tmp}/#{name}"

  directory git_tmp do
    mode '0755'
    user new_resource.user
    group new_resource.group
    action :create
  end

  git path do
    repository repository
    reference new_resource.reference
    user new_resource.user
    action :sync
  end

  install_by_path(path)
end

action :install do
  converge_by("cabal install #{ new_resource.name }") do
    if new_resource.github
      install_by_git("https://github.com/#{new_resource.github}.git")
    elsif new_resource.git
      install_by_git(new_resource.git)
    elsif new_resource.path
      install_by_path(new_resource.path)
    else
      install_by_name
    end
  end
end
