if node['platform_family'] == 'debian'
    include_recipe 'brh::debian_minimal'
    include_recipe 'brh::debian_personal'
end

include_recipe 'brh::packages_minimal'
include_recipe 'brh::packages_personal'
