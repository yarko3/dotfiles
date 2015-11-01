include_recipe 'cabal'

cabal_update 'brh'

cabal_install 'hoogle' do
    user 'brh'
end

cabal_install 'pandoc' do
    user 'brh'
    force_reinstalls true
end
