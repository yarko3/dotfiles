include_recipe 'cabal'

cabal_update 'brh'

packages = [
        "hoogle",
        "pandoc",
        "hunit",
        "ghc-mod",
        "pointfree",
        "hdevtools"
]

for p in packages
    cabal_install p do
        user 'brh'
        force_reinstalls true
    end
end
