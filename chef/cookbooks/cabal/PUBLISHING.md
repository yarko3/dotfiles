
## To upload new versions.

 * Update version metadata.rb
 * Add notes to CHANGELOG.md
 * Run:

    knife cookbook site share cabal "Package Management" -u <user> -k <api pem>
