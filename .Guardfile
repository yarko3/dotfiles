# A sample Guardfile
# More info at https://github.com/guard/guard#readme

## Uncomment and set this to only include directories you want to watch
# directories %w(app lib config test spec features)

## Uncomment to clear the screen before every task
clearing :on

## Guard internally checks for changes in the Guardfile and exits.
## If you want Guard to automatically start up again, run guard in a
## shell loop, e.g.:
##
##  $ while bundle exec guard; do echo "Restarting Guard..."; done
##
## Note: if you are using the `directories` clause above and you are not
## watching the project directory ('.'), then you will want to move
## the Guardfile to a watched dir and symlink it back, e.g.
#
#  $ mkdir config
#  $ mv Guardfile config/
#  $ ln -s config/Guardfile .
#
# and, you'll have to watch "config/Guardfile" instead of "Guardfile"


# Add files and commands to this file, like the example:
#   watch(%r{file/path}) { `command(s)` }
#

guard :shell do
    watch(%r{^.+\.(cpp|h)$}) do
        `gmake build`
    end

    watch(%r{^.+\.(out|tsk)}) do
        `gmake test`
    end
end

# This is an example with all options that you can specify for guard-process
#guard 'process', :name => 'brh-guard', :command => 'clang++ -std=c++11 knapsack.cpp', :env => {}, :stop_signal => "KILL"  do
#watch('Gemfile.lock')
#watch('.+\.cpp')
#end
