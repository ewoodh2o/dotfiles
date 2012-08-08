# git autocomplete for branch names, etc
source /usr/local/git/contrib/completion/git-completion.bash

# MacPorts Installer addition on 2009-09-29_at_15:51:19: adding an appropriate PATH variable for use with MacPorts.
# export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.
# export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

alias apachestart="sudo /opt/local/apache2/bin/apachectl start"
alias apachestop="sudo /opt/local/apache2/bin/apachectl stop"
alias apacherestart="sudo /opt/local/apache2/bin/apachectl graceful"
alias passengerstart="passenger start -a 127.0.0.1 -d -p"

export MY_BUNDLER_ENV='dev'
export EDITOR=mate

alias brake="bundle exec rake"
alias be="bundle exec"
