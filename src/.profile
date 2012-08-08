# git autocomplete for branch names, etc
source ~/dotfiles/git/git-completion.bash

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

export MY_BUNDLER_ENV='dev'
export EDITOR=mate

alias brake="bundle exec rake"
alias be="bundle exec"
alias bc="bundle exec rails c"
