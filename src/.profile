# git autocomplete for branch names, etc
source ~/dotfiles/src/git/git-completion.bash

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

export MY_BUNDLER_ENV='dev'
export EDITOR=mate
export JRUBY_OPTS='--1.9'

alias brake="bundle exec rake"
alias be="bundle exec"
alias bc="bundle exec rails c"
alias emacs="emacs -nw -Q"
