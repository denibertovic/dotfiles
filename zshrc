# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#
# THE BEST THEME so far is fwalch
# gallifrey is good
#
ZSH_THEME="gallifrey"


# docker related aliases
#
alias c='docker-compose'
alias m='docker-machine'
# Delete all stoped/exited containers except data containers which will have the naming scheme
# someNameData or someNameDataContainer - We have to use camel case because docker-compose.yml doesn't allow for
# dashes or underscores
# TODO: figure out how to use labels for this (need inverted query)
alias dclean='docker ps -a -f status=exited | grep -vi "datacontainer" | tail -n +2 | cut -c1-12 | xargs --no-run-if-empty docker rm -v'
# remove all images tagged as <none>
alias dcleanimages='docker images -q -f dangling=true | xargs --no-run-if-empty docker rmi'
alias dstoplast='docker ps -l -q | xargs docker stop -t 1'
alias dps='docker ps'
alias di="docker images | awk '{ print \$1}' | tail -n +2 | sort | uniq"

# development related aliases
alias p="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"
alias shell='./manage.py shell'
alias debug='python -m pdb manage.py runserver'
alias idebug='python -m ipdb manage.py runserver --pm'
alias clojure='docker run -e LEIN_ROOT=1 -i -t clojure /bin/bash -c "lein repl"'
alias ghci=ghci-color

# other aliases
alias sudo='sudo '
alias dk='cd /home/deni/work/datarobot'
alias ll='ls -l'
alias la='ls -la'
alias noack='ack-grep --no-css --no-js --ignore-dir=static --ignore-dir=__data --ignore-dir=migrations'
alias ack='ack-grep'
alias duh='du -h --max-depth=1'
alias proxy='ssh -D 9999 magrathea.kset.org -p 80'
alias psc='ps xawf -eo pid,user,cgroup,args'
alias ctl='systemctl'
alias t='/usr/local/bin/todo.sh'

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"
#
WORKON_HOME=/home/deni/.virtualenvs

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git virtualenvwrapper docker cabal python keybase)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
#export PATH=/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/NX/bin:/opt/android-sdk-linux_86/tools:/opt/scala/bin:/opt/eclipse:/usr/local/bin/p4/bin

### Added by the Heroku Toolbelt
export PATH="$HOME/.cabal-sandbox/bin:$HOME/.ghc/bin:$HOME/.cabal/bin::/usr/local/heroku/bin:/home/deni/programs/appengine:/opt/scala/bin:$PATH"


#export PYTHONPATH="$PYTHONPATH":/usr/lib/python2.7/dist-packages/wx-2.8-gtk2-unicode/
unsetopt correct_all

# added by travis gem
[ -f /home/deni/.travis/travis.sh ] && source /home/deni/.travis/travis.sh

# added by Nix Installer
[ -e /home/deni/.nix-profile/etc/profile.d/nix.sh  ] && source /home/deni/.nix-profile/etc/profile.d/nix.sh;

RPROMPT="\$(cabal_sandbox_info) $RPROMPT"

