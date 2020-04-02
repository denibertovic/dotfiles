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
ZSH_THEME="fwalch"

# TEMP ALIASES
#
alias met='cd /home/deni/meetup/docker'
alias mtorrents='/home/deni/dotfiles/scripts/mount_torrents.sh'
alias mbackups='/home/deni/dotfiles/scripts/mount_backups.sh'
alias utorrents='umount /media/varys/torrents'
alias ubackups='umount /media/varys/backups'

# kube
alias check-kubectl-version='echo $(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)'
alias fetch-latest-kubectl='curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl'
alias k='kubectl --namespace=${KUBECTL_NAMESPACE:-default}'
alias gci='google-chrome --incognito'

# notifications
alias pause-notifications='killall -SIGUSR1 dunst'
alias resume-notifications='killall -SIGUSR2 dunst'

# Force gpg2
# alias gpg='gpg2'

# Keymaps
# Xmonad map is for switching Caps Lock for Control
alias setkben='setxkbmap en_US && xmodmap ~/.Xmodmap'
alias setkbhr='setxkbmap hr && xmodmap ~/.Xmodmap'

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
alias dcleanvolumes='docker volume ls -q | xargs docker volume rm'
alias dgc='docker run --rm -v /var/run/docker.sock:/var/run/docker.sock -v /etc:/etc spotify/docker-gc'
alias dstoplast='docker ps -l -q | xargs docker stop -t 1'
alias dps='docker ps'
alias di='docker images | awk "{ print \$1}" | tail -n +2 | sort | uniq'

# development related aliases
alias p='python -c "import IPython; IPython.terminal.ipapp.launch_new_instance()"'
alias shell='./manage.py shell'
alias debug='python -m pdb manage.py runserver'
alias idebug='python -m ipdb manage.py runserver --pm'
alias clojure='docker run -e LEIN_ROOT=1 -i -t clojure /bin/bash -c "lein repl"'
# alias ghci=ghci-color

# other aliases
alias sudo='sudo '
alias dk='cd /home/deni/work'
alias ll='ls -l'
alias la='ls -la'
alias noack='ack --ignore-file=ext:json --no-css --no-js --ignore-dir=.terraform --ignore-dir=static --ignore-dir=__data --ignore-dir=migrations --ignore-dir=.stack-work --ignore-dir=.stack --ignore-dir=.ghc --ignore-dir=.ghcjs --ignore-dir=.spago --ignore-file=ext:sql'
alias duh='du -h --max-depth=1'
alias proxy='ssh -D 9999 magrathea.kset.org -p 80'
# alias psc='ps xawf -eo pid,user,cgroup,args'
alias ctl='systemctl'
alias t='/home/deni/.local/bin/todo'
alias t-check-tags='grep -E -o "@.*" ~/Dropbox/todo/todo.txt  | sort | uniq'
alias get-subtitles='subliminal download -l en .'
alias copy-path='pwd | tr -d "\n" | xsel -b'
alias asciicast2gif='docker run -v $PWD:/data asciinema/asciicast2gif'

# tmux
alias tmux='tmux -2'

# jira
alias jira-myissues='/home/deni/.virtualenvs/jira/bin/jira-cli view --search-jql="assignee=dbertovic AND status=Open" | less'
alias jira-do='/home/deni/.virtualenvs/jira/bin/jira-cli update --transition="resolve issue" --resolution="Fixed"'
alias jira-listcomments='/home/deni/.virtualenvs/jira/bin/jira-cli view --comments-only'
alias jira-addcomment='/home/deni/.virtualenvs/jira/bin/jira-cli update --comment'

# eid
alias eid-client='/usr/lib/akd/eidmiddleware/Client'
alias eid-signer='/usr/lib/akd/eidmiddleware/Signer'

# haskell
alias h='hoogle --count=100'

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
plugins=(
    denv
    git
    git-flow
    virtualenvwrapper
    docker
    cabal
    python
    keybase
    stack
    vault
    pass
)

source $ZSH/oh-my-zsh.sh

export PATH="/usr/local/heroku/bin:/home/deni/programs/appengine:/opt/scala/bin:/home/deni/.local/bin:/usr/local/bin/terraform:$PATH"


#export PYTHONPATH="$PYTHONPATH":/usr/lib/python2.7/dist-packages/wx-2.8-gtk2-unicode/
unsetopt correct_all

# added by travis gem
[ -f /home/deni/.travis/travis.sh ] && source /home/deni/.travis/travis.sh

# RPROMPT="\$(cabal_sandbox_info) $RPROMPT"

# VIM mode
bindkey -v

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]% %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1

share-session () {
    if (( $# == 0 ))
    then
        echo Usage: $0 WORKDIR;
    else
        docker run --rm -it \
            -v $1:/opt/workdir \
            -v /home/deni/.vim:/home/user/.vim \
            -v /home/deni/dotfiles/vim/vimrc:/home/user/.vimrc \
            -e LOCAL_USER_ID=$(id -u $USER) denibertovic/tmate tmate
    fi;
}

# I have no idea why I did this
export SSH_AUTH_SOCK=/run/user/$(id -u)/gnupg/S.gpg-agent.ssh

# log last cmd to file
# useful for documenting steps
note () {
    if [[ $# -eq 1  ]]; then
        echo "Logging: `fc -ln -1`"
        echo `fc -ln -1` >> "$HOME/Dropbox/notes/$1-`date +"%Y-%m-%d"`.md" ;
    else
        echo "Logging: `fc -ln -1`"
        echo `fc -ln -1` >> "$HOME/Dropbox/notes/note-`date +"%Y-%m-%d"`.md" ;
    fi
}

# setopt prompt_subst
#
eval "$(denv hook ZSH)"

